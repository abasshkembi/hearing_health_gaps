# Filename: 01A_data_preprocessing.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: December 18 2023

# Description: Cleaning participant demographics, survey responses, and environmental
# ------------ noise levels for Cohort One

# Data/code pipeline: 
# --------- 1. Two main data files (uploaded by Xin to VDI under `hds:// processed / Hearing Scripts / xinzi / lancet /`):
# ------------ a. `environment_daily_averaged.RData` (participant-level averages, 24-hr environmental noise levels)
# ------------ b. `lancet_profile.csv` (participant demographics and survey responses)
# --------------- i. Description of files under `Lancet-Paper.html`
# --------- 2. Extracted county ACS data uploaded by Abas to VDI under `hds:// code / DataImports / ashkembi / Lancet2023 /`
# ------------ a. `acs2019_county_ageSexRace.RData` (2017-2021 ACS age, sex, race data by county)

# Study population: Cohort One (Nov 14, 2019 to Nov 14, 2022)






# -------------------- 
# 1. Read in raw data files and set up environment
# --------------------


# load in libraries
library(tidyverse)

# set my local working directory
setwd("/home/hdsuser/Papers/Lancet 2023 Analysis/")

# canonical participant level, 24-hr environmental noise levels averaged over study period
load("Data/01_imported/environment_daily_averaged.RData")
dim(environment_daily_average)
# [1] 186929      3
colnames(environment_daily_average)
# [1] "canonical_subject_id" "duration_hours"       "lex_24"  


#  participant characteristics (demographics, survey responses)
demo_lancet <- read.csv("Data//01_imported/lancet_profile.csv") %>% as_tibble() 
dim(demo_lancet)
# [1] 186929     27
colnames(demo_lancet)
# [1] "canonical_subject_id"          "subject_id"                    "birth_year"                   
# [4] "state"                         "country"                       "state_matched"                
# [7] "country_mathced"               "zip_code"                      "zipcode_type"                 
# [10] "county"                        "county_num"                    "home_environment_population_q"
# [13] "hearing_ability_rate_q"        "hearing_ability_audiogram_q"   "hearing_loss_diagnosed_q"     
# [16] "hearing_aid_wear_q"            "hearing_aid_dontuse_q"         "assigned_sex"                 
# [19] "education"                     "socioeconomic_status"          "ethnicity"                    
# [22] "sub_ethnicity"                 "task_start_time_baseline"      "tbl1_miss"                    
# [25] "tbl2_miss"                     "tbl3_miss"                     "total_miss" 


# 2017-2021 ACS data by county broken down by age, sex, and race
load("Data/01_imported/acs2019_county_ageSexRace.RData") 
dim(count_by_county)
# [1] 322100      7
colnames(count_by_county)
# "GEOID" "NAME"  "sex"   "age"   "total" "race"  "count"






# -------------------- 
# 2. Clean zipcodes and counties
# --------------------


# remove all people with "0" as zipcode - we don't know these people's zip code
demo_lancet2 <- demo_lancet %>%
  filter(zip_code != 0)
nrow(demo_lancet2)
# [1] 165272
### 165,272 people remain
nrow(demo_lancet2) - nrow(demo_lancet)
# [1] -21657
### removed 21,657 people


# remove all people with `NA` as their county
demo_lancet3 <- demo_lancet2 %>%
  filter(!is.na(county))
nrow(demo_lancet3)
# [1] 165244
### 165,244 people remain
nrow(demo_lancet3) - nrow(demo_lancet2)
# [1] -28
### removed 28 people


# split data into two:
# -------------------- 1. people with one county matched to their zip code
# -------------------- 2. people with more than one county matched to their zip code

demo_lancet3_county <- demo_lancet3 %>% filter(county_num == 1)
demo_lancet3_county2 <- demo_lancet3 %>% filter(county_num > 1)

nrow(demo_lancet3_county)
# [1] 145437
nrow(demo_lancet3_county2)
# [1] 19807

# the sum of rows in split up dataset should equal the rows in the original, pre-split dataset
(nrow(demo_lancet3_county) + nrow(demo_lancet3_county2)) == nrow(demo_lancet3)
# [1] TRUE

# check if there is anyone with multiple states assigned to their possible counties
demo_lancet3_county2 %>% filter(str_detect(state_matched, ","))
# there are 40 people, all who originally said they are in CT but their zip code has
# been assigned to 3 counties across CT, NY:
### Windham County (CT)
### New London County (CT)
### Suffolk County (NY)

# since these people said they live in CT in their baseline survey, remove the NY option
demo_lancet3_county2 <- demo_lancet3_county2 %>%
  mutate(county = ifelse(
    str_detect(state_matched, ","), # get the 40 people
    str_remove(county, "Suffolk County, "), # remove Suffolk County
    county # else: county
  )) %>%
  mutate(county_num = ifelse(
    str_detect(state_matched, ","), # get the 40 people
    county_num - 1, # assign 2 counties 
    county_num # else: county
  )) %>%
  mutate(state_matched = ifelse(
    str_detect(state_matched, ","), # get the 40 people
    state, # assign CT
    state_matched # else: leave it as is
  ))



# 1. randomly assign people with more than one county to a single county
# 2. save the random county assignment

set.seed(123); comma_df <- demo_lancet3_county2 %>%
  rowwise() %>% # makes future operations run on each row, not on the entire dataset
  mutate(random_county_n = sample(1:county_num, n(), replace = TRUE)) %>% # assign a random integer reflecting the beginning of a comma (n)
  mutate(random_county_m = random_county_n + 1) %>% # the mth comma position (n + 1)
  mutate(county_comma = paste0(", ", county, ","))  # add a comma to the beginning and end of each county string

comma_df2 <- comma_df %>%
  # find the index of the nth comma
  mutate(n_comma = as.numeric(unlist(str_locate_all(county_comma, ",") %>% map(~ .x[,1][random_county_n])))) %>%
  # find the index of the mth comma
  mutate(m_comma = as.numeric(unlist(str_locate_all(county_comma, ",") %>% map(~ .x[,1][random_county_m])))) %>%
  # take the county located between the nth and mth comma
  mutate(new_random_county = str_sub(string = county_comma, 
                                     start = n_comma+2, #remove comma and space
                                     end = m_comma-1)) # remove comma

# visual check to make sure it is working
comma_df2 %>% 
  select(county, state, state_matched, county_num, random_county_n, random_county_m, new_random_county)
# working good

# check to see if sampling of county is out of bounds
comma_df2 %>% filter(random_county_n > county_num) %>% nrow()
# [1] 0
# working good

# the number of rows in the newly assigned county dataset should equal the original dataset
nrow(comma_df2) == nrow(demo_lancet3_county2)
# [1] TRUE

# everything is looking good so far
### save randomly selected county dataset for future comparison
### then merge in newly assigned county into original, large dataset

# save .RData file
save(comma_df2, file = "Data/02_generated/random_county_assignment.RData")

# remove unnecessary columns
demo_lancet3_county2_random <- comma_df2 %>% select(-c(random_county_n:m_comma))

demo_lancet4 <- demo_lancet3_county %>% 
  mutate(new_random_county = county) %>% # create a new column of county in the tibble with 1 county 
  rbind(demo_lancet3_county2_random) # row bind the two dataframes

nrow(demo_lancet4)
#[1] 165244

nrow(demo_lancet4) == nrow(demo_lancet3)
#[1] TRUE
## no one has been accidentally dropped


# check if anyone has a state_matched that has two states - should be 0
demo_lancet4 %>% filter(str_detect(state_matched, ",")) %>% nrow()
# [1] 0

# note that there are still people whose state is not the state_matched according to the zip_code
demo_lancet4 %>% filter(state != state_matched)
# 3,534 people have this issue
### going foward, we will assume that their real state is state_matched for analysis purposes









# -------------------- 
# 3. Clean demographics
# --------------------


# limit to people in the US and PR for this analysis
demo_lancet5 <- demo_lancet4 %>% filter(country %in% c("US", "PR"))
nrow(demo_lancet5)
# [1] 165119
nrow(demo_lancet5) - nrow(demo_lancet4)
# [1] -125
### removed 125 people

# remove county_matched outside of US or PR
# these are all military people
demo_lancet5 <- demo_lancet5 %>% filter(country_mathced %in% c("US", "PR"))
nrow(demo_lancet5)
# [1] 165065
165065 - 165119
### removed 54 people

# which state abb are not in the normal list of 50 states
unique(demo_lancet5$state_matched)[which(!(unique(demo_lancet5$state_matched) %in% state.abb))]
#"DC" "PR" "AE" "AP" "GU" "VI" "AS" "MP" NA  
### keep only DC and PR for this analysis

# create vector of necessary state abb, appending DC and PR
ahs_state_abb <- c(state.abb, "DC", "PR")
# create vector of necessary state names, appending DC and PR
ahs_state_names <- c(state.name, "District of Columbia", "Puerto Rico")

# filter for these 52 states
demo_lancet5 <- demo_lancet5 %>% filter(state_matched %in% ahs_state_abb)
nrow(demo_lancet5)
# [1] 165014
165014 - 165065
# removed 51 people

# filter for people with any empty new_random_county
demo_lancet5 %>% filter(new_random_county == "") %>% nrow()
# [1] 12
### there are 12 of these people

# remove these people
demo_lancet5 <- demo_lancet5 %>% filter(new_random_county != "")
nrow(demo_lancet5)
# [1] 165002
165002 - 165014
# removed 12 people


ahs_counties <- demo_lancet5 %>% 
  count(new_random_county, state_matched)  %>%
  ungroup()

ahs_county_cross <- ahs_counties %>%
  left_join(
    tibble(
      state_matched = ahs_state_abb,
      state_name = ahs_state_names
    ),
    by = "state_matched"
  ) %>%
  mutate(COUNTY_NAME = paste0(new_random_county, ", ", state_name)) %>%
  select(-n)

# an inner and left join should result in the same outcome if all the county names are correct in the AHS data
nrow(demo_lancet5 %>% left_join(ahs_county_cross, by = c("new_random_county", "state_matched"))) == nrow(demo_lancet5 %>% inner_join(ahs_county_cross, by = c("new_random_county", "state_matched")))
# [1] TRUE

# do an inner join
demo_lancet5 <- demo_lancet5 %>% inner_join(ahs_county_cross, by = c("new_random_county", "state_matched"))
dim(demo_lancet5)
# [1] 165002     30


# get county name, GEOID crosswalk
county_cross <- count_by_county %>%
  select(GEOID, NAME) %>% 
  distinct()

# remove all accents in the words
county_cross2 <- county_cross %>%
  mutate(COUNTY_NAME = stringi::stri_trans_general(str = NAME, 
                                          id = "Latin-ASCII"))

# there are some counties that just do not crosswalk well for whatever reason
# i've made a cleaned crosswalk for them here
erroneous_counties_cross <- tibble(
  COUNTY_NAME = c("Toa Alta, Puerto Rico", "Dekalb County, Georgia", "City and Borough of Juneau, Alaska",
                  "Mclennan County, Texas", "Dupage County, Illinois", "Municipality of Anchorage, Alaska",
                  "St Louis County, Minnesota", "Mclean County, Illinois", "Mccracken County, Kentucky",
                  "Mckinley County, New Mexico", "St Clair County, Alabama"),
  CLEAN_COUNTY_NAME = c("Toa Alta Municipio, Puerto Rico", "DeKalb County, Georgia", "Juneau City and Borough, Alaska",
                        "McLennan County, Texas", "DuPage County, Illinois", "Anchorage Municipality, Alaska",
                        "St. Louis County, Minnesota", "McLean County, Illinois", "McCracken County, Kentucky",
                        "McKinley County, New Mexico", "St. Clair County, Alabama")
)

# assign GEOID
demo_lancet5 <- demo_lancet5 %>%
  # fix the erroneous counties
  left_join(erroneous_counties_cross, by = "COUNTY_NAME") %>%
  mutate(COUNTY_NAME = ifelse(is.na(CLEAN_COUNTY_NAME), COUNTY_NAME, CLEAN_COUNTY_NAME)) %>%
  # make all cities lowercase
  mutate(COUNTY_NAME = str_replace(COUNTY_NAME, " City", " city")) %>% # make any uppercase "city" to lowercase
  # bring in the GEOIDs
  left_join(county_cross2 %>% mutate(COUNTY_NAME = str_replace(COUNTY_NAME, " City", " city")),  # same in the crosswalk
            by = c("COUNTY_NAME"))

# should have no missing GEOIDs
demo_lancet5 %>% filter(is.na(GEOID)) %>% nrow()
# [1] 0

# remove a column we don't need anymore
demo_lancet5 <- demo_lancet5 %>% select(-CLEAN_COUNTY_NAME)




# create estimated age variable


demo_lancet6 <- demo_lancet5 %>% 
  # get first four digits for the time participant started baseline
  mutate(baseline_start_year = as.numeric(str_extract(task_start_time_baseline, "^\\d{4}"))) %>%
  # 16055 people have missing baseline start year
  ### make conservative estimate of 2019 for these people
  mutate(baseline_start_year = ifelse(is.na(baseline_start_year), 2019, baseline_start_year)) %>% 
  mutate(age_est = baseline_start_year - birth_year)

demo_lancet6 %>% filter(age_est < 18) %>% nrow()
# 555 people are below 18 - we need at least 18 for the analysis

demo_lancet6 <- demo_lancet6 %>% filter(age_est >= 18)
nrow(demo_lancet6)
# [1] 164422


# create 10 age classifications to match to ACS data
demo_lancet6 <- demo_lancet6 %>%
  mutate(age = 
           cut(
             age_est,
             breaks = c(17, 19, 24, 29, 34, 44, 54, 65, 74, 84, Inf),
             labels = c("18_19",  "20_24",  "25_29",  "30_34",  "35_44",  "45_54",  "55_64",  "65_74",  "75_84", "85_inf")
           )
  ) %>%
  mutate(age = as.factor(age),
         age = relevel(age, "35_44")) # because this has the most data and is in the middle

round(prop.table(table(demo_lancet6$age)), 3)
# 35_44  18_19  20_24  25_29  30_34  45_54  55_64  65_74  75_84 85_inf 
# 0.259  0.042  0.130  0.144  0.153  0.150  0.080  0.033  0.009  0.001 


demo_lancet6 <- demo_lancet6 %>%
  mutate(sex = case_when(
    assigned_sex == "[Female]" ~ "female",
    assigned_sex == "[Male]" ~ "male",
    TRUE ~ NA
  )) %>% 
  filter(!is.na(sex)) %>% # need only male/female for post stratification
  mutate(sex = factor(sex, levels = c("male", "female")))

nrow(demo_lancet6)
# [1] 163859
163859 - 164422
# 563 people were removed who were not male or female

round(prop.table(table(demo_lancet6$sex)), 3)
#  male female 
# 0.623  0.377 


demo_lancet6 <- demo_lancet6 %>%
  mutate(education = case_when(                      # education
    education %in% c("[never_attended_school_or_only_attended_kindergarten]",
                     "[grades_1_through_4]","[grades_5_through_9]",
                     "[grades_9_through_11]") ~ "Not HS grad or GED",
    education %in% c("[grade_12_or_GED]") ~ "HS grad or GED",
    education %in% c("[1_to_3_years_after_high_school_tech_school]",
                     "[1_to_3_years_after_high_school_college]") ~ "Some college",
    education %in% c("[college_4_years_or_more]") ~ "Bachelors degree",
    education %in% c("[advanced_degree_masters]","[advanced_degree_doc]") ~ "Graduate degree",
    education == "[skip]" ~ "Skip",
    TRUE ~ NA))

round(prop.table(table(demo_lancet6$education)), 3)
#  Bachelors degree    Graduate degree     HS grad or GED Not HS grad or GED               Skip       Some college 
#             0.313              0.233              0.127              0.021              0.005              0.302 

demo_lancet6 <- demo_lancet6 %>%
  mutate(race = case_when(                            # race/ethnicity categorizing
    str_detect(ethnicity, "white") &  !str_detect(ethnicity, "hispanic") &  !str_detect(ethnicity, "asian") &  !str_detect(ethnicity, "black") ~ "whitenh",
    str_detect(ethnicity, "asian") &  !str_detect(ethnicity, "hispanic") &  !str_detect(ethnicity, "white") &  !str_detect(ethnicity, "black") ~ "asian",
    str_detect(ethnicity, "black") &  !str_detect(ethnicity, "hispanic")  &  !str_detect(ethnicity, "asian") &  !str_detect(ethnicity, "white") ~ "blackNH",
    str_detect(ethnicity, "hispanic") ~ "hispanic",
    is.na(ethnicity) ~ NA,
    TRUE ~ "other_multi")) %>%
  mutate(race = as.factor(race),
         race = relevel(race, ref = "whitenh"))

demo_lancet6 %>% filter(is.na(race)) %>% nrow()
# 2 people do not have a race

demo_lancet6 <- demo_lancet6 %>% filter(!is.na(race))
nrow(demo_lancet6)
# [1] 163857


















# -------------------- 
# 4. Add in the environmental noise levels
# --------------------


environment_daily_average$duration_hours %>% summary()
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   5.46   13.42   15.44   15.92   18.72   41.21   64008 
####### duration_hours are odd

environment_daily_average$lex_24 %>% summary()
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   31.33   66.37   68.81   68.78   71.24   95.76   64008 
####### these look fine

environment_daily_average2 <- environment_daily_average %>%
  filter(duration_hours <= 24)
nrow(environment_daily_average2)
# [1] 122867

nrow(environment_daily_average2) - nrow(environment_daily_average)
# [1] -64062
# removed 64,062 participants


full_lancet_data <- demo_lancet6 %>% left_join(environment_daily_average2, by = "canonical_subject_id")
dim(full_lancet_data)
# [1] 163857     39





# -------------------- 
# 5. Add in county-level fixed effects for poverty and audiologists
# --------------------

load("Data/01_imported/acs2019_county_income2poverty150_locq.RData")

df_incpov150_2 <- df_incpov150_locq %>% mutate(perc_incpov150 = round(perc, 2)) %>% select(-c(NAME:perc))

full_lancet_data <- full_lancet_data %>%
  left_join(df_incpov150_2, by = "GEOID")












# -------------------- 
# 6. Export the full data
# --------------------

save(full_lancet_data, file = "Data/02_generated/processed_lancet_data_v2.RData")





