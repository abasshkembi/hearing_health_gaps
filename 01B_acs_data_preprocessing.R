# Filename: 01_data_preprocessing.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: December 18 2023

# Description: ACS/BLS data on county demographics, income, and audiologist concentration
# ------------ for post-stratification after multilevel regression


library(tidyverse)

# the county breakdowns by age, sex, and race
load("/home/hdsuser/Papers/Lancet 2023 Analysis/Data/01_imported/acs2019_county_ageSexRace.RData")

# county-level estimates of the percent of individuals <150% of the federal poverty limit
# and metropolitan/nonmetropolitan estimates of the concentration of audiologists, mapped to county
load("/home/hdsuser/Papers/Lancet 2023 Analysis/Data/01_imported/acs2019_county_income2poverty150_locq.RData")

df_incpov150_2 <- df_incpov150_locq %>% mutate(perc_incpov150 = round(perc, 2)) %>% select(-c(NAME:perc))

# decided to combine all those >75 years old in the analysis
# so changing the ACS data to combine 85+ with 75-84
count_by_county1 <- count_by_county %>%
  filter(age %in% c("75_84", "85_inf")) %>%
  group_by(GEOID, NAME, sex, race) %>%
  summarise(count = sum(count),
            total = median(total)) %>%
  ungroup() %>%
  mutate(age = "75_84") %>%
  select(GEOID, NAME, sex, age, total, race, count)

count_by_county2 <- count_by_county %>%
  filter(!(age %in% c("75_84", "85_inf")))

new_count_by_county <-
  count_by_county2 %>% rbind(count_by_county1)


acs_df_mod <- new_count_by_county %>%
  # extract state name
  mutate(state_name = str_extract(NAME, ", .+$") %>% str_remove(", ")) %>%
  group_by(GEOID) %>%
  mutate(sum_county = sum(count)) %>%
  ungroup() %>%
  mutate(prop = count/sum_county) %>%
  # add in the income and audiooligst info to the ACS data
  left_join(df_incpov150_2, by = "GEOID")


# final data used for post-stratification
save(acs_df_mod, file = "Data/02_generated/acs2019_county_full.RData")



