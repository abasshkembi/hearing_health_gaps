# Filename: 02E_allOtherModels_model.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: March 7 2024

# Description: Model construction for (1) no hearing test and (2) no hearing aid use in Cohort One

# Study population: Cohort One (Nov 14, 2019 to Nov 14, 2022)

# Model outcomes
#----- (1) No hearing test  among all participants
#----- (2) No hearing aid/cochlear implant use among those with self-reported
#-----     diagnosed hearing loss



# -------------------- 
# 1. Model 1 - Hearing test
# --------------------

load("Data/02_generated/processed_lancet_data.RData")
load("Data/01_imported/acs2019_county_income2poverty150_locq.RData")

df_incpov150_2 <- df_incpov150_locq %>% 
  mutate(perc_incpov150 = round(perc, 2)) %>% 
  select(-c(NAME:perc))


full_lancet_data2 <- full_lancet_data %>%
  left_join(df_incpov150_2, by = "GEOID") %>%
  mutate(age2 = as.character(age)) %>%
  mutate(age2 = ifelse(age2 == "85_inf", "75_84", age2)) %>%
  mutate(age = as.factor(age2),
         age = relevel(age, "65_74"))

table(full_lancet_data2$hearing_ability_audiogram_q)
full_lancet_data2 %>% count(hearing_ability_audiogram_q)


# create dataframe for model
df_trial_srht <- full_lancet_data2 %>%
  # if it's been more than 10 years or never done hearing test
  mutate(sr_ht = ifelse(hearing_ability_audiogram_q %in% c("[moreTen]", "[never]"), 1, 0)) %>%
  filter(hearing_ability_audiogram_q != "[ptna]") %>% # remove prefer not to answer people
  # rearrange reference of location quotient
  mutate(loc_q_cat = factor(loc_q_cat, levels = c("High", "Low", "Unknown"))) %>%
  select(age, sex, race, perc_incpov150, loc_q_cat, state_name, GEOID, sr_ht) %>% 
  na.omit()

table(df_trial_srht$sr_ht)
#     0      1 
# 49653   39113 



mod_sr_ht <- df_trial_srht %>%
  glmer(sr_ht ~ age + sex + race + perc_incpov150 + loc_q_cat + (1|GEOID) + (1|state_name), family = "binomial", data = .)

#saveRDS(mod_sr_ht, file = "Models/mod_sr_ht.rds")

summary(mod_sr_ht)










# -------------------- 
# 1. Model 2 - No hearing aid/cochlear implant use AMONG those with self-report hearing loss diagnosis
# --------------------

load("Data/02_generated/processed_lancet_data.RData")
#load("Data/01_imported/acs2019_county_income2poverty150.RData")
load("Data/01_imported/acs2019_county_income2poverty150_locq.RData")

df_incpov150_2 <- df_incpov150_locq %>% mutate(perc_incpov150 = round(perc, 2)) %>% select(-c(NAME:perc))


full_lancet_data2 <- full_lancet_data %>%
  left_join(df_incpov150_2, by = "GEOID") %>%
  mutate(age2 = as.character(age)) %>%
  mutate(age2 = ifelse(age2 == "85_inf", "75_84", age2)) %>%
  mutate(age = as.factor(age2),
         age = relevel(age, "65_74"))

table(full_lancet_data2$hearing_aid_wear_q)
full_lancet_data2 %>% count(hearing_aid_wear_q)

df_trial_haci_small <- full_lancet_data2 %>%
  mutate(sr_haci = ifelse(hearing_aid_wear_q %in% c("[no]"), 1, 0)) %>%
  filter(hearing_aid_wear_q != "[ptna]") %>%
  filter(!is.na(hearing_aid_wear_q)) %>%
  select(age, sex, race, state_name, GEOID, sr_haci) %>% 
  na.omit()

table(df_trial_haci_small$sr_haci)
#   0    1 
# 3492 9217

df_trial_haci_small %>% dplyr::count(age)


mod_srhaci <- df_trial_haci_small %>%
  glmer(sr_haci ~ age + sex + race + (1|GEOID) + (1|state_name), family = "binomial", data = .)

#saveRDS(mod_srhaci, file = "Models/mod_srhaci.rds")

summary(mod_srhaci)










