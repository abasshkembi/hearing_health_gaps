# Filename: 03_table1.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: March 15 2024

# Description: Nationwide estimation of prevalence and cases of four main outcomes
# ------------ for the construction of Table 1


library(tidyverse)
setwd("/home/hdsuser/Papers/Lancet 2023 Analysis/")

# -------------------- 
# 1. Noise estimates
# --------------------

load("Data/02_generated/SAEs/prob_noise_70.RData")
noise_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

# nationwide overall
noise_sae %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# age breakdown nationwide
noise_sae %>%
  group_by(age) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# sex breakdown nationwide
noise_sae %>%
  group_by(sex) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# race breakdown nationwide
noise_sae %>%
  group_by(race) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 






# -------------------- 
# 2. Self-reported poor/fair hearing ability estimates
# --------------------


load("Data/02_generated/SAEs/prob_srHL.RData")
srhl_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

# nationwide overall
srhl_sae %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# age breakdown nationwide
srhl_sae %>%
  group_by(age) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# sex breakdown nationwide
srhl_sae %>%
  group_by(sex) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# race breakdown nationwide
srhl_sae %>%
  group_by(race) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 








# -------------------- 
# 3. Self-reported hearing loss diagnosis estimates
# --------------------


load("Data/02_generated/SAEs/prob_srDx.RData")
srdx_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

# nationwide overall
srdx_sae %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# age breakdown nationwide
srdx_sae %>%
  group_by(age) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# sex breakdown nationwide
srdx_sae %>%
  group_by(sex) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# race breakdown nationwide
srdx_sae %>%
  group_by(race) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 









# -------------------- 
# 4. Self-reported hearing aid/cochlear implant estimates
# --------------------


load("Data/02_generated/SAEs/prob_haci_full_yes.RData")
haci_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

# nationwide overall
haci_sae %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# age breakdown nationwide
haci_sae %>%
  group_by(age) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# sex breakdown nationwide
haci_sae %>%
  group_by(sex) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 

# race breakdown nationwide
haci_sae %>%
  group_by(race) %>%
  summarise(n_county = n(),p_pred = sum(p_pred),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() 



