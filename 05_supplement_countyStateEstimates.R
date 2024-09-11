# Filename: 05_supplement_countyStateEstimates.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: March 15 2024

# Description: county and state-level breakdowns of main outcomes for Appendix D


library(tidyverse)
setwd("/home/hdsuser/Papers/Lancet 2023 Analysis/")

# -------------------- 
# 1. Noise estimates
# --------------------

load("Data/02_generated/SAEs/prob_noise_70.RData")
noise_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

noise_county <- noise_sae %>%
  group_by(GEOID, NAME) %>%
  summarise(n_county = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_county*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_county*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_county*100, 1)) %>%
  ungroup()

write.csv(noise_county, "Estimates/county_noiseOverexposure.csv")

noise_state <- noise_sae %>%
  group_by(state_name) %>%
  summarise(n_state = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_state*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_state*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_state*100, 1)) %>%
  ungroup()
noise_state %>% arrange(-prop_pred)

write.csv(noise_state, "Estimates/state_noiseOverexposure.csv")


## age-adjusted by county

age_weights <- noise_sae %>%
  group_by(age) %>%
  summarise(total_pop = sum(count)) %>%
  ungroup() %>%
  mutate(weight = total_pop/sum(total_pop))

noise_county_ageAdj <- noise_sae %>%
  group_by(GEOID, NAME, state_name, age) %>%
  summarise(n_county = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_county*1000, 3),
         prop_pred_q2.5 = round(n_pred_q2.5/n_county*1000, 3),
         prop_pred_q97.5 = round(n_pred_q97.5/n_county*1000, 3),) %>%
  ungroup() %>%
  left_join(age_weights, by = "age") %>%
  mutate(prop_pred = ifelse(n_county == 0, 0, prop_pred),
         prop_pred_q2.5 = ifelse(n_county == 0, 0, prop_pred_q2.5),
         prop_pred_q97.5 = ifelse(n_county == 0, 0, prop_pred_q97.5)) %>%
  mutate(prop_ageAdj = prop_pred*weight,
         prop_ageAdj_q2.5 = prop_pred_q2.5*weight,
         prop_ageAdj_q97.5 = prop_pred_q97.5*weight) %>%
  group_by(GEOID, NAME) %>%
  summarise(ageAdj_rate = round(sum(prop_ageAdj), 1),
            ageAdj_rate_q2.5 = round(sum(prop_ageAdj_q2.5), 1),
            ageAdj_rate_q97.5 = round(sum(prop_ageAdj_q97.5), 1)) %>%
  ungroup()
noise_county_ageAdj

write.csv(noise_county_ageAdj, "Estimates/county_noiseOverexposure_ageAdjustedRates.csv")

# -------------------- 
# 2. Self-reported poor/fair hearing ability
# --------------------

load("Data/02_generated/SAEs/prob_srHL.RData")
srhl_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))


srHL_county <- srhl_sae %>%
  group_by(GEOID, NAME) %>%
  summarise(n_county = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_county*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_county*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_county*100, 1)) %>%
  ungroup()
srHL_county

write.csv(srHL_county, "Estimates/county_poorfairHearingAbility.csv")

srHL_state <- srhl_sae %>%
  group_by(state_name) %>%
  summarise(n_state = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_state*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_state*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_state*100, 1)) %>%
  ungroup()
srHL_state %>% arrange(-prop_pred)

write.csv(srHL_state, "Estimates/state_poorfairHearingAbility.csv")

## age-adjusted by county

age_weights <- srhl_sae %>%
  group_by(age) %>%
  summarise(total_pop = sum(count)) %>%
  ungroup() %>%
  mutate(weight = total_pop/sum(total_pop))

srHL_county_ageAdj <- srhl_sae %>%
  group_by(GEOID, NAME, state_name, age) %>%
  summarise(n_county = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_county*1000, 3),
         prop_pred_q2.5 = round(n_pred_q2.5/n_county*1000, 3),
         prop_pred_q97.5 = round(n_pred_q97.5/n_county*1000, 3),) %>%
  ungroup() %>%
  left_join(age_weights, by = "age") %>%
  mutate(prop_pred = ifelse(n_county == 0, 0, prop_pred),
         prop_pred_q2.5 = ifelse(n_county == 0, 0, prop_pred_q2.5),
         prop_pred_q97.5 = ifelse(n_county == 0, 0, prop_pred_q97.5)) %>%
  mutate(prop_ageAdj = prop_pred*weight,
         prop_ageAdj_q2.5 = prop_pred_q2.5*weight,
         prop_ageAdj_q97.5 = prop_pred_q97.5*weight) %>%
  group_by(GEOID, NAME) %>%
  summarise(ageAdj_rate = round(sum(prop_ageAdj), 1),
            ageAdj_rate_q2.5 = round(sum(prop_ageAdj_q2.5), 1),
            ageAdj_rate_q97.5 = round(sum(prop_ageAdj_q97.5), 1)) %>%
  ungroup()
srHL_county_ageAdj

write.csv(srHL_county_ageAdj, "Estimates/county_poorfairHearingAbility_ageAdjustedRates.csv")


# -------------------- 
# 3. Self-reported hearing loss diagnosis
# --------------------

load("Data/02_generated/SAEs/prob_srDx.RData")
srdx_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))


srDx_county <- srdx_sae %>%
  group_by(GEOID, NAME) %>%
  summarise(n_county = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_county*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_county*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_county*100, 1)) %>%
  ungroup()
srDx_county

write.csv(srDx_county, "Estimates/county_hearinglossDx.csv")

srDx_state <- srdx_sae %>%
  group_by(state_name) %>%
  summarise(n_state = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_state*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_state*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_state*100, 1)) %>%
  ungroup()
srDx_state %>% arrange(-prop_pred)

write.csv(srDx_state, "Estimates/state_hearinglossDx.csv")


# -------------------- 
# 4. Hearing aid/cochlear implant use
# --------------------

load("Data/02_generated/SAEs/prob_haci_full_yes.RData")
haci_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))


srHACI_county <- haci_sae %>%
  group_by(GEOID, NAME) %>%
  summarise(n_county = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_county*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_county*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_county*100, 1)) %>%
  ungroup()
srHACI_county

write.csv(srHACI_county, "Estimates/county_hearingAidUse.csv")

srHACI_state <- haci_sae %>%
  group_by(state_name) %>%
  summarise(n_state = sum(count),
            n_pred = sum(p_pred),
            n_pred_q2.5 = sum(p_pred_q2.5),
            n_pred_q97.5 = sum(p_pred_q97.5)) %>%
  mutate(prop_pred = round(n_pred/n_state*100, 1),
         prop_pred_q2.5 = round(n_pred_q2.5/n_state*100, 1),
         prop_pred_q97.5 = round(n_pred_q97.5/n_state*100, 1)) %>%
  ungroup()
srHACI_state %>% arrange(-prop_pred)

write.csv(srHACI_state, "Estimates/state_hearingAidUse.csv")






# -------------------- 
# 5. No hearing loss diagnosis among those with poor/fair hearing ability
# --------------------

srDxNOsrHL_county <- srDx_county %>%
  select(GEOID, NAME, n_county, 
         n_pred_Dx = n_pred, 
         n_pred_Dx_q2.5 = n_pred_q2.5, 
         n_pred_Dx_q97.5 = n_pred_q97.5) %>%
  full_join(
    srHL_county %>%
      select(GEOID, 
             n_pred_HL = n_pred, 
             n_pred_HL_q2.5 = n_pred_q2.5, 
             n_pred_HL_q97.5 = n_pred_q97.5),
    by = "GEOID"
  ) %>%
  group_by(GEOID, NAME, n_county) %>%
  transmute(prop_noDx = round((n_pred_HL-n_pred_Dx)/n_pred_HL*100, 1),
            prop_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/n_pred_HL_q97.5*100, 1),
            prop_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/n_pred_HL_q2.5*100, 1)) %>%
  ungroup()

srDxNOsrHL_county %>% arrange(-prop_noDx) %>% filter(str_detect(NAME, ", Michigan"))

write.csv(srDxNOsrHL_county, "Estimates/county_noHearingLossDiagnosis.csv")



srDxNOsrHL_state <- srDx_state %>%
  select(state_name, n_state, 
         n_pred_Dx = n_pred, 
         n_pred_Dx_q2.5 = n_pred_q2.5, 
         n_pred_Dx_q97.5 = n_pred_q97.5) %>%
  full_join(
    srHL_state %>%
      select(state_name, 
             n_pred_HL = n_pred, 
             n_pred_HL_q2.5 = n_pred_q2.5, 
             n_pred_HL_q97.5 = n_pred_q97.5),
    by = "state_name"
  ) %>%
  group_by(state_name, n_state) %>%
  transmute(prop_noDx = round((n_pred_HL-n_pred_Dx)/n_pred_HL*100, 3),
            prop_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/n_pred_HL_q97.5*100, 3),
            prop_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/n_pred_HL_q2.5*100, 3)) %>%
  ungroup()

srDxNOsrHL_state %>% arrange(-prop_noDx)

write.csv(srDxNOsrHL_state, "Estimates/state_noHearingLossDiagnosis.csv")







# nationwide
srhl_sae %>%
  summarise(n_pred_HL = sum(p_pred),
            n_pred_HL_q2.5 = sum(p_pred_q2.5),
            n_pred_HL_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  cbind(
    srdx_sae %>%
      summarise(n_pred_Dx = sum(p_pred),
                n_pred_Dx_q2.5 = sum(p_pred_q2.5),
                n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  mutate(
    n_noDx = round((n_pred_HL-n_pred_Dx)/1000000, 1), # in millions
    n_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/1000000, 1),
    n_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/1000000, 1),
    prop_noDx = round((n_pred_HL-n_pred_Dx)/n_pred_HL*100, 1),
    prop_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/n_pred_HL_q97.5*100, 1),
    prop_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/n_pred_HL_q2.5*100, 1)) %>%
  transmute(
    prevalence = paste0(prop_noDx, " (", prop_noDx_q2.5, ", ", prop_noDx_q97.5, ")"),
    cases = paste0(n_noDx, " (", n_noDx_q2.5, ", ", n_noDx_q97.5, ")")
  )

#age
srhl_sae %>%
  group_by(age) %>%
  summarise(n_pred_HL = sum(p_pred),
            n_pred_HL_q2.5 = sum(p_pred_q2.5),
            n_pred_HL_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  full_join(
    srdx_sae %>%
      group_by(age) %>%
      summarise(n_pred_Dx = sum(p_pred),
                n_pred_Dx_q2.5 = sum(p_pred_q2.5),
                n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  mutate(age = age, 
            n_noDx = round((n_pred_HL-n_pred_Dx)/1000000, 1), # in millions
            n_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/1000000, 1),
            n_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/1000000, 1),
            prop_noDx = round((n_pred_HL-n_pred_Dx)/n_pred_HL*100, 1),
            prop_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/n_pred_HL_q97.5*100, 1),
            prop_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/n_pred_HL_q2.5*100, 1)) %>%
  transmute(
    age = paste0(age, "age______"),
    prevalence = paste0(prop_noDx, " (", prop_noDx_q2.5, ", ", prop_noDx_q97.5, ")"),
    cases = paste0(n_noDx, " (", n_noDx_q97.5, ", ", n_noDx_q2.5, ")")
  )


#sex
srhl_sae %>%
  group_by(sex) %>%
  summarise(n_pred_HL = sum(p_pred),
            n_pred_HL_q2.5 = sum(p_pred_q2.5),
            n_pred_HL_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  full_join(
    srdx_sae %>%
      group_by(sex) %>%
      summarise(n_pred_Dx = sum(p_pred),
                n_pred_Dx_q2.5 = sum(p_pred_q2.5),
                n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  transmute(sex = sex, 
            n_noDx = round((n_pred_HL-n_pred_Dx)/1000000, 1),
            n_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/1000000, 1),
            n_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/1000000, 1),
            prop_noDx = round((n_pred_HL-n_pred_Dx)/n_pred_HL*100, 1),
            prop_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/n_pred_HL_q97.5*100, 1),
            prop_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/n_pred_HL_q2.5*100, 1)) %>%
  transmute(
    sex = sex,
    prevalence = paste0(prop_noDx, " (", prop_noDx_q2.5, ", ", prop_noDx_q97.5, ")"),
    cases = paste0(n_noDx, " (", n_noDx_q97.5, ", ", n_noDx_q2.5, ")")
  )


#race
srhl_sae %>%
  group_by(race) %>%
  summarise(n_pred_HL = sum(p_pred),
            n_pred_HL_q2.5 = sum(p_pred_q2.5),
            n_pred_HL_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  full_join(
    srdx_sae %>%
      group_by(race) %>%
      summarise(n_pred_Dx = sum(p_pred),
                n_pred_Dx_q2.5 = sum(p_pred_q2.5),
                n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  transmute(race = race, 
            n_noDx = round((n_pred_HL-n_pred_Dx)/1000000, 1),
            n_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/1000000, 1),
            n_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/1000000, 1),
            prop_noDx = round((n_pred_HL-n_pred_Dx)/n_pred_HL*100, 1),
            prop_noDx_q2.5 = round((n_pred_HL_q97.5-n_pred_Dx_q97.5)/n_pred_HL_q97.5*100, 1),
            prop_noDx_q97.5 = round((n_pred_HL_q2.5-n_pred_Dx_q2.5)/n_pred_HL_q2.5*100, 1)) %>%
  transmute(
    race = race,
    prevalence = paste0(prop_noDx, " (", prop_noDx_q2.5, ", ", prop_noDx_q97.5, ")"),
    cases = paste0(n_noDx, " (", n_noDx_q97.5, ", ", n_noDx_q2.5, ")")
  )





# -------------------- 
# 6. No hearing aid use among those with a diagnosed hearing loss
# --------------------

srHACINOsrDx_county <- srHACI_county %>%
  select(GEOID, NAME, n_county, 
         n_pred_HACI = n_pred, 
         n_pred_HACI_q2.5 = n_pred_q2.5, 
         n_pred_HACI_q97.5 = n_pred_q97.5) %>%
  full_join(
    srDx_county %>%
      select(GEOID, 
             n_pred_Dx = n_pred, 
             n_pred_Dx_q2.5 = n_pred_q2.5, 
             n_pred_Dx_q97.5 = n_pred_q97.5),
    by = "GEOID"
  ) %>%
  group_by(GEOID, NAME, n_county) %>%
  transmute(prop_noHACI = round((n_pred_Dx-n_pred_HACI)/n_pred_Dx*100, 1),
            prop_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/n_pred_Dx_q97.5*100, 1),
            prop_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/n_pred_Dx_q2.5*100, 1)) %>%
  ungroup()

srHACINOsrDx_county %>% arrange(-prop_noHACI) %>% filter(str_detect(NAME, ", Michigan"))

write.csv(srHACINOsrDx_county, "Estimates/county_noHearingAidUse.csv")



srHACINOsrDx_state <- srHACI_state %>%
  select(state_name, n_state, 
         n_pred_HACI = n_pred, 
         n_pred_HACI_q2.5 = n_pred_q2.5, 
         n_pred_HACI_q97.5 = n_pred_q97.5) %>%
  full_join(
    srDx_state %>%
      select(state_name, 
             n_pred_Dx = n_pred, 
             n_pred_Dx_q2.5 = n_pred_q2.5, 
             n_pred_Dx_q97.5 = n_pred_q97.5),
    by = "state_name"
  ) %>%
  group_by(state_name, n_state) %>%
  transmute(prop_noHACI = round((n_pred_Dx-n_pred_HACI)/n_pred_Dx*100, 3),
            prop_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/n_pred_Dx_q97.5*100, 3),
            prop_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/n_pred_Dx_q2.5*100, 3)) %>%
  ungroup()

srHACINOsrDx_state %>% arrange(-prop_noHACI)

write.csv(srHACINOsrDx_state, "Estimates/state_noHearingAidUse.csv")





# nationwide
srdx_sae %>%
  summarise(n_pred_Dx = sum(p_pred),
            n_pred_Dx_q2.5 = sum(p_pred_q2.5),
            n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  cbind(
    haci_sae %>%
      summarise(n_pred_HACI = sum(p_pred),
                n_pred_HACI_q2.5 = sum(p_pred_q2.5),
                n_pred_HACI_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  transmute(
    n_noHACI = round((n_pred_Dx-n_pred_HACI)/1000000, 1), # in millions
    n_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/1000000, 1),
    n_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/1000000, 1),
    prop_noHACI = round((n_pred_Dx-n_pred_HACI)/n_pred_Dx*100, 1),
    prop_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/n_pred_Dx_q97.5*100, 1),
    prop_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/n_pred_Dx_q2.5*100, 1)) %>%
  transmute(
    prevalence = paste0(prop_noHACI, " (", prop_noHACI_q2.5, ", ", prop_noHACI_q97.5, ")"),
    cases = paste0(n_noHACI, " (", n_noHACI_q97.5, ", ", n_noHACI_q2.5, ")")
  )

#age
srdx_sae %>%
  group_by(age) %>%
  summarise(n_pred_Dx = sum(p_pred),
            n_pred_Dx_q2.5 = sum(p_pred_q2.5),
            n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  full_join(
    haci_sae %>%
      group_by(age) %>%
      summarise(n_pred_HACI = sum(p_pred),
                n_pred_HACI_q2.5 = sum(p_pred_q2.5),
                n_pred_HACI_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  transmute(age = age, 
            n_noHACI = round((n_pred_Dx-n_pred_HACI)/1000000, 1), # in millions
            n_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/1000000, 1),
            n_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/1000000, 1),
            prop_noHACI = round((n_pred_Dx-n_pred_HACI)/n_pred_Dx*100, 1),
            prop_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/n_pred_Dx_q97.5*100, 1),
            prop_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/n_pred_Dx_q2.5*100, 1)) %>%
  transmute(
    age = paste0(age, "age_______"),
    prevalence = paste0(prop_noHACI, " (", prop_noHACI_q2.5, ", ", prop_noHACI_q97.5, ")"),
    cases = paste0(n_noHACI, " (", n_noHACI_q97.5, ", ", n_noHACI_q2.5, ")")
  )


#sex
srdx_sae %>%
  group_by(sex) %>%
  summarise(n_pred_Dx = sum(p_pred),
            n_pred_Dx_q2.5 = sum(p_pred_q2.5),
            n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  full_join(
    haci_sae %>%
      group_by(sex) %>%
      summarise(n_pred_HACI = sum(p_pred),
                n_pred_HACI_q2.5 = sum(p_pred_q2.5),
                n_pred_HACI_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  transmute(sex = sex, 
            n_noHACI = round((n_pred_Dx-n_pred_HACI)/1000000, 1),
            n_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/1000000, 1),
            n_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/1000000, 1),
            prop_noHACI = round((n_pred_Dx-n_pred_HACI)/n_pred_Dx*100, 1),
            prop_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/n_pred_Dx_q97.5*100, 1),
            prop_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/n_pred_Dx_q2.5*100, 1)) %>%
  transmute(
    sex = sex,
    prevalence = paste0(prop_noHACI, " (", prop_noHACI_q2.5, ", ", prop_noHACI_q97.5, ")"),
    cases = paste0(n_noHACI, " (", n_noHACI_q97.5, ", ", n_noHACI_q2.5, ")")
  )


#race
srdx_sae %>%
  group_by(race) %>%
  summarise(n_pred_Dx = sum(p_pred),
            n_pred_Dx_q2.5 = sum(p_pred_q2.5),
            n_pred_Dx_q97.5 = sum(p_pred_q97.5)) %>%
  ungroup() %>%
  full_join(
    haci_sae %>%
      group_by(race) %>%
      summarise(n_pred_HACI = sum(p_pred),
                n_pred_HACI_q2.5 = sum(p_pred_q2.5),
                n_pred_HACI_q97.5 = sum(p_pred_q97.5)) %>%
      ungroup()
  ) %>%
  transmute(race = race, 
            n_noHACI = round((n_pred_Dx-n_pred_HACI)/1000000, 1),
            n_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/1000000, 1),
            n_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/1000000, 1),
            prop_noHACI = round((n_pred_Dx-n_pred_HACI)/n_pred_Dx*100, 1),
            prop_noHACI_q2.5 = round((n_pred_Dx_q97.5-n_pred_HACI_q97.5)/n_pred_Dx_q97.5*100, 1),
            prop_noHACI_q97.5 = round((n_pred_Dx_q2.5-n_pred_HACI_q2.5)/n_pred_Dx_q2.5*100, 1)) %>%
  transmute(
    race = race,
    prevalence = paste0(prop_noHACI, " (", prop_noHACI_q2.5, ", ", prop_noHACI_q97.5, ")"),
    cases = paste0(n_noHACI, " (", n_noHACI_q97.5, ", ", n_noHACI_q2.5, ")")
  )
