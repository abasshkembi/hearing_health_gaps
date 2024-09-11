# Filename: 02D_hearingAidUse_model.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: December 18 2023

# Description: Model construction for Small Area Estimation for using hearing aids/cochlear implants in Cohort One

# Study population: Cohort One (Nov 14, 2019 to Nov 14, 2022)

# Model outcome: Hearing aid/cochlear implant use




# -------------------- 
# 1. Read in raw data files and set up environment
# --------------------


library(tidyverse)
library(lme4)
library(matrixStats)

load("Data/02_generated/processed_lancet_data_v2.RData")
load("Data/02_generated/acs2019_county_full.RData")




# get data ready for model

full_lancet_data2 <- full_lancet_data %>%
  mutate(age2 = as.character(age)) %>%
  mutate(age2 = ifelse(age2 == "85_inf", "75_84", age2)) %>% # group 85+ to 75+
  mutate(age = as.factor(age2),
         age = relevel(age, "65_74")) # set 65 to 74 as reference

# get complete cases for data
full_lancet_data2 %>% dplyr::count(hearing_aid_wear_q)
#   hearing_aid_wear_q      n
# 1 [both]                 71
# 2 [cochlearImplant]     103
# 3 [hearingAid]         3318
# 4 [no]                 9217
# 5 [ptna]                 38
# 6 NA                 151110

df_trial_haci_full_yes <- full_lancet_data2 %>%
  mutate(hearing_aid_wear_q = ifelse(hearing_loss_diagnosed_q == "[no]", "[no HL Dx]", hearing_aid_wear_q)) %>%
  mutate(sr_haci_yes = ifelse(hearing_aid_wear_q %in% c("[both]", "[cochlearImplant]", "[hearingAid]"), 1, 0)) %>%
  filter(hearing_aid_wear_q != "[ptna]") %>%
  filter(!is.na(hearing_aid_wear_q)) %>%
  select(age, sex, race, perc_incpov150, state_name, GEOID, sr_haci_yes) %>% 
  na.omit()

dim(df_trial_haci_full_yes)
#[1] 123705      7

table(df_trial_haci_full_yes$sr_haci_yes)
#       0      1 
#  120228   3477 




# -------------------- 
# 2. Run the model
# --------------------




mod_srhaci_full_yes <- df_trial_haci_full_yes %>%
  glmer(sr_haci_yes ~ age + sex + race + perc_incpov150 + (1|GEOID) + (1|state_name), family = "binomial", data = .)

#### save the model
#saveRDS(mod_srhaci_full_yes, file = "Models/mod_srhaci_full_yes.rds")
#### read in the saved model
mod_srhaci_full_yes <- readRDS("/home/hdsuser/Papers/Lancet 2023 Analysis/Models/mod_srhaci_full_yes.rds")

summary(mod_srhaci_full_yes)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: sr_haci_yes ~ age + sex + race + perc_incpov150 + (1 | GEOID) +      (1 | state_name)
#Data: .

#     AIC      BIC   logLik deviance df.resid 
# 26261.7  26427.1 -13113.9  26227.7   123688 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.9955 -0.1430 -0.1089 -0.0920 15.3738 

#Random effects:
#  Groups     Name        Variance Std.Dev.
#GEOID      (Intercept) 0.01757  0.1325  
#state_name (Intercept) 0.01242  0.1114  
#Number of obs: 123705, groups:  GEOID, 2703; state_name, 52

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -1.3295478  0.0772876 -17.203  < 2e-16 ***
#  age18_19        -3.3669351  0.1659052 -20.294  < 2e-16 ***
#  age20_24        -3.2046923  0.0941896 -34.024  < 2e-16 ***
#  age25_29        -3.2333355  0.0922415 -35.053  < 2e-16 ***
#  age30_34        -3.1096971  0.0853162 -36.449  < 2e-16 ***
#  age35_44        -2.6674555  0.0620692 -42.976  < 2e-16 ***
#  age45_54        -1.9742997  0.0599257 -32.946  < 2e-16 ***
#  age55_64        -1.0403157  0.0567275 -18.339  < 2e-16 ***
#  age75_84         1.1357102  0.0728745  15.584  < 2e-16 ***
#  sexfemale       -0.3007448  0.0393542  -7.642 2.14e-14 ***
#  raceasian       -0.3441924  0.1083655  -3.176  0.00149 ** 
#  raceblackNH     -0.3322766  0.1056983  -3.144  0.00167 ** 
#  racehispanic    -0.1358121  0.0719663  -1.887  0.05914 .  
#  raceother_multi  0.0616101  0.0901710   0.683  0.49444    
#  perc_incpov150  -0.0002577  0.0030937  -0.083  0.93362    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# -------------------- 
# 3. Small area estimation
# --------------------

# create dummy data frame of df for SAE
acs_df_mod2 <- acs_df_mod %>%
  mutate(age = relevel(as.factor(age), "65_74"),
         race = relevel(as.factor(race), "whitenh"),
         sex = relevel(as.factor(sex), "male"))

# create model matrix with fixed effects predictors of main model
matrix_trial <- model.matrix(~age+sex+race+perc_incpov150, data = acs_df_mod2)

# get df of fixed effects coefficients and SEs
df_coefs <- tibble(variable = rownames(coef(summary(mod_srhaci_full_yes))),
                   beta = coef(summary(mod_srhaci_full_yes))[,"Estimate"],
                   se = coef(summary(mod_srhaci_full_yes))[,"Std. Error"])

# vector of coefficients should equal the number of columns in model matrix
nrow(df_coefs) == ncol(matrix_trial)
#[1] TRUE

# get df of random effects coefficients
### GEOID
beta_GEOID <- 
  tibble(GEOID = rownames(ranef(mod_srhaci_full_yes)$GEOID),
         pred_GEOID = unlist(ranef(mod_srhaci_full_yes)$GEOID["(Intercept)"]))
### state name
beta_state_name <- 
  tibble(state_name = rownames(ranef(mod_srhaci_full_yes)$state_name),
         pred_state_name = unlist(ranef(mod_srhaci_full_yes)$state_name["(Intercept)"]))




# Monte Carlo simulation start

### initialize empty matrix for 1000 iterations
trial_matrix <- matrix(NA, nrow = nrow(acs_df_mod), ncol = 1000)

### start the simulation 
for(i in 1:1000) {
  
  ### generate random coefficient assuming N(mu = beta, sd = se)
  df_coefs <- df_coefs %>%
    rowwise() %>%
    mutate(final_beta = rnorm(1, beta, se)) %>%
    ungroup()
  
  ### extract vector of MC simulated coefficients
  vector_trial <- df_coefs$final_beta
  
  ### merge in the data
  trial1 <- acs_df_mod %>%
    mutate(pred = as.vector(matrix_trial%*%vector_trial)) %>% # matrix multiplication
    left_join(beta_GEOID, by = "GEOID") %>% # get coefficients from GEOID
    left_join(beta_state_name, by = "state_name") %>% # get coefficients from state
    mutate(pred_GEOID = ifelse(is.na(pred_GEOID), 0, pred_GEOID), # if outside of model, assign 0 for population-level effects
           pred_state_name = ifelse(is.na(pred_state_name), 0, pred_state_name)) %>% # if outside of model, assign 0 for population-level effects
    mutate(final_pred = pred+pred_GEOID+pred_state_name) # final log-odds should be the sum
  
  # get log-odds for every row
  trial_matrix[,i] <- trial1$final_pred
  
  if((i %% 100) == 0) print(paste0("MC Iteration: ", i, " of 1000"))
  
}

# get 50th, 2.5th and 97.5th percentile of log-odds 
summ_matrix <- trial_matrix %>% rowQuantiles(probs = c(0.5, 0.025, 0.975))

# convert to data frame
summ_tibble <- summ_matrix %>%
  as_tibble() %>%
  rename(q50 = 1, q2.5 = 2, q97.5 = 3)

# merge in log-odds
acs_df_mod_probs <- acs_df_mod %>%
  # get mean prediction from model
  mutate(logodds = predict(mod_srhaci_full_yes, 
                           newdata = acs_df_mod, 
                           allow.new.levels = TRUE)) %>%
  # get mean, 95% CI from Monte Carlo simulation
  cbind(summ_tibble) %>%
  as_tibble() %>%
  mutate(prob = exp(logodds)/(1+exp(logodds)),
         prob_q50 = exp(q50)/(1+exp(q50)),
         prob_q2.5 = exp(q2.5)/(1+exp(q2.5)),
         prob_q97.5 = exp(q97.5)/(1+exp(q97.5))) %>%
  mutate(p_pred = prob*count,
         p_pred_q50 = prob_q50*count,
         p_pred_q2.5 = prob_q2.5*count,
         p_pred_q97.5 = prob_q97.5*count) 

save(acs_df_mod_probs, file = "Data/02_generated/SAEs/prob_haci_full_yes.RData")






########### end of script









# check some descriptives

acs_df_mod_probs %>%
  #group_by(GEOID, state_name) %>%
  #group_by(race) %>%
  #group_by(age) %>%
  #group_by(sex) %>%
  summarise(p_pred = sum(p_pred),
            p_pred_q50 = sum(p_pred_q50),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q50 = round(p_pred_q50/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup() #%>%
#mutate(diff = prop - prop_q50) %>%
#filter(diff > 0.05)
#mutate(diff_ends = prop_q97.5 - prop_q2.5) %>%
#ggplot(aes(diff_ends)) + geom_histogram()





