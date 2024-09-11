# Filename: 02A_noise_model.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: December 18 2023

# Description: Model construction for Small Area Estimation for noise overexposure in Cohort One

# Study population: Cohort One (Nov 14, 2019 to Nov 14, 2022)

# Model outcome: noise overexposure (>= 70 dBA)




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
         age = relevel(age, "45_54")) # set 45 to 54 as reference

# get complete cases for data
df_trial_noise <- full_lancet_data2 %>%
  mutate(above_70 = ifelse(lex_24 >= 70, 1, 0)) %>%
  select(age, sex, race, perc_incpov150, state_name, GEOID, above_70) %>% 
  na.omit()

dim(df_trial_noise)
#[1] 113177      7

table(df_trial_noise$above_70)
#     0     1 
# 71143 42034 




# -------------------- 
# 2. Run the model
# --------------------




mod <- df_trial_noise %>%
  glmer(above_70 ~ age + sex + race + perc_incpov150 + (1|GEOID) + (1|state_name), family = "binomial", data = .)

#### save the model
#saveRDS(mod, file = "Models/mod_noise_70.rds")
#### read in the saved model
mod_noise_70 <- readRDS("/home/hdsuser/Papers/Lancet 2023 Analysis/Models/mod_noise_70.rds")

summary(mod_noise_70)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: above_70 ~ age + sex + race + perc_incpov150 + (1 | GEOID) +      (1 | state_name)
#Data: .

#     AIC      BIC   logLik deviance df.resid 
#145298.1 145461.9 -72632.0 145264.1   113160 

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-1.3425 -0.8000 -0.6335  1.1428  4.9153 

#Random effects:
#  Groups     Name        Variance Std.Dev.
#  GEOID      (Intercept) 0.03582  0.1893  
#  state_name (Intercept) 0.03089  0.1758  
#Number of obs: 113177, groups:  GEOID, 2562; state_name, 52

#Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -0.619009   0.044818 -13.812  < 2e-16 ***
#  age18_19         0.248199   0.039047   6.356 2.06e-10 ***
#  age20_24         0.205593   0.024390   8.429  < 2e-16 ***
#  age25_29         0.100902   0.022915   4.403 1.07e-05 ***
#  age30_34         0.141233   0.022182   6.367 1.93e-10 ***
#  age35_44         0.218285   0.019472  11.210  < 2e-16 ***
#  age55_64        -0.354722   0.027616 -12.845  < 2e-16 ***
#  age65_74        -0.953853   0.044343 -21.511  < 2e-16 ***
#  age75_84        -1.871098   0.112513 -16.630  < 2e-16 ***
#  sexfemale       -0.098701   0.013566  -7.276 3.44e-13 ***
#  raceasian       -0.442724   0.031312 -14.139  < 2e-16 ***
#  raceblackNH      0.496823   0.030616  16.228  < 2e-16 ***
#  racehispanic     0.317566   0.020490  15.499  < 2e-16 ***
#  raceother_multi  0.034743   0.030083   1.155 0.248118    
#  perc_incpov150   0.005384   0.001552   3.469 0.000522 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1










# -------------------- 
# 3. Small area estimation
# --------------------

# create dummy data frame of df for SAE
acs_df_mod2 <- acs_df_mod %>%
  mutate(age = relevel(as.factor(age), "45_54"),
         race = relevel(as.factor(race), "whitenh"),
         sex = relevel(as.factor(sex), "male"))

# create model matrix with fixed effects predictors of main model
matrix_trial <- model.matrix(~age+sex+race+perc_incpov150, data = acs_df_mod2)

# get df of fixed effects coefficients and SEs
df_coefs <- tibble(variable = rownames(coef(summary(mod_noise_70))),
                   beta = coef(summary(mod_noise_70))[,"Estimate"],
                   se = coef(summary(mod_noise_70))[,"Std. Error"])

# vector of coefficients should equal the number of columns in model matrix
nrow(df_coefs) == ncol(matrix_trial)
#[1] TRUE

# get df of random effects coefficients
### GEOID
beta_GEOID <- 
  tibble(GEOID = rownames(ranef(mod_noise_70)$GEOID),
         pred_GEOID = unlist(ranef(mod_noise_70)$GEOID["(Intercept)"]))
### state name
beta_state_name <- 
  tibble(state_name = rownames(ranef(mod_noise_70)$state_name),
         pred_state_name = unlist(ranef(mod_noise_70)$state_name["(Intercept)"]))




# Monte Carlo simulation start

### initialize emmpty matrix for 1000 iterations
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
  mutate(logodds = predict(mod_noise_70, 
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

save(acs_df_mod_probs, file = "Data/02_generated/SAEs/prob_noise_70.RData")






########### end of script









# check some descriptives

acs_df_mod_probs %>%
  #group_by(GEOID, state_name) %>%
  #group_by(race) %>%
  #group_by(age) %>%
  summarise(p_pred = sum(p_pred),
            p_pred_q50 = sum(p_pred_q50),
            p_pred_q2.5 = sum(p_pred_q2.5),
            p_pred_q97.5 = sum(p_pred_q97.5),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*100, 3),
         prop_q50 = round(p_pred_q50/n*100, 3),
         prop_q2.5 = round(p_pred_q2.5/n*100, 3),
         prop_q97.5 = round(p_pred_q97.5/n*100, 3),) %>%
  ungroup()





