# Filename: 02C_hearingLossDx_model.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: December 18 2023

# Description: Model construction for Small Area Estimation for diagnosed hearing loss in Cohort One

# Study population: Cohort One (Nov 14, 2019 to Nov 14, 2022)

# Model outcome: Hearing loss diagnosis




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
table(full_lancet_data$hearing_loss_diagnosed_q)

#[dontKnow]       [no]     [ptna]      [yes] 
#      2768     111266        193      12479 

df_trial_dx <- full_lancet_data2 %>%
  mutate(sr_dx = ifelse(hearing_loss_diagnosed_q %in% c("[yes]"), 1, 0)) %>%
  filter(!(hearing_loss_diagnosed_q %in% c("[dontKnow]", "[ptna]"))) %>%
  mutate(loc_q_cat = factor(loc_q_cat, levels = c("High", "Low", "Unknown"))) %>%
  select(age, sex, race, perc_incpov150, loc_q_cat, state_name, GEOID, sr_dx) %>% 
  na.omit()

dim(df_trial_dx)
#[1] 160896      8

table(df_trial_dx$sr_dx)
#      0      1 
# 148417  12479 




# -------------------- 
# 2. Run the model
# --------------------




mod_srdx <- df_trial_dx %>%
  glmer(sr_dx ~ age + sex + race + perc_incpov150 + (1|GEOID) + (1|state_name), family = "binomial", data = .)

#### save the model
#saveRDS(mod_srdx, file = "Models/mod_srDx.rds")
#### read in the saved model
mod_srdx <- readRDS("/home/hdsuser/Papers/Lancet 2023 Analysis/Models/mod_srDx.rds")

summary(mod_srdx)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: sr_dx ~ age + sex + race + perc_incpov150 + (1 | GEOID) + (1 |      state_name)
#Data: .

#       AIC      BIC   logLik deviance df.resid 
#   79316.7  79486.5 -39641.4  79282.7   160879 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.1120 -0.2893 -0.2232 -0.1777  8.6900 

#Random effects:
#  Groups     Name        Variance Std.Dev.
#GEOID      (Intercept) 0.026685 0.1634  
#state_name (Intercept) 0.007499 0.0866  
#Number of obs: 160896, groups:  GEOID, 2779; state_name, 52

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -2.089831   0.046322 -45.116  < 2e-16 ***
#  age18_19        -1.432790   0.079056 -18.124  < 2e-16 ***
#  age20_24        -1.330629   0.046431 -28.658  < 2e-16 ***
#  age25_29        -1.239164   0.043201 -28.684  < 2e-16 ***
#  age30_34        -0.904627   0.037568 -24.080  < 2e-16 ***
#  age35_44        -0.497571   0.028948 -17.188  < 2e-16 ***
#  age55_64         0.648232   0.031176  20.793  < 2e-16 ***
#  age65_74         1.266059   0.037112  34.115  < 2e-16 ***
#  age75_84         1.957957   0.055910  35.020  < 2e-16 ***
#  sexfemale       -0.116589   0.020627  -5.652 1.58e-08 ***
#  raceasian       -0.530746   0.058656  -9.048  < 2e-16 ***
#  raceblackNH     -0.473425   0.056052  -8.446  < 2e-16 ***
#  racehispanic    -0.215082   0.036489  -5.894 3.76e-09 ***
#  raceother_multi -0.022517   0.046578  -0.483   0.6288    
#  perc_incpov150   0.004049   0.001850   2.188   0.0286 *  
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
df_coefs <- tibble(variable = rownames(coef(summary(mod_srdx))),
                   beta = coef(summary(mod_srdx))[,"Estimate"],
                   se = coef(summary(mod_srdx))[,"Std. Error"])

# vector of coefficients should equal the number of columns in model matrix
nrow(df_coefs) == ncol(matrix_trial)
#[1] TRUE

# get df of random effects coefficients
### GEOID
beta_GEOID <- 
  tibble(GEOID = rownames(ranef(mod_srdx)$GEOID),
         pred_GEOID = unlist(ranef(mod_srdx)$GEOID["(Intercept)"]))
### state name
beta_state_name <- 
  tibble(state_name = rownames(ranef(mod_srdx)$state_name),
         pred_state_name = unlist(ranef(mod_srdx)$state_name["(Intercept)"]))




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
  mutate(logodds = predict(mod_srdx, 
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

save(acs_df_mod_probs, file = "Data/02_generated/SAEs/prob_srDx.RData")






########### end of script









# check some descriptives

acs_df_mod_probs %>%
  #group_by(GEOID, state_name) %>%
  group_by(race) %>%
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





