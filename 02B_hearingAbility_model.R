# Filename: 02B_hearingAbility_model.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: December 18 2023

# Description: Model construction for Small Area Estimation for poor/fair hearing ability in Cohort One

# Study population: Cohort One (Nov 14, 2019 to Nov 14, 2022)

# Model outcome: Poor/fair hearing ability




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
table(full_lancet_data$hearing_ability_rate_q)

#[excellent]      [fair]      [good]      [poor]      [ptna]  [veryGood] 
#      26018       22519       43954        6233         175       49328 

df_trial_hl <- full_lancet_data2 %>%
  mutate(sr_hl = ifelse(hearing_ability_rate_q %in% c("[fair]", "[poor]"), 1, 0)) %>%
  filter(hearing_ability_rate_q != "[ptna]") %>%
  select(age, sex, race, perc_incpov150, state_name, GEOID, sr_hl) %>% 
  na.omit()

dim(df_trial_hl)
#[1] 148052      7

table(df_trial_hl$sr_hl)
#      0      1 
# 119300  28752 




# -------------------- 
# 2. Run the model
# --------------------




mod_srhl <- df_trial_hl %>%
  glmer(sr_hl ~ age + sex + race + perc_incpov150 + (1|GEOID) + (1|state_name), family = "binomial", data = .)

#### save the model
#saveRDS(mod_srhl, file = "Models/mod_srHL.rds")
#### read in the saved model
mod_srhl <- readRDS("/home/hdsuser/Papers/Lancet 2023 Analysis/Models/mod_srHL.rds")

summary(mod_srhl)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: sr_hl ~ age + sex + race + perc_incpov150 + (1 | GEOID) + (1 |      state_name)
#Data: .

#      AIC      BIC   logLik deviance df.resid 
# 141186.2 141354.6 -70576.1 141152.2   148035 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.4004 -0.4926 -0.4319 -0.3689  3.6126 

#Random effects:
#  Groups     Name        Variance Std.Dev.
#GEOID      (Intercept) 0.015623 0.12499 
#state_name (Intercept) 0.007429 0.08619 
#Number of obs: 148052, groups:  GEOID, 2742; state_name, 52

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -1.257932   0.035083 -35.856  < 2e-16 ***
#  age18_19        -0.647158   0.039942 -16.202  < 2e-16 ***
#  age20_24        -0.616527   0.025917 -23.789  < 2e-16 ***
#  age25_29        -0.644480   0.025333 -25.440  < 2e-16 ***
#  age30_34        -0.563738   0.024371 -23.132  < 2e-16 ***
#  age35_44        -0.353430   0.020581 -17.172  < 2e-16 ***
#  age55_64         0.286714   0.025618  11.192  < 2e-16 ***
#  age65_74         0.686393   0.033618  20.418  < 2e-16 ***
#  age75_84         1.376116   0.057345  23.997  < 2e-16 ***
#  sexfemale        0.080424   0.013981   5.753 8.79e-09 ***
#  raceasian       -0.302476   0.035380  -8.549  < 2e-16 ***
#  raceblackNH     -0.481039   0.037989 -12.663  < 2e-16 ***
#  racehispanic    -0.124950   0.023241  -5.376 7.61e-08 ***
#  raceother_multi -0.099480   0.032047  -3.104  0.00191 ** 
#  perc_incpov150   0.009343   0.001333   7.008 2.41e-12 ***
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
df_coefs <- tibble(variable = rownames(coef(summary(mod_srhl))),
                   beta = coef(summary(mod_srhl))[,"Estimate"],
                   se = coef(summary(mod_srhl))[,"Std. Error"])

# vector of coefficients should equal the number of columns in model matrix
nrow(df_coefs) == ncol(matrix_trial)
#[1] TRUE

# get df of random effects coefficients
### GEOID
beta_GEOID <- 
  tibble(GEOID = rownames(ranef(mod_srhl)$GEOID),
         pred_GEOID = unlist(ranef(mod_srhl)$GEOID["(Intercept)"]))
### state name
beta_state_name <- 
  tibble(state_name = rownames(ranef(mod_srhl)$state_name),
         pred_state_name = unlist(ranef(mod_srhl)$state_name["(Intercept)"]))




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
  mutate(logodds = predict(mod_srhl, 
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

save(acs_df_mod_probs, file = "Data/02_generated/SAEs/prob_srHL.RData")






########### end of script









# check some descriptives

acs_df_mod_probs %>%
  #group_by(GEOID, state_name) %>%
  #group_by(race) %>%
  #group_by(age) %>%
  group_by(sex) %>%
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





