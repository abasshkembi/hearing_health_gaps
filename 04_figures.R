# Filename: 04_figures.R
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: March 15 2024

# Description: County-level estimation of four main outcomes for paper figures


library(tidyverse)
library(sf)
library(biscale)
library(cowplot)


# -------------------- 
# Figure 2 - bivariate map of age-adjusted rates of noise overexposure 
#            and poor/fair hearing ability
# --------------------

### read in county shapefile
county_shp <- st_read("/home/hdsuser/Papers/Lancet 2023 Analysis/Data/01_imported/county_shapefiles_transformed/counties_shp_transformed.shp")
### read in noise estimates
load("Data/02_generated/SAEs/prob_noise_70.RData")
noise_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

# create natiownide weights to create age-adjusted estiamtes
age_weights <- noise_sae %>%
  group_by(age) %>%
  summarise(total_pop = sum(count)) %>%
  ungroup() %>%
  mutate(weight = total_pop/sum(total_pop))

# create age adjusted rates per 1,000 people
trial_sae_noise_ageAdj <- 
  noise_sae %>%
  group_by(GEOID, state_name, age) %>%
  summarise(p_pred = sum(p_pred),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*1000, 3)) %>%
  ungroup() %>%
  left_join(age_weights, by = "age") %>%
  mutate(prop = ifelse(n == 0, 0, prop)) %>%
  mutate(age_prop = prop*weight) %>%
  group_by(GEOID, state_name) %>%
  summarise(age_rate = sum(age_prop)) %>%
  ungroup()



# read in the poor/fair hearing ability
load("Data/02_generated/SAEs/prob_srHL.RData")
srhl_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))


# create age adjusted rate per 1,000 people
trial_sae_srHL_ageAdj <- 
  srhl_sae %>%
  group_by(GEOID, state_name, age) %>%
  summarise(p_pred = sum(p_pred),
            n = sum(count)) %>%
  mutate(prop = round(p_pred/n*1000, 3)) %>%
  ungroup() %>%
  left_join(age_weights, by = "age") %>%
  mutate(prop = ifelse(n == 0, 0, prop)) %>%
  mutate(age_prop = prop*weight) %>%
  group_by(GEOID, state_name) %>%
  summarise(age_rate = sum(age_prop)) %>%
  ungroup()

trial_sae_srHL_ageAdj %>% filter(is.na(age_rate))
# no missing




# combine the noise and hearing ability data together

biscale_map_ageAdj <- trial_sae_noise_ageAdj %>% rename(age_rate_noise = age_rate) %>%
  left_join(trial_sae_srHL_ageAdj %>% rename(age_rate_srHL = age_rate), 
            by = c("GEOID", "state_name")) %>%
  biscale::bi_class(x = age_rate_srHL, y = age_rate_noise,
                    style = "quantile", dim = 4)

breaks_biscale <- bi_class_breaks(biscale_map_ageAdj, x = age_rate_srHL, y = age_rate_noise, 
                                  style = "quantile", 
                                  dim = 4, dig_lab = c(x = 3, y = 3), split = TRUE)

biscale_map_ageAdj_sf <- county_shp %>%
  st_transform("EPSG:3082") %>%
  left_join(biscale_map_ageAdj, by = "GEOID") %>%
  filter(!(STATEFP %in% c("60", "66", "69", "78")))


map <- ggplot() +
  geom_sf(data = biscale_map_ageAdj_sf, mapping = aes(fill = bi_class),
          color = NA, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet2", dim = 4) +
  #bi_theme(base_family = "Montserrat") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Noise overexposure and poor/fair hearing ability",
       subtitle = "Age-adjusted rates per 1,000 US adults")

legend <- bi_legend(pal = "DkViolet2",
                    dim = 4,
                    xlab = "Poor/Fair Hearing",
                    ylab = "Noise Overexposure",
                    breaks = breaks_biscale,
                    arrows = FALSE,
                    size = 7)

# combine map with legend
fig1_final <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, 0, 0.25, 0.25)

fig1_final





# -------------------- 
# Figure 3 - crude prevalence of those without a hearing loss diagnosis
#            among those with poor/fair hearing ability
# --------------------

# crude poor/fair hearing ability estimates
trial_sae_srHL <- srhl_sae %>%
  group_by(GEOID, state_name) %>%
  summarise(p_pred = sum(p_pred),
            n = sum(count)) %>%
  mutate(crude_rate = round(p_pred/n*1000, 3),
         prop = round(p_pred/n*100, 3)) %>%
  ungroup()

load("Data/02_generated/SAEs/prob_srDx.RData")
srdx_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

# crude hearing loss diagnosis estimates
trial_sae_srDx <- srdx_sae %>%
  group_by(GEOID, state_name) %>%
  summarise(p_pred = sum(p_pred),
            n = sum(count)) %>%
  mutate(crude_rate = round(p_pred/n*1000, 3),
         prop = round(p_pred/n*100, 3)) %>%
  ungroup()

# estimate prevalence of individuals w/o hearing loss Dx
# among those with a poor/fair hearing ability
trial_sae_HLdiff <- trial_sae_srDx %>% dplyr::select(GEOID, state_name, n, p_pred) %>% rename(p_pred_Dx = p_pred) %>%
  left_join(trial_sae_srHL %>% rename(p_pred_HL = p_pred), by = c("GEOID", "state_name", "n")) %>%
  mutate(diff = round((p_pred_HL-p_pred_Dx)/p_pred_HL*100, 3))

trial_sae_HLdiff %>% #.$age_rate %>% summary()
  ggplot(aes(diff)) + geom_histogram()


pale <- rev(c('#F9F9F7', "#F0ECE9", "#E7DFD8", '#E1D5B1',
              '#BE9543', '#6F4115', "#251607"))






trial_sae_srHL$prop %>% summary()

county_shp %>%
  st_transform("EPSG:3082") %>%
  left_join(trial_sae_HLdiff, by = "GEOID") %>%
  filter(!(STATEFP %in% c("60", "66", "69", "78"))) %>% #.$STATEFP %>% unique %>% sort()
  mutate(col_breaks = cut(diff, breaks = c(25, 40, 42, 44, 46, 48, 50, 70),
                          labels = c("25-40%", "40-42%", "42-44%", "44-46%", "46-48%", "48-50%", "50-70%")
  )) %>%
  ggplot(aes(fill = fct_rev(col_breaks)), color = "grey") +
  geom_sf(linewidth = 0.03) +
  scale_fill_manual(values = pale,
                    name = "Estimated\nPrevalence (%)", 
                    #labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                    na.value = "#f0f0f0") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Prevalence of no hearing loss diagnosis",
       subtitle = "Among adults with poor/fair hearing ability")








# -------------------- 
# Figure 4 - crude prevalence of those without hearing aids/cochlear implants
#            among those with a diagnosed hearing loss
# --------------------


# load in hearing aid/cochlear implant use estimtaes
load("Data/02_generated/SAEs/prob_haci_full_yes.RData")
haci_sae <- acs_df_mod_probs %>%
  mutate(GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
         NAME = ifelse(GEOID %in% c("02261"), "Valdez-Cordova", NAME))

# create crude county-level estimates
trial_sae_haci <- haci_sae %>%
  group_by(GEOID, state_name) %>%
  summarise(p_pred = sum(p_pred),
            n = sum(count)) %>%
  mutate(crude_rate = round(p_pred/n*1000, 3),
         prop = round(p_pred/n*100, 3)) %>%
  ungroup()


trial_sae_hacidiff <- trial_sae_haci %>% dplyr::select(GEOID, state_name, n, p_pred) %>% rename(p_pred_haci = p_pred) %>%
  left_join(trial_sae_srDx %>% rename(p_pred_Dx = p_pred), by = c("GEOID", "state_name", "n")) %>%
  mutate(diff = round((p_pred_Dx - p_pred_haci)/p_pred_Dx*100, 3))

trial_sae_hacidiff %>% #.$age_rate %>% summary()
  ggplot(aes(diff)) + geom_histogram()



pale <- rev(c('#F9F9F7', "#fcf9e8", "#F1DFC3", '#D5A38A',
              '#B8755A', '#8B2D1B', "#6F1811"))

county_shp %>%
  st_transform("EPSG:3082") %>%
  left_join(trial_sae_hacidiff, by = "GEOID") %>%
  filter(!(STATEFP %in% c("60", "66", "69", "78"))) %>% #.$STATEFP %>% unique %>% sort()
  mutate(col_breaks = cut(diff, breaks = c(20, 40, 42, 44, 46, 48, 50, 70),
                          labels = c("20-40%", "40-42%", "42-44%", "44-46%", "46-48%", "48-50%", "50-70%")
  )) %>%
  ggplot(aes(fill = fct_rev(col_breaks)), color = "grey") +
  geom_sf(linewidth = 0.03) +
  scale_fill_manual(values = pale,
                    name = "Estimated\nPrevalence (%)", 
                    #labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                    na.value = "#f0f0f0") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Prevalence of no hearing aid/cochlear implant use",
       subtitle = "Among adults with a diagnosed hearing loss")




















# Supplemental Figure S1 - crude noise by county

### get county level estimates - crude
trial_sae_noise <- noise_sae %>%
  group_by(GEOID, state_name) %>%
  summarise(p_pred = sum(p_pred),
            n = sum(count)) %>%
  mutate(crude_rate = round(p_pred/n*1000, 3),
         prop = round(p_pred/n*100, 3)) %>%
  ungroup()


trial_sae_noise$prop %>% summary()

pale <- rev(c('white', "grey96", '#DBE5F9',
              '#a6bddb','#74a9cf','#0570b0', '#034e7b'))


county_shp %>%
  st_transform("EPSG:3082") %>%
  left_join(trial_sae_noise, by = "GEOID") %>%
  #filter(!(STATEFP %in% c("02", "15", "72", "60", "66", "69", "78"))) %>% #.$STATEFP %>% unique %>% sort()
  filter(!(STATEFP %in% c("60", "66", "69", "78"))) %>% #.$STATEFP %>% unique %>% sort()
  mutate(col_breaks = cut(prop, breaks = c(18, 30, 32, 34, 36, 38, 40, 51),
                          labels = c("18-30%", "30-32%", "32-34%", "34-36%", "36-38%", "38-40%", "40-51%"))) %>%
  ggplot(aes(fill = fct_rev(col_breaks))) +
  geom_sf(linewidth = 0.05) +
  scale_fill_manual(values = pale,
                    name = "Noise\nOverexposure\nPrevalence (%)", 
                    #labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                    na.value = "#f0f0f0") +
  theme_void()







# Supplemental Table S2 - crude poor/fair hearing ability by county

pale <- rev(c('white', "#FFF8F4", '#fee5d9','#fcbba1',
              '#fc9272','#cb181d', '#99000d'))


trial_sae_srHL$prop %>% summary()

# supplemental figure 2 - crude prevalence of poor/fair hearing ability by county
county_shp %>%
  st_transform("EPSG:3082") %>%
  left_join(trial_sae_srHL, by = "GEOID") %>%
  filter(!(STATEFP %in% c("60", "66", "69", "78"))) %>% #.$STATEFP %>% unique %>% sort()
  mutate(col_breaks = cut(prop, breaks = c(18, 25, 27, 29, 31, 33, 35, 42),
                          labels = c("18-25%", "25-27%", "27-29%", "29-31%", "31-33%", "33-35%", "35-42%"))) %>%
  ggplot(aes(fill = fct_rev(col_breaks))) +
  geom_sf(linewidth = 0.05) +
  scale_fill_manual(values = pale,
                    name = "Poor/Fair\nHearing Ability\nPrevalence (%)", 
                    #labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                    na.value = "#f0f0f0") +
  theme_void()



