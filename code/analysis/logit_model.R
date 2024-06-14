## Deforestation matching dataset

library(tidyverse)
library(tidysynth)
library(sf)
library(foreign)
library(lfe)
library(brm)
library(gsynth)
library(panelView)
library(modelsummary)
library(foreach)
library(doFuture)
library(future)
library(future.apply)
library(gtools)

## Load data ------------

kmr_list = list(CC = "Central_Coast",
           CST = "Central_Southern_Tablelands",
           DRP = "Darling_Riverine_Plains",
           FW = "Far_West",
           NC = "North_Coast",
           NT = "Northern_Tablelands",
           NS = "Northwest_Slopes",
           R = "Riverina",
           SC = "South_Coast")

#plan(multisession, workers = 4)
for (kmr in names(kmr_list)) {
kmr_abbr <- kmr
lots <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = kmr_list[[kmr]])
lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')
lots$lot_area <- st_area(lots)

slats_data <- read_csv(paste0("output/slats_prop/slats_prop_", kmr, ".csv"))
cov_data_files <- list.files("output/cov_prop_parallel", full.names = T, pattern = paste0('^cov_prop_', kmr))
cov_data <- future_lapply(cov_data_files, fread) %>% do.call(smartbind, .) %>% arrange(lot_id)
inside_current_sf <- read_csv(paste0("output/revoked_state_forests/lots_revoked_sf/lots_current_sf_", kmr, ".csv"))
inside_revoked_sf <- read_csv(paste0("output/revoked_state_forests/lots_revoked_sf/lots_inside_revoked_sf_", kmr, ".csv"))
buffer_revoked_sf <- read_csv(paste0("output/revoked_state_forests/lots_revoked_sf/lots_buffer_revoked_sf_", kmr, ".csv"))
revoked_sf_np <- st_read("output/revoked_state_forests/revoked_sf_np.shp")
revoked_sf_np_buffer <- st_read("output/revoked_state_forests/revoked_sf_np_grouped_buffer.shp")
#lots_data <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = kmr_list[[kmr]])

## Data cleaning and refactoring -------------

#cov_data$lot_id <- paste0(kmr, "_", 1:nrow(cov_data))

# Fire is NA means no fire history
cov_data$fire[is.na(cov_data$fire)] <- 0

# Recategorise forest code and tenure as classes were lost after exactextractr
forest_code_lookup <- read.dbf("../risk-model-covariates/data/forest_tenure/aus_forten18_geotiff/aus_forten18_geotiff/aus_forten18.tif.vat.dbf")
cov_data$forest_code <- forest_code_lookup[cov_data$forest_code,'FOR_CODE']
cov_data$forest_tenure <- forest_code_lookup[cov_data$forest_tenure,'FOR_TEN']
cov_data$forest_tenure_type <- forest_code_lookup[cov_data$forest_tenure_type,'FOR_TYPE']

cov_data$lot_area <- NULL

cov_data_reshaped <- reshape_df(cov_data)

sf_all <- fread('output/state_forests/state_forests_all.csv')

buffer_revoked_sf <- buffer_revoked_sf %>%
  filter(!(lot_id %in% inside_current_sf$lot_id)) %>%
  filter(!(lot_id %in% inside_revoked_sf$lot_id)) %>%
  left_join(sf_all, by = 'group_idx') %>%
  group_by(lot_id) %>%
  summarise(revocation_year = min(revocation_year))

## Fit preliminary linear model ------
model_data <- slats_data %>%
  filter(!(lot_id %in% inside_current_sf$lot_id)) %>%
  filter(!(lot_id %in% inside_revoked_sf$lot_id)) %>%
  mutate(year = substr(lyr, 7, 10)) %>%
  left_join(cov_data_reshaped, by = c('lot_id', 'year')) %>%
  left_join(lots, by = 'lot_id') %>%
  mutate(total_deforestation = forestry) %>%
  mutate(any_deforestation = total_deforestation > 0) %>%
  filter(woody_nsw > 0) %>%
  mutate(deforestation_proportion = total_deforestation / (lot_area*woody_nsw)) %>%
  left_join(buffer_revoked_sf, by = 'lot_id') %>%
  mutate(revoked = revocation_year < year) %>%
  mutate(forest_tenure = ifelse(is.na(forest_tenure), 99, forest_tenure))
#model_data$deforestation_pct[model_data$deforestation_pct > 1] <- 1

model_logit <- felm(as.numeric(deforestation_proportion) ~ revoked + temp + prec | year + lot_id, data = model_data)

summary(model_logit)

readr::write_rds(model_logit, paste0("output/logit_model/logit_model_", kmr, ".rds"))
}


reshape_df <- function(df) {
  df <- df %>%
    select(-prec, -temp)
  df_prec <- df %>%
    pivot_longer(cols = starts_with("prec_"), names_to = 'year', names_prefix = 'prec_',
                 values_to = 'prec') %>%
    select(lot_id, year, prec)
  df_temp <- df %>%
    pivot_longer(cols = starts_with("temp_"), names_to = 'year', names_prefix = 'temp_',
                 values_to = 'temp') %>%
    select(lot_id, year, temp)
  df_woody <- df %>%
    pivot_longer(cols = starts_with("woody_veg_"), names_to = 'year', names_prefix = 'woody_veg_',
                 values_to = 'woody_veg') %>%
    select(lot_id, year, woody_veg)
  df_comb <- reduce(list(df_prec, df_temp, df_woody), left_join, by = c('lot_id', 'year'))
  
  df_out <- df %>%
    select(-starts_with("prec"), -starts_with("temp"), -starts_with("woody_veg")) %>%
    right_join(df_comb, by = 'lot_id')
  return(df_out)
}
