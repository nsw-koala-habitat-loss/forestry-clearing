# Matching algorithm

library(tidyverse)
library(MatchIt)
library(data.table)
library(future)
library(future.apply)
library(foreign)
library(sjmisc)
library(arrow)
library(INLA)
library(pscl)
library(sf)
library(lfe)

read_from_file <- T

match_coef_plt <- function(match_model) {
  coefs <- match_model$summary.fixed 
  coefs$coef <- rownames(coefs)
  coef_plot <- coefs %>%
    #pivot_longer(cols = c('mean', '0.025quant', '0.975quant')) %>%
    ggplot(aes(y = mean, x = coef)) +
    geom_linerange(aes(ymin = `0.025quant`, ymax = `0.975quant`), color = 'gray20', linewidth = 1) +
    geom_hline(yintercept = 0) +
    geom_point(aes(y = mean), size = 2) +
    ggpubr::theme_pubr() +
    coord_flip() +
    ggtitle("Coefficient estimates")
  return(coef_plot)
}

## Matching between revoked and current state forests to estimate the reduction in clearing after revocation of state forests -------------------

file_list <- list.files('output/cov_prop_parallel', full.names = T)
cov_prop <- arrow::open_csv_dataset(file_list, skip_empty_rows = T)
lots_sf_association <- fread('output/model_data/lots_sf_association.csv')

cov_prop_filter <- cov_prop %>%
  select(lot_id, lot_area, elev, income, pop_den, prec, prop_size, prop_value, slope, temp, drought, fire, forest_code, forest_tenure_type, forest_tenure, landuse, remoteness, soil_fert, soil_nitrogen, soil_type, ecol_cond, woody_nsw) %>%
  filter((woody_nsw*lot_area) > 1) %>%
  left_join(lots_sf_association, by = 'lot_id') %>%
  mutate(current_sf = CurrentTen == 'FCNSW', revoked_sf = !is.na(revocation_year)) %>%
  filter(current_sf | revoked_sf) %>%
  mutate(fire = ifelse(is.na(fire), 0, fire)) %>%
  mutate(forest_code = ifelse(is.na(forest_code), 0, forest_code)) %>%
  select(-woody_nsw)

cov_prop_filter_revoked_sf <- cov_prop_filter
reclass_forest_vars <- function(df) {
  forest_code_lookup <- read.dbf("../risk-model-covariates/data/forest_tenure/aus_forten18_geotiff/aus_forten18_geotiff/aus_forten18.tif.vat.dbf")
  lookup_codes <- df$forest_code
  df$forest_code <-   forest_code_lookup[match(lookup_codes, forest_code_lookup$VALUE),'FOR_CODE']
  df$forest_tenure <- forest_code_lookup[match(lookup_codes, forest_code_lookup$VALUE),'FOR_TEN']
  df$forest_tenure_type <- forest_code_lookup[match(lookup_codes, forest_code_lookup$VALUE),'TEN_TYPE']
  df$forest_type <- forest_code_lookup[match(lookup_codes, forest_code_lookup$VALUE),'FOR_TYPE']
  df$forest_category <- forest_code_lookup[match(lookup_codes, forest_code_lookup$VALUE),'FOR_CAT']
  df <- df %>%
    mutate(forest_code = ifelse(is.na(forest_code), "None", forest_code),
           forest_tenure = ifelse(is.na(forest_tenure), "None", forest_tenure),
           forest_tenure_type = ifelse(is.na(forest_tenure_type), "None", forest_tenure_type))
   dummy_df <- to_dummy(df, forest_tenure, suffix = 'label')
   df <- bind_cols(df, dummy_df)
  return(df)
}

cov_prop_revoked_sf_df <- collect(cov_prop_filter_revoked_sf)
cov_prop_revoked_sf_out <- reclass_forest_vars(cov_prop_revoked_sf_df)
unmatched_data <- filter(cov_prop_revoked_sf_out, !is.na(soil_nitrogen))

readr::write_rds(unmatched_data, paste0('output/match_data/unmatched_data.rds'))

# Conduct matching
m.out <- matchit(revoked_sf ~ lot_area + ecol_cond + elev + income + pop_den + prec + temp + drought + slope + as.character(fire) + as.character(remoteness) + forest_type + forest_category + dist_tpf, 
                 data = unmatched_data, replace = F)
summary(m.out)

plot(summary(m.out))

# Get matching data and obtain indices for matching
m.data <- match.data(m.out)
treated_group <- unmatched_data[as.numeric(rownames(m.out$match.matrix)), 'lot_id']
control_group <- unmatched_data[as.numeric(m.out$match.matrix), 'lot_id']

m.data$matches <- NA
m.data$matches[match(treated_group$lot_id, m.data$lot_id)] <- control_group$lot_id
m.data$matches[match(control_group$lot_id, m.data$lot_id)] <- treated_group$lot_id
m.data$revocation_year[match(control_group$lot_id, m.data$lot_id)] <- m.data$revocation_year[match(treated_group$lot_id, m.data$lot_id)]
m.data$pair_id <- NA
for (i in 1:nrow(m.data)) {
  m.data$pair_id[match(treated_group$lot_id, m.data$lot_id)] <- i
  m.data$pair_id[match(control_group$lot_id, m.data$lot_id)] <- i
}

# Load SLATS data
slats_data <- arrow::open_csv_dataset("output/slats_prop")
slats_data_match <- slats_data %>%
  filter(lot_id %in% m.data$lot_id) %>%
  mutate(year = substr(lyr, 7, 10) %>% as.numeric()) %>%
  mutate(end_year = substr(lyr, 12, 15) %>% as.numeric()) %>%
  mutate(annual_deforestation = forestry / (end_year - year)) %>%
  collect()

match_data <- slats_data_match %>%
  left_join(m.data, by = 'lot_id') %>%
  mutate(annual_deforestation_cells = round(annual_deforestation / 625)) %>% # 25m x 25m cells, 625m2 area
  mutate(t = year - revocation_year) %>%
  mutate(BA = ifelse(year > revocation_year, 1, 0)) %>%
  mutate(CI = ifelse(revoked_sf, 1, 0))

# BACI estimation
readr::write_rds(match_data, paste0('output/match_data/match_data.rds'))
match_model <- inla(annual_deforestation_cells ~ 1 + BA + CI + BA*CI + BA*t + CI*t + BA*CI*t + f(lot_id, model = 'iid'), data=match_data, family = 'nbinomial')

summary(match_model)

match_coef_plt(match_model) %>%
  ggsave("plots/match_model_coef.png", .)

plt <- match_data %>%
  mutate(baci_id = paste0(BA, "-", CI)) %>%
  ggplot(aes(x = t, y = annual_deforestation_cells, 
             color = as.factor(baci_id))) +
  #geom_point() +
  geom_smooth(method = 'lm', se = F) +
  ggpubr::theme_pubr()

plt %>%
  ggsave("plots/baci.png", .)

readr::write_rds(match_model, paste0('output/matching_estimates/matching_zinb.rds'))

## Model for private native forestry ---------------
if (!read_from_file) {
  file_list <- list.files('output/cov_prop_parallel', full.names = T)
  #file_list <- file_list[1:length(file_list) %% 50 == 0]
  cov_prop <- arrow::open_csv_dataset(file_list, skip_empty_rows = T)
  
  timber_processing_facilities <- st_read("https://spatial.industry.nsw.gov.au/arcgis/rest/services/PUBLIC/Forestry_NSW_Wood_Processing_Facilities/MapServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    st_transform(3308)
  
  lots_xy <- arrow::open_csv_dataset('output/lots_xy') %>% collect()
  lots_xy_sf <- st_as_sf(lots_xy, coords = c('X', 'Y'), crs = 3308)
  timber_supply_zones <- st_read("data/TimberSupplyZones/TimberSZ_LL.shp") %>%
    st_transform(3308)
  lots_tsz <- st_join(lots_xy_sf, timber_supply_zones)
  lots_tsz_df <- lots_tsz %>%
    st_drop_geometry() %>%
    select(lot_id, TSZName)
  
  nearest_timber_processing_facility <- sf::st_nearest_feature(lots_xy_sf, timber_processing_facilities)
  dist_timber_processing_facility <- st_distance(lots_xy_sf, timber_processing_facilities[nearest_timber_processing_facility,], by_element = T)
  dist_timber_processing_df <- data.frame(lot_id = lots_xy_sf$lot_id, dist_tpf = dist_timber_processing_facility)
  fwrite(dist_timber_processing_df, "data/r_data/dist_timber_processing_df.csv")
  
  cov_prop_pnf <- cov_prop %>%
    select(lot_id, lot_area, elev, income, pop_den, prec, prop_size, prop_value, slope, temp, drought, fire, forest_code, forest_tenure_type, forest_tenure, landuse, remoteness, soil_fert, soil_nitrogen, soil_type, ecol_cond, woody_nsw) %>%
    filter((woody_nsw*lot_area) > 1) %>%
    left_join(lots_sf_association, by = 'lot_id') %>%
    mutate(current_sf = CurrentTen == 'FCNSW', revoked_sf = !is.na(revocation_year)) %>%
    mutate(current_sf = ifelse(is.na(current_sf), FALSE, current_sf)) %>%
    filter(!current_sf & !revoked_sf) %>% # Not current or revoked state forest
    mutate(fire = ifelse(is.na(fire), 0, fire)) %>%
    mutate(forest_code = ifelse(is.na(forest_code), 0, forest_code)) %>%
    left_join(lots_tsz_df, by = 'lot_id') %>%
    left_join(dist_timber_processing_df, by = 'lot_id')
  
  cov_pnf_out <- collect(cov_prop_pnf)
  cov_pnf <- reclass_forest_vars(cov_pnf_out)
  unmatched_data <- cov_pnf[,c('lot_id', 'treated', 'lot_area' , 'ecol_cond','elev', 'income','pop_den', 'prec','temp','drought','slope','fire','remoteness','forest_category', 'forest_type', "dist_tpf", "woody_nsw", "TSZName")]
  unmatched_data <- unmatched_data[complete.cases(unmatched_data),]
  fwrite(unmatched_data, 'output/match_data/match_data_pnf.csv')

} else {
  unmatched_data <- fread('output/match_data/match_data_pnf.csv')
}

# Load SLATS data
slats_data <- arrow::open_csv_dataset("output/slats_prop")

# Find lot id of properties with some history of deforestation
lots_with_deforestation <- slats_data %>%
  group_by(lot_id) %>%
  summarise(forestry = sum(forestry)) %>%
  filter(forestry > 0) %>%
  collect()

lots_pretreat_deforestation <- slats_data %>%
  mutate(year = substr(lyr, 7, 10) %>% as.numeric()) %>%
  mutate(end_year = substr(lyr, 12, 15) %>% as.numeric()) %>%
  mutate(annual_deforestation = forestry / (end_year - year))  %>%
  filter(year < 2005) %>%
  group_by(lot_id) %>%
  summarise(pretreat_deforestation = sum(annual_deforestation)) %>%
  collect()

# Conduct matching ---------------
set.seed(2059943085)
revoked_tsz_list <- revoked_state_forests_tsz %>%
  group_by(TSZName) %>%
  filter(!is.na(ha_revoked_tsz)) %>%
  filter(ha_revoked_tsz > 100) %>%
  summarise(max_year = max(year, na.rm=T))



for (i in 1:nrow(revoked_tsz_list)) {
  treatment_year <- revoked_tsz_list[i,"max_year"][[1]]
  tsz_revocations <- revoked_tsz_list[i,"TSZName"][[1]]
  include_tsz <- revoked_tsz_list[revoked_tsz_list$max_year <= (treatment_year-2) & revoked_tsz_list$TSZName != tsz_revocations,"TSZName"][[1]]
  if (is.na(treatment_year) | length(include_tsz) == 0) {
    next
  }
  print(paste(tsz_revocations,"|", treatment_year, "|",  do.call(paste, as.list(include_tsz))))
}

for (i in 1:nrow(revoked_tsz_list)) {
  treatment_year <- revoked_tsz_list[i,"max_year"][[1]]
  tsz_revocations <- revoked_tsz_list[i,"TSZName"][[1]]
  include_tsz <- revoked_tsz_list[revoked_tsz_list$max_year <= (treatment_year-2) & revoked_tsz_list$TSZName != tsz_revocations,"TSZName"][[1]]
  if (is.na(treatment_year) | length(include_tsz) == 0) {
    next
  }
  tsz_i <- revoked_tsz_list[i,"TSZName"][[1]]
  
  unmatched_data_i <- unmatched_data %>%
    left_join(lots_pretreat_deforestation, by = 'lot_id') %>%
    mutate(treated = ifelse(TSZName %in% tsz_i, 1, 0))
  treated_data <- unmatched_data_i %>%
    filter(lot_id %in% lots_with_deforestation$lot_id) %>%
    filter(treated==1)
  untreated_data <- unmatched_data_i %>%
    filter(lot_id %in% lots_with_deforestation$lot_id) %>%
    filter(!(treated==1) & !(TSZName %in% tsz_revocations) & (TSZName %in% include_tsz)) 
  sample_multiplier <- 20
  if (nrow(treated_data)*sample_multiplier > nrow(untreated_data)) {
    untreated_data_sample <- slice_sample(untreated_data, n=nrow(treated_data)*20)
    unmatched_data_sample <- rbind(treated_data, untreated_data_sample)
  } else {
    unmatched_data_sample <- rbind(treated_data, untreated_data)
  }
  
  m.out <- matchit(treated ~ log(lot_area) + ecol_cond + elev + income + pop_den + prec + temp + drought + slope + as.character(fire)+ forest_type + forest_category + dist_tpf + woody_nsw, data = unmatched_data_sample , replace = F, method = 'nearest', verbose = TRUE)
  
  plot(summary(m.out))
  #readr::write_rds(m.out, "output/match_data/m_out.rds")
  
  # Get matching data and obtain indices for matching
  m.data <- match.data(m.out)
  treated_group <- unmatched_data[as.numeric(rownames(m.out$match.matrix)), 'lot_id']
  control_group <- unmatched_data[as.numeric(m.out$match.matrix), 'lot_id']
  
  slats_data_match <- slats_data %>%
    filter(lot_id %in% m.data$lot_id) %>%
    mutate(year = substr(lyr, 7, 10) %>% as.numeric()) %>%
    mutate(end_year = substr(lyr, 12, 15) %>% as.numeric()) %>%
    mutate(annual_deforestation = forestry / (end_year - year))  %>%
    collect()
  
  match_data <- slats_data_match %>%
    inner_join(m.data, by = 'lot_id') %>%
    mutate(annual_deforestation_cells = round(annual_deforestation / 625)) %>% # 25m x 25m cells, 625m2 area
    mutate(t = year - treatment_year) %>%
    mutate(BA = ifelse(year >= treatment_year, 1, 0)) %>%
    mutate(CI = ifelse(treated, 1, 0))
  
  # BACI estimation
  match_model <- inla(annual_deforestation_cells ~ 1 + BA + CI + BA*CI + BA*t + CI*t + BA*CI*t + 
                        f(lot_id, model = 'iid'), data=match_data, family = 'nbinomial')
  
  readr::write_rds(m.data, paste0("output/match_data/match_model_tsz/match_data_tsz_", tsz_i, ".rds"))
  readr::write_rds(m.out, paste0("output/match_data/match_model_tsz/match_out_tsz_", tsz_i, ".rds"))
  readr::write_rds(match_model, paste0("output/match_data/match_model_tsz/match_model_tsz_", tsz_i, ".rds"))
  
  match_coef_plt(match_model) %>%
    ggsave(paste0("plots/match_model_tsz_",tsz_i,".png"), ., units = 'cm', width = 15, height = 10)
}

plt <- match_data %>%
  mutate(baci_id = paste0(BA, "-", CI)) %>%
  ggplot(aes(x = t, y = annual_deforestation_cells, 
             color = as.factor(baci_id))) +
  #geom_point() +
  geom_smooth(method = 'lm', se = F) +
  ggpubr::theme_pubr()
plt


