library(sf)
library(mapview)
library(future)
library(future.apply)
library(foreach)
library(lubridate)
library(units)
library(INLA)
library(tidyverse)
library(tidyquant)
library(MatchIt)

source("code/preprocessing/extract_functions.R")

read_from_file <- T

if (read_from_file) {
  harvest_history <- st_read('data/r_data/harvest_history.shp')
  
  current_state_forests <- st_read('data/r_data/current_state_forests.shp')
  
  hardwood_plantations <- st_read("data/r_data/hardwood_plantations.shp")
} else {

  harvest_history <- st_read('https://services2.arcgis.com/iCBB4zKDwkw2iwDD/arcgis/rest/services/HFDHarvestHistory/FeatureServer/5/query?outFields=*&where=1%3D1&f=geojson')
  
  current_state_forests <- st_read('https://services2.arcgis.com/iCBB4zKDwkw2iwDD/arcgis/rest/services/NSW_Dedicated_State_Forests/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')
  
  hardwood_plantations <- st_read('https://services2.arcgis.com/iCBB4zKDwkw2iwDD/arcgis/rest/services/FCNSW_Hardwood_Plantation/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')
  
  st_write(current_state_forests, "data/r_data/current_state_forests.shp", append=F)
  st_write(harvest_history, "data/r_data/harvest_history.shp", append=F)
  st_write(hardwood_plantations, "data/r_data/hardwood_plantations.shp")
} 

revoked_state_forests <- st_read("data/Revoked_State_forest_Areas/RevokedStateForestAreas/RevokedStateForestAreas.shp")

npws_estate <- st_read("data/npws_estateinternalboundaries/NPWS_EstateInternalBoundaries.shp") %>%
  st_transform(3308)

timber_supply_zones <- st_read("data/TimberSupplyZones/TimberSZ_LL.shp") %>%
  st_transform(3308)

timber_processing_facilities <- st_read("https://spatial.industry.nsw.gov.au/arcgis/rest/services/PUBLIC/Forestry_NSW_Wood_Processing_Facilities/MapServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
  st_transform(3308)

## Read from file ----------------

## Quantify the area lost per year in current state forests
current_state_forests <- st_make_valid(current_state_forests) %>%
  st_transform(3308)

harvest_history <- harvest_history %>%
  st_transform(3308) %>%
  st_make_valid()
colnames(harvest_history) <- c("OBJECTID", "EventDate", "RCNo", "SFName", "SFNo", "CptNo", "EventType", "HarvestType", "HarvestDetail", "SHAPE_Area", "SHAPE_Length", "geometry")

harvest_history$area <- st_area(harvest_history)

current_state_forests_joined <- current_state_forests %>%
  group_by(SFNo) %>%
  summarise(geometry = st_union(geometry),
            SFName = first(SFName)) %>%
  arrange(SFNo)

## Classify state forests and harvest history by timber supply zones
current_state_forests_joined <- st_join(current_state_forests_joined,
                                        timber_supply_zones,
                                        join = st_intersects,
                                        largest = T)

## Loop through all timber supply zones to get intersection
current_state_forests_centroid <- st_centroid(current_state_forests_joined)
harvest_history_centroid <- st_centroid(harvest_history)
centroid_distances <- st_distance(current_state_forests_centroid, harvest_history_centroid)

# Intersect to find harvest history in each state forest
plan(multisession)
max_distance <- 10000 # max distance between centroid of harvest and state forest to be considered, speeds up intersect calculations
units(max_distance) <- 'm'

state_forest_harvests <- future_lapply(1:nrow(current_state_forests_joined),
                                       function(i) {

  state_forest_i <- current_state_forests_joined[i,]
  
  harvest_history_i <- harvest_history[centroid_distances[i,] < max_distance,]
  
  harvest_history_intersection <- st_intersection(state_forest_i, 
                                                  harvest_history_i)
  
  harvest_dates <- as.POSIXct(harvest_history_intersection$EventDate / 1000, origin = "1970-01-01", tz = "Australia/Sydney") %>%
    as.Date()
  
  harvest_history_intersection$EventDate <- harvest_dates
  
  return(harvest_history_intersection)
}, future.seed = TRUE)

# Summarise clearing in each state forest by area
harvest_df <- future_lapply(state_forest_harvests, function(obj) {
  obj <- obj %>%
    mutate(year = as.numeric(format(EventDate, "%Y"))) %>%
    group_by(year) %>%
    summarise(geometry = st_union(geometry), HarvestType = first(HarvestType), 
              HarvestDetail = first(HarvestDetail), .groups = 'drop')
  ha_cleared <- as.numeric(st_area(obj)) * 0.0001
  year <- obj$year
  harvest_type <- obj$HarvestType
  harvest_details <- obj$HarvestDetail
  df <- data.frame(ha_cleared, year, harvest_type, harvest_details)
  df %>% 
    group_by(year, harvest_details) %>%
    summarise(ha_cleared = sum(ha_cleared), .groups = 'drop')
}, future.seed = TRUE) %>%
  bind_rows(.id = 'i')
harvest_df$year <- as.numeric(harvest_df$year)
harvest_df$i <- as.numeric(harvest_df$i)

# Join with current state forest layer ----------------
current_state_forests_joined$i <- as.numeric(1:nrow(current_state_forests_joined))
current_state_forests_harvests <- current_state_forests_joined %>%
  left_join(harvest_df, by = 'i')

current_state_forests_harvests$area_ha <- as.numeric(st_area(current_state_forests_harvests)) * 0.0001

current_state_forests_cum_harvests <- current_state_forests_harvests %>%
  group_by(SFNo) %>%
  mutate(cum_ha_cleared = cumsum(ha_cleared)) %>%
  mutate(cum_ha_cleared_prop = as.numeric(cum_ha_cleared / area_ha))

# Calculate area of revocations in 10 km buffer area -------

## Recreate full dataset for revocations
# Find NPWS estate with past harvests
##harvest_history_union <- st_union(harvest_history)
#harvest_history_outside_bounds <- st_difference(harvest_history_union, st_union(revoked_state_forests))
#revoked_state_forests_additional <- st_intersection(harvest_history_outside_bounds, npws_estate)

buffer_dist <- 10000
revoked_state_forests_centroid <- st_centroid(revoked_state_forests)
current_state_forest_distance_revoked <- st_distance(current_state_forests_centroid, revoked_state_forests_centroid)
revoked_area_years <- future_lapply(1:nrow(current_state_forest_distance_revoked),
                                    function(i) {
  revoked_sf <- revoked_state_forests[current_state_forest_distance_revoked[i,] < units::as_units(buffer_dist, 'm'),]
  ha_revoked <- as.numeric(st_area(revoked_sf)) * 0.0001 # m2 to ha
  count_revoked <- length(unique(revoked_sf$SFNo))
  year <- format(revoked_sf$Revocation, '%Y')
  if (count_revoked == 0) {
    df <- data.frame(year = c(), ha_revoked = c(), count_revoked = c())
  } else {
    df <- data.frame(year, ha_revoked, count_revoked)
  }
}, future.seed=TRUE)
revoked_area_year_df <- revoked_area_years %>% 
  bind_rows(.id = 'i') %>% 
  filter(!is.na(year)) %>%
  group_by(i, year) %>%
  summarise(ha_revoked = as.numeric(sum(ha_revoked)), 
            count_revoked = as.numeric(sum(count_revoked)),
            .groups='drop')
revoked_area_year_df$year <- as.numeric(revoked_area_year_df$year)
revoked_area_year_df$i <- as.numeric(revoked_area_year_df$i)
revoked_area_year_df

# Distance to timber processing facilities
current_state_forest_processing_distance <- st_distance(current_state_forests_centroid,
                                                        timber_processing_facilities) %>%
  apply(1, min)

# Calculate area of revocations in the timber supply zone
revoked_state_forests_joined <- st_join(revoked_state_forests,
                                  timber_supply_zones,
                                  join = st_intersects,
                                  largest = T)
revoked_state_forests_joined$area_ha <- as.numeric(st_area(revoked_state_forests_joined) * 0.0001)

# Area of revoked state forests in timber supply zone
revoked_state_forests_tsz <- revoked_state_forests_joined %>%
  st_drop_geometry() %>%
  mutate(year = as.numeric(format(Revocation, '%Y'))) %>%
  group_by(TSZName, year) %>%
  summarise(ha_revoked_tsz = sum(area_ha))

revoked_state_forests_tsz <- revoked_state_forests_tsz %>%
  left_join(revoked_state_forests_tsz %>%
              group_by(TSZName) %>%
              filter(ha_revoked_tsz == max(ha_revoked_tsz)) %>%
              transmute(year_max_revoke = year), by = 'TSZName')

revoked_state_forests_tsz$year_max_revoke[revoked_state_forests_tsz$TimberSupp == 'Supply Zone 13'] = NA

revoked_state_forests_tsz %>%
  filter(year > 2002) %>%
  full_join(expand.grid(year = 1900:2023, TSZName = unique(revoked_state_forests_tsz$TSZName)), by = c("TSZName", "year")) %>%
  arrange(TSZName, year) %>%
  mutate(ha_revoked_tsz = ifelse(is.na(ha_revoked_tsz), 0, ha_revoked_tsz)) %>%
  ungroup() %>%
  #filter((TSZName %in% c("Batemans Bay", "Queanbeyan", "Narooma"))) %>%
  filter(!(TSZName %in% c("Western Cypress", 'Riverina'))) %>%
  group_by(TSZName) %>%
  mutate(cum_ha_revoked_tsz = cumsum(ha_revoked_tsz)) %>%
  ggplot(aes(x = year, y = cum_ha_revoked_tsz, color = TSZName)) +
  coord_cartesian(xlim = c(2002, 2023)) +
  geom_line()

## Covariates ----------------
# Extract covariates in state forests
if (read_from_file) {
  cov <- read_csv('data/r_data/cov.csv')
} else {
  cov <- terra::extract(load_covariates(), current_state_forests_centroid)
  write_csv(cov, 'data/r_data/cov.csv')
}

cov$i <- 1:nrow(cov)
cov$timber_processing_dist <- current_state_forest_processing_distance

## Model data construction -----

analysis_period <- 1970:2023
i <- as.numeric(1:nrow(current_state_forests_joined))
full_time_series <- expand.grid(analysis_period, i)
colnames(full_time_series) <- c('year', 'i')
current_state_forests_joined$i <- i
model_data <- current_state_forests_joined %>%
  st_drop_geometry() %>%
  left_join(full_time_series, by = 'i') %>%
  left_join(harvest_df, by = c('i', 'year')) %>%
  mutate(ha_cleared = ifelse(is.na(ha_cleared), 0, as.numeric(ha_cleared))) %>%
  left_join(revoked_area_year_df, by = c('i', 'year')) %>%
  mutate(ha_revoked = ifelse(is.na(ha_revoked), 0, as.numeric(ha_revoked))) %>%
  mutate(count_revoked = ifelse(is.na(count_revoked), 0, as.numeric(count_revoked))) %>%
  left_join(revoked_state_forests_tsz, by = c('TimberSupp', 'year')) %>%
  mutate(ha_revoked_tsz = ifelse(is.na(ha_revoked_tsz), 0, ha_revoked_tsz)) %>%
  left_join(cov, by = 'i') %>%
  arrange(SFNo, year) %>%
  mutate(any_clearing = ifelse(ha_cleared > 0, 1, 0)) %>%
  mutate(ha_revoked_lag1 = lag(ha_revoked)) %>%
  mutate(count_revoked_lag1 = lag(count_revoked)) %>%
  mutate(any_revoked_lag1 = lag(ifelse(ha_revoked > 0, 1, 0))) %>%
  mutate(ha_revoked_tsz_lag1 = lag(ha_revoked_tsz)) %>%
  mutate(cum_revocations = cumsum(ha_revoked)) %>%
  mutate(treated = ifelse(!is.na(year_max_revoke), 1, 0))

## Summary data from timber supply zones --------
model_data %>%
  group_by(TimberSupp, year) %>%
  summarise(ha_revoked = sum(ha_revoked), 
            ha_cleared = sum(ha_cleared),
            TSZName = first(TSZName)) %>%
  pivot_longer(c('ha_revoked', 'ha_cleared')) %>%
  #filter(name == 'ha_cleared') %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line() +
  facet_wrap(vars(TSZName), scales = 'free_y') +
  theme_bw()

## Matching by covariates and long-term 
m.out <- model_data %>%
  filter(year == 1970) %>%
  matchit(treated ~ area + ecol_cond + elev + income + pop_den + prec + temp + drought + slope + as.character(fire) + as.character(forest_code) + as.character(remoteness) + soil_fert + soil_nitrogen +
                   as.character(forest_tenure) + as.character(TimberSupp), 
                 data = ., replace = F)
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

model_data$fire <- ifelse(is.na(model_data$fire), "NONE", model_data$fire)

## Model fitting ----
model <- inla(any_clearing ~ any_revoked_lag1 + f(SFNo, model = 'iid') + f(year, model = 'iid'), data = model_data, family = 'logistic')

summary(model)

match_coef_plt(model)



mapview(current_state_forests_joined %>%
               filter(SFNo == 488)) +
  (harvest_history[centroid_distances[which(current_state_forests_joined$SFNo==488),] < as_units(10000, 'm'),] %>% 
     mapview(col.regions = 'yellow'))

current_state_forests_cum_harvests %>%
  ggplot(aes(x = year, y = cum_ha_cleared, color = SFNo, group = SFNo)) +
  geom_line() +
  theme_bw() +
  theme(legend.position= 'none')

current_state_forests_joined$area_ha <- as.numeric(st_area(current_state_forests_joined)) * 0.0001

current_state_forests_joined %>%
  st_drop_geometry() %>%
  group_by(TSZName) %>%
  summarise(area_ha = sum(area_ha))
ts <- expand.grid(year = 1899:2023, TSZName = unique(timber_supply_zones$TSZName))
harvest_history_tsz <- st_intersects(harvest_history,timber_supply_zones)
harvest_history$TSZName<- sapply(harvest_history_tsz, \(i) timber_supply_zones$TSZName[i][1])
harvest_history$area <- as.numeric(st_area(harvest_history)) * 0.0001

harvest_history %>%
  st_drop_geometry() %>%
  mutate(year = year(as.POSIXct(EventDate / 1000, origin = "1970-01-01", tz = "Australia/Sydney"))) %>%
  group_by(TSZName, year) %>%
  summarise(ha_cleared = sum(as.numeric(area)),
            TSZName = first(TSZName)) %>%
  ungroup() %>%
  full_join(ts, by = c('year', 'TSZName')) %>%
  group_by(TSZName) %>%
  arrange(year) %>%
  mutate(ha_cleared = ifelse(is.na(ha_cleared), 0, ha_cleared)) %>%
  mutate(cum_ha_cleared = cumsum(ha_cleared)) %>%
  ggplot(aes(x = year, y = ha_cleared)) +
  geom_line(alpha = 0.5) +
  tidyquant::geom_ma(aes(color = TSZName), linetype = 1, n = 5) +
  facet_wrap(vars(TSZName)) +
  coord_cartesian(xlim = c(1970, 2023)) +
  theme_bw()

harvest_history %>%
  st_drop_geometry() %>%
  mutate(year = year(as.POSIXct(EventDate / 1000, origin = "1970-01-01", tz = "Australia/Sydney"))) %>%
  group_by(year) %>%
  summarise(ha_cleared = sum(as.numeric(area * 0.0001))) %>%
  ungroup() %>%
  arrange(year) %>%
  mutate(ha_cleared = ifelse(is.na(ha_cleared), 0, ha_cleared)) %>%
  mutate(cum_ha_cleared = cumsum(ha_cleared)) %>%
  ggplot(aes(x = year, y = ha_cleared)) +
  geom_line(alpha = 0.5) +
  #tidyquant::geom_ma(linetype = 1, n = 5) +
  coord_cartesian(xlim = c(2000, 2022)) +
  theme_bw()


## Correlate clearing in SLATS -----------
if (read_from_file) {
  current_state_forests_slats <- read_csv('data/r_data/current_state_forests_slats.csv')
} else {
  current_state_forests_slats <- extract_slats(current_state_forests_joined, 'SFNo')
  write_csv(current_state_forests_slats, 'data/r_data/current_state_forests_slats.csv')
}
current_state_forests_slats$SFNo <- current_state_forests_slats$id
current_state_forests_slats$year <- rowMeans(cbind(current_state_forests_slats$end_year, current_state_forests_slats$start_year)) %>% floor()
current_state_forests_slats$forestry <- current_state_forests_slats$forestry / (current_state_forests_slats$end_year - current_state_forests_slats$start_year)

slats_time_series <- expand.grid(year = seq(1988, 2020, 1), SFNo = unique(current_state_forests_slats$SFNo)) %>%
  as.data.frame()

current_state_forests_slats_ts <- current_state_forests_slats %>%
  right_join(slats_time_series, by = c("SFNo" = "SFNo", "year" = "year")) %>%
  filter(year >= 1988 & year <= 2020 & !is.na(SFNo)) %>%
  group_by(SFNo) %>%
  arrange(SFNo, year) %>%
  mutate(agriculture = ifelse(is.na(agriculture), lead(agriculture), agriculture),
         forestry = ifelse(is.na(forestry), lead(forestry), forestry), 
         natural = ifelse(is.na(natural), lead(natural), natural),
         infrastructure = ifelse(is.na(infrastructure), lead(infrastructure), infrastructure)) %>%
  select(SFNo, year, agriculture, forestry, natural, infrastructure)

slats_match_df <- current_state_forests_harvests %>%
  mutate(area_ha = as.numeric(st_area(geometry)) * 0.0001) %>%
  st_drop_geometry() %>%
  select(-SFName, -TimberSupp, -TSZName, -TSZ, -area_ha) %>%
  filter(!is.na(harvest_details)) %>%
  #filter(harvest_details %in% c("STS Light","STS Medium","STS Heavy", "STS Regen", "STS Release", "Clearfell", "AGS")) %>%
  full_join(current_state_forests_slats_ts, by = c('SFNo'='SFNo', 'year'='year')) %>%
  mutate(forestry = ifelse(is.na(forestry), 0, forestry)) %>%
  mutate(ha_cleared = ifelse(is.na(ha_cleared), 0, ha_cleared)) %>%
  left_join(current_state_forests_joined %>% st_drop_geometry(), by = c('SFNo'))

slats_match_df %>%
  group_by(year) %>%
  summarise(forestry = sum(forestry) * 0.0001, fcnsw_clearing = sum(ha_cleared)) %>%
  pivot_longer(c(forestry, fcnsw_clearing)) %>%
  filter(year > 1988 & year < 2021) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line() +
  theme_bw()

slats_match_df_binned <- slats_match_df %>%
  mutate(slats_clearing = (forestry * 0.0001)) %>%
  mutate(fcnsw_clearing = ha_cleared) %>%
  filter(year > 2005 & year < 2019) %>%
  mutate(year_bin = cut(year, seq(1988, 2024, 1))) %>%
  group_by(SFNo , year_bin) %>%
  summarise(fcnsw_clearing = sum(fcnsw_clearing, na.rm = T), 
            slats_clearing = sum(slats_clearing, na.rm = T),
            TSZName = first(TSZName))

slats_match_df_binned %>%
  ggplot(aes(x = fcnsw_clearing, y = slats_clearing, color = TSZName)) +
  geom_point() +
  geom_abline() +
  theme_bw()

slats_match_agg <- slats_match_df %>%
  filter(year > 2000 & year < 2019) %>%
  group_by(SFNo) %>%
  mutate(slats_clearing = (forestry * 0.0001)) %>%
  mutate(fcnsw_clearing = ha_cleared) %>%
  summarise(fcnsw_clearing = sum(fcnsw_clearing, na.rm = T), 
            slats_clearing = sum(slats_clearing, na.rm = T),
            area_ha = mean(area_ha, na.rm = T)) 

sf_area <- as.numeric(st_area(current_state_forests_joined))*0.0001
slats_match_agg %>%
  select(-area_ha) %>%
  right_join(current_state_forests_joined, by = 'SFNo') %>%
  ggplot(aes(x = fcnsw_clearing / area_ha, y = slats_clearing / area_ha)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method = 'lm') +
  theme_bw()

slats_match_df %>%
  group_by(TSZName, year) %>%
  mutate(slats_clearing = (forestry * 0.0001)) %>%
  mutate(fcnsw_clearing = ha_cleared) %>%
  summarise(fcnsw_clearing = sum(fcnsw_clearing, na.rm = T), slats_clearing = sum(slats_clearing, na.rm = T)) %>%
  pivot_longer(c('fcnsw_clearing', 'slats_clearing')) %>%
  #filter(name == 'ha_cleared') %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line() +
  coord_cartesian(xlim = c(2000, 2020)) +
  facet_wrap(vars(TSZName), scales = 'free_y') +
  theme_bw()

reg <- inla(fcnsw_clearing ~ slats_clearing + f(SFNo, model = 'iid'), data=slats_match_df_binned)
reg %>%
  summary()

model_data2 <- slats_match_df %>%
  mutate() %>%
  mutate(ha_revoked_tsz = ifelse(is.na(ha_revoked_tsz), 0, ha_revoked_tsz)) 

lm(forestry ~ ha_revoked_tsz + f(SFNo, model = 'iid'), data = model_data2)
