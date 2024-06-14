## Construct full dataset of state forests (past and present) 
# 1. Construct dataset
# 2. Assign unique identifier for each observation
# 3. Extract SLATS, covariates and woody vegetation in the background

library(future)
library(sf)
library(mapview)
library(data.table)

source("code/preprocessing/extract_functions.R")

# Extract SLATS and covariates of current and former state forests
plan(multisession)
slats_future <- future({
  sf_slats <- extract_slats(sf_full, 'sf_id')
  fwrite(sf_slats, 'output/state_forests/state_forest_slats.csv')
})

cov_future <- future({
  sf_cov <- extract_cov(sf_full)
  fwrite(sf_cov, 'output/state_forests/state_forest_cov.csv', append=F)
})

woody_future <- future({
  sf_woody_veg <- extract_woody_veg(sf_full, "sf_id")
  fwrite(sf_woody_veg, 'output/state_forests/state_forest_woody_veg.csv', append=F)
})

sf_full %>% mapview(zcol = 'SFNo')

# Extract SLATS from buffer areas
mapview(sf_full)
buffer_width <- 10000
sf_buffer <- sf_full %>%
  st_buffer(buffer_width) %>%
  st_difference(st_union(sf_full))

buffer_future <- future({
  sf_buffer_cov <- extract_cov(sf_buffer)
  fwrite(sf_buffer_cov, 'output/state_forests/state_forest_buffer_cov.csv')
})

# Union state forests based on SFNo
sf_full_union <- sf_full %>%
  group_by(SFNo, revocation_year) %>%
  summarise(revoked = min(revoked)) %>%
  group_by(SFNo) %>%
  arrange(revoked) %>%
  mutate(sf_id = ifelse(revoked==0, paste0(SFNo, "_", 0), paste0(SFNo, "_", row_number())))
View(sf_full_union)
mapview(sf_full_union, zcol = 'sf_id')

