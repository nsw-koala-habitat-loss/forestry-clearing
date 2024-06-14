## Extract SF ID
library(terra)
library(exactextractr)
library(future)
library(future.apply)
library(data.table)
library(sf)
library(readr)
library(tidyverse)

kmr_list = list(CC = "Central_Coast",
                CST = "Central_Southern_Tablelands",
                DRP = "Darling_Riverine_Plains",
                FW = "Far_West",
                NC = "North_Coast",
                NT = "Northern_Tablelands",
                NS = "Northwest_Slopes",
                R = "Riverina",
                SC = "South_Coast")

plan(multisession)

future_lapply(names(kmr_list), function(kmr) {
  kmr_abbr <- kmr
  rast <- terra::rast('output/state_forests/sf_full.tif')
  lots <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = kmr_list[[kmr]])
  sf_full <- read_csv('output/state_forests/state_forests_all.csv')
  lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')
  sf_num_id <- exact_extract(rast, lots, 'mode')
  lots$sf_id <- sf_full[match(sf_num_id, sf_full$sf_num_id),'sf_id']$sf_id
  lot_info <- st_drop_geometry(lots)
  fwrite(lot_info, paste0("output/lot_info/lot_sf_", kmr, '.csv'), append = F)
})

library(arrow)
sf_full <- read_csv('output/state_forests/state_forests_all.csv')
lot_files <- list.files('output/lot_info/', full.names = T)
lot_files <- lot_files[grepl("lot_sf", lot_files)]
lot_sf <- bind_rows(lapply(lot_files, read_csv))
lot_sf_association <- lot_sf %>%
  filter(sf_id != "") %>%
  left_join(sf_full, by = 'sf_id')
fwrite(lot_sf_association, 'output/model_data/lots_sf_association.csv',append=F)


