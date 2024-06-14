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
  lots <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = kmr_list[[kmr]])
  #lots <- lots[1:10,]
  lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')
  centroid_xy = st_centroid(lots) %>% st_coordinates()
  lots_xy = cbind(lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_'), centroid_xy)
  fwrite(lots_xy, paste0("output/lots_xy/lots_xy_", kmr, '.csv'), append = F)
})