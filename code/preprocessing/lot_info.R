library(sf)
library(data.table)
library(future.apply)

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
  lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')
  lots$lot_area <- st_area(lots)
  lot_info <- st_drop_geometry(lots)
  fwrite(lot_info, paste0("output/lot_info/lot_info_", kmr, '.csv'))
})