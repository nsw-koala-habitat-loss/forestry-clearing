# Extract SLATS data 

library(terra)
library(dplyr)
library(exactextractr)
library(sf)
library(data.table)

source("code/preprocessing/extract_functions.R")

# Properties
kmr = list(CC = "Central_Coast",
           CST = "Central_Southern_Tablelands",
           DRP = "Darling_Riverine_Plains",
           FW = "Far_West",
           NC = "North_Coast",
           NT = "Northern_Tablelands",
           NS = "Northwest_Slopes",
           R = "Riverina",
           SC = "South_Coast")

extract_kmr <- function(kmr_id) {
  
  lyr = kmr[[kmr_id]]
  kmr_abbr <- names(kmr)[kmr_id]
  
  lots <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = lyr)
  lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')
  
  prop_extract <- extract_slats(lots, id_col = 'lot_id')
  
  fwrite(prop_extract, paste0('output/slats_prop/slats_prop_', kmr_abbr, '.csv'))
}

args <- commandArgs(trailingOnly = T)
if (length(args) > 0) {
  job_id <- as.numeric(args[1])
}
extract_kmr(job_id)
