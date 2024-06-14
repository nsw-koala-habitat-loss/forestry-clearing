# Extract covariate values into lots

library(terra)
library(dplyr)
library(exactextractr)
library(sf)
library(data.table)

source("code/preprocessing/extract_functions.R")

kmr = list(CC = "Central_Coast",
           CST = "Central_Southern_Tablelands",
           DRP = "Darling_Riverine_Plains",
           FW = "Far_West",
           NC = "North_Coast",
           NT = "Northern_Tablelands",
           NS = "Northwest_Slopes",
           R = "Riverina",
           SC = "South_Coast")

# Covariates
args <- commandArgs(trailingOnly = T)
if (!is.na(args[1])) {
  job_id <- args[1] + 1
}

extract_kmr_woody <- function(kmr_id) {
  lyr = kmr[[kmr_id]]
  kmr_abbr <- names(kmr)[kmr_id]
  
  lots <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = lyr)
  lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')
  
  prop_extract <- extract_woody_veg(lots, 'lot_id')
  
  fwrite(prop_extract, paste0('output/cov_prop/cov_prop_', kmr_abbr, '.csv'))
  
  sf_woody_veg <- extract_woody_veg(lots)
  fwrite(sf_woody_veg, paste0('output/woody_prop/woody_prop_',kmr_abbr, '.csv'), append=F)
}

extract_kmr_woody(job_id)