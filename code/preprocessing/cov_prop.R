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
if (length(args) > 0) {
  job_id <- as.numeric(args[1])
}

# Divide lot data to N number of chunks for parallel processing
num_chunks <- 1000

run_df <- expand.grid(kmr_id = seq(1,length(kmr)), cov_type = c('both'), chunk = 1:num_chunks)

extract_kmr_cov <- function(job_id) {
  kmr_id <- run_df[job_id, 'kmr_id']
  cov_type <- run_df[job_id, 'cov_type']
  chunk_id <- run_df[job_id, 'chunk']
  
  lyr = kmr[[kmr_id]]
  kmr_abbr <- names(kmr)[kmr_id]
  
  out_file <- paste0('output/cov_prop_parallel/cov_prop_', kmr_abbr, "_", as.character(chunk_id), "_", cov_type, '.csv')
  
  #if (file.exists(out_file)) return()
  
  lots <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = lyr)
  lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')
  
  chunk_size <- ceiling(nrow(lots)/num_chunks)
  chunk_start <- (chunk_id-1)*chunk_size + 1
  chunk_end <- min(chunk_id*chunk_size, nrow(lots))
  
  if (chunk_start > nrow(lots)) return()
  
  lot_subset <- lots[chunk_start:chunk_end,]
  rm(lots)
  gc()
  
  prop_extract <- extract_cov(lot_subset, cov_type = cov_type)
  prop_extract <- cbind(lot_id = lot_subset$lot_id, lot_area = st_area(lot_subset), prop_extract)
  
  fwrite(prop_extract, out_file)
}

if (!exists('job_id')) {
  stop("job id needs to be specified")
}

extract_kmr_cov(job_id)