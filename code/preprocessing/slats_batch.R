# R batch script for processing SLATS data
library(foreach)
library(doParallel)
library(pbapply)
library(terra)
library(dplyr)
library(tools)

source("code/preprocessing/spatial_functions.R")
source("code/preprocessing/load_slats.R")

ow <- TRUE # Overwrite?

args <- commandArgs(trailingOnly = T)
if (length(args) > 0) {
  job_id <- as.numeric(args[1])
}
slats_batch(slats_path[[job_id]])
