# Script to reclassify SLATS data to a common format

library(tidyverse)
library(terra)
library(foreach)
library(doParallel)
library(pbapply)

# Codes:
# 1. Natural/ fire
# 2. Agriculture
# 3. Infrastructure
# 4. Forestry

rcl_19882008 <- c(11, 1,
                  12, 1,
                  40, 2,
                  41, 2,
                  42, 2,
                  55, 2,
                  46, 3,
                  48, 3,
                  51, 3,
                  53, 4,
                  60, 4,
                  61, 4, 
                  62, 4) %>% matrix(ncol = 2, byrow = T)

rcl_20082014 <- c(11, 12, 1,
                  70, 79, 2,
                  80, 89, 3,
                  90, 99, 4) %>% matrix(ncol = 3, byrow = T)

slats_dir <- "processed_data/slats/"
recoded_files <- list.files(slats_dir, pattern = "slats_recoded_\\d{4}_\\d{4}.tif$", full.names = T)
file.rename(recoded_files, sub("slats_recoded_", "slats_", recoded_files))

slats_tif <- list.files(slats_dir, pattern = "slats_\\d{4}_\\d{4}.tif$", full.names = T)

cl <- makeCluster(detectCores() - 1, type = "PSOCK")
registerDoParallel(cl)

foreach (i = 1:length(slats_tif), .packages = c("terra")) %dopar% {
  tif_path <- slats_tif[[i]]
  start_year <- sub(".*_(\\d{4})_.*", "\\1", tif_path)
  print(sprintf("Start conversion for %s", tif_path))
  tif_path_mod <- sub("slats_", "slats_recoded_", tif_path)
  slats_rast <- rast(tif_path)
  
  rcl <- NULL
  if (start_year %in% as.character(seq(1988, 2007, 1))) {
    rcl <- rcl_19882008
  } else if (start_year %in% as.character(seq(2008, 2014, 1))) {
    rcl <- rcl_20082014
  }

  if (!is.null(rcl)) {
    recoded <- classify(slats_rast, rcl, others = NA)
  } else {
    recoded <- slats_rast
  }
  
  terra::writeRaster(recoded, tif_path, overwrite= T)
  print(sprintf("Finished conversion for %s", tif_path))
  return(recoded)
}

stopCluster(cl)