library(terra)
library(sf)
library(tidyverse)

## 1988-2010 data --------

fcn_slats_1988_2010 <- function(raster = TRUE, name_only = T) {
  year = c(seq(1988,2006,2), 2007:2008)
  year_num = lapply(year, \(x) sprintf('%02d', x %% 100))
  path <- "data/slats/slats1988_2010/slatswoodychange19882010/SLATS_WoodyChange1988_2010/Data/e%s%sLCC_Epoch.shp"
  slats_path <- list()
  for (i in 1:(length(year)-1)) {
    slats_path[[i]] <- list(
      year = year[i],
      end_year = year[i+1],
      path = sprintf(path, year_num[i], year_num[i+1]),
      field = "GRIDCODE",
      change_code = "19882008"
    )
  }
  # Check if all paths exist
  if (!all(sapply(slats_path, \(x) file.exists(x$path)))) {
    stop("Not all SLATS files exist in directory")
  }
  
  if (name_only) {
    return(slats_path)
  }
  
  slats_19882010 <- lapply(slats_path, function(path_obj) {
    path <- path_obj$path
    sp <- sf::st_read(path, quiet = T)
    vars <- names(sp)
    
    # Filter only forestry activity
    if ("Major_Cate" %in% vars) {
      return(dplyr::filter(sp, Major_Cate == "Forestry Activity"))
    } else {
      return(dplyr::filter(sp, Category == "Forestry Activity"))
    }
  })
}

## 2008-2014 data -----
fcn_slats_2008_2014 <- function(name_only = T) {
  year <- 2008:2014
  year_num = year %% 100
  path <- "data/slats/slats2008_2014/cipsspot5nswe%02d%02d3308/cips_spot5_nsw_e%02d%02d_3308.shp"
  slats_path <- list()
  for (i in 1:(length(year)-1)) {
    slats_path[[i]] <- list(
      year = year[i],
      end_year = year[i+1],
      path = sprintf(path, year_num[i], year_num[i+1], year_num[i], year_num[i+1]),
      field = "gridcode",
      change_code = "20082014"
    )
  }
  
  year <- 2014:2015
  year_num = year %% 100
  path <- "data/slats/slats2008_2014/vegetationwoodychange%02d%02dcips/Vegetation_WoodyChange_%02d%02d_CIPS/cips_spot5_nsw_e%02d%02d_3308.shp"
  for (i in 1:(length(year)-1)) {
    slats_path[[length(slats_path)+1]] <- list(
      year = year[i],
      end_year = year[i+1],
      path = sprintf(path, year_num[i], year_num[i+1], year_num[i], year_num[i+1], year_num[i], year_num[i+1]),
      field = "gridcode",
      change_code = "20082014"
    )
  }
  
  # Check if all paths exist
  if (!all(sapply(slats_path, \(x) file.exists(x$path)))) {
    stop("Not all SLATS files exist in directory")
  }
  
  if (name_only) {
    return(slats_path)
  }
  
  slats_20082014 <- lapply(slats_path, function(path_obj) {
    path <- path_obj$path
    sp <- sf::st_read(path, quiet = T)
    vars <- names(sp)
    
    # Filter only forestry activity
    sp %>% filter(substr(as.character(code), 0, 1) == "9")
  })
}


## 2015-2017 data ----
fcn_slats_2015_2017 <- function(name_only = T) {
  year <- 2015:2017
  year_num = year %% 100
  path <- "data/slats/slats_2015_17/vegetationslatscvmsrenswe%02d%02daeql0r5m/cvmsre_nsw_e%02d%02d_aeql0_r5m.tif"
  slats_path <- list()
  for (i in 1:(length(year)-1)) {
    slats_path[[i]] <- list(
      year = year[i],
      end_year = year[i+1],
      path = sprintf(path, year_num[i], year_num[i+1], year_num[i], year_num[i+1]),
      field = "NA",
      change_code = "20152017"
    )
  }
  # Check if all paths exist
  if (!all(sapply(slats_path, \(x) file.exists(x$path)))) {
    stop("Not all SLATS files exist in directory")
  }
  
  if (name_only) {
    return(slats_path)
  }
  
  slats_20152017 <- lapply(slats_path, function(path_obj) {
    path <- path_obj$path
    sp <- terra::rast(path)
    return(sp)
  })
}

## 2017-2020 data ----
fcn_slats_2017_2020 <- function(name_only = T) {
  year <- 2017:2021
  year_num = year %% 100
  path <- "data/slats/slats_2017_20/cvmsre_nsw_e%02d%02d_aerl0/cvmsre_nsw_e%02d%02d_aerl0.tif"
  slats_path <- list()
  for (i in 1:(length(year)-1)) {
    slats_path[[i]] <- list(
      year = year[i],
      end_year = year[i+1],
      path = sprintf(path, year_num[i], year_num[i+1], year_num[i], year_num[i+1]),
      field = "NA",
      change_code = "20172020"
    )
  }
  # Check if all paths exist
  if (!all(sapply(slats_path, \(x) file.exists(x$path)))) {
    stop("Not all SLATS files exist in directory")
  }
  
  if (name_only) {
    return(slats_path)
  }
  
  slats_2017_2020 <- lapply(slats_path, function(path_obj) {
    path <- path_obj$path
    sp <- terra::rast(path)
    return(sp)
  })
}

slats_path <- c(fcn_slats_1988_2010(), fcn_slats_2008_2014(), fcn_slats_2015_2017(), fcn_slats_2017_2020()) %>%
  lapply(function(x) {
    x$out_name = paste0("slats_", x$year, "_", x$end_year, ".tif")
    return(x)
  }) %>%
  rev()

slats_path_df <- do.call(rbind, lapply(slats_path, unlist))
write_csv(slats_path_df %>% as.data.frame(), 'data/slats/slats_paths.txt', col_names = F)
