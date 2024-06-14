# Functions to extract covariates and SLATS data

library(terra)
library(exactextractr)
library(dplyr)

load_covariates <- function(output_dir = NULL, cov_type = 'all') {
  if (is.null(output_dir)) {
    output_dir <- "../risk-model-covariates/output"
  }
  
  categorical_covs <- c('landuse', 'drought', 'soil_fert', 'soil_type', 'forest_tenure', 'forest_tenure_type', 'forest_code', 'fire', 'remoteness')
  
  continuous_covs <- c("woody_nsw", "prec", "ecol_cond", "prop_size", "prop_value",
                       "drought", "elev", "temp", "slope", "soil_nitrogen",
                       "income", "pop_den")
  
  output_files <- list.files(output_dir, "*.tif$", full.names = T)
  lyr_names <- list.files(output_dir, "*.tif$") %>% tools::file_path_sans_ext()
  
  full_list <- 1:length(lyr_names)
  if (cov_type == "categorical") {
    idx <- lyr_names %in% categorical_covs
  } else if (cov_type == 'continuous') {
    idx <- lyr_names %in% continuous_covs
  } else {
    idx <- lyr_names %in% continuous_covs | lyr_names %in% categorical_covs
  }
  
  comb_rast <- rast(output_files[idx])
  names(comb_rast) <- lyr_names[idx]
  return(comb_rast)
}

extract_cov <- function(sf_obj, output_dir=NULL, cov_type = 'both') {
  if (is.null(output_dir)) {
    output_dir <- "../risk-model-covariates/output"
  }
  
  if (cov_type == 'both'| cov_type == 'categorical' ) {
    covs <- load_covariates(output_dir, cov_type = 'categorical')
    prop_extract_categorical <- exact_extract(x = covs, y = sf_obj, fun='mode')
    colnames(prop_extract_categorical) <- names(covs)
  }
  
  if (cov_type == 'both'| cov_type == 'continuous') {
    covs <- load_covariates(output_dir, cov_type = 'continuous')
    prop_extract_continuous <- exact_extract(x = covs, y = sf_obj, fun='mean')
    colnames(prop_extract_continuous) <- names(covs)
  }
  
  if (cov_type == 'categorical') return(prop_extract_categorical)
  if (cov_type == 'continuous') return(prop_extract_continuous)
  
  return(cbind(prop_extract_categorical,prop_extract_continuous))
  
}

extract_slats <- function(sf_obj, id_col = 'id') {
  # sf_obj: a Simple Features (sf) object of interest
  # id_col: the name of the column in the sf_obj that uniquely identifies each feature in sf_obj

  # SLATS data
  rast_files <- list.files('output/slats', '*.tif$', full.names = T)
  rast_names <- list.files('output/slats', '*.tif$', full.names = F) %>% tools::file_path_sans_ext()
  slats_uniform <- rast(rast_files)
  names(slats_uniform) <- rast_names
  
  slats_codes <- c("natural", "agriculture", "infrastructure", "forestry")
  
  sum_func <- function(df) {
    res <- sapply(1:4, function(id) {
      mat <- df[,rast_names] == id
      t(mat & !is.na(mat)) %*% df$coverage_area
    })
    
    res <- res %>% as.data.frame()
    colnames(res) <- slats_codes
    res <- cbind(id = df$id[1], lyr = rast_names, res)
    return(res)
  }
  
  sf_obj$id <- sf_obj[[id_col]]
  
  prop_extract <- exact_extract(x = slats_uniform, y = sf_obj, fun = sum_func, coverage_area = TRUE, include_cols = c("id"), summarize_df = TRUE)
  prop_extract[[id_col]] <- prop_extract$id
  prop_extract <- prop_extract %>%
    mutate(start_year = as.numeric(substr(lyr, 7, 10))) %>%
    mutate(end_year = as.numeric(substr(lyr, 12, 15)))
  
  return(prop_extract)
}

extract_woody_veg <- function(sf_obj, id_col = 'id') {
  # SLATS data
  #rast_files <- list.files('data/woodyv3_geotiff', '*.tif$', full.names = T)[1]
  #rast_names <- list.files('data/woodyv3_geotiff', '*.tif$', full.names = F)[1] %>% 
  #  tools::file_path_sans_ext()
  
  rast_files <- list.files('../risk-model-covariates/output', pattern='woody_veg', full.names = T)
  rast_names <- list.files('../risk-model-covariates/output', 'woody_veg', full.names = F) %>% tools::file_path_sans_ext()
  
  #ref_raster <- list.files('output/slats', '*.tif$', full.names = T)[1] %>% rast()
  woody_uniform <- rast(rast_files)
  names(woody_uniform) <- rast_names
  woody_codes <- c("forest", "sparse_woody", "non_woody")
  sf_obj$id <- sf_obj[[id_col]]
  
  sum_func <- function(df) {
    res <- sapply(1:3, function(id) {
      mat <- df[,rast_names] == id
      t(mat & !is.na(mat)) %*% df$coverage_area
    })
    
    res <- res %>% as.data.frame()
    colnames(res) <- woody_codes
    res <- cbind(id = df$id[1], lyr = rast_names, res)
    return(res)
  }
  
  prop_extract <- exact_extract(x = woody_uniform, y = sf_obj, fun = sum_func, coverage_area = TRUE, include_cols = c("id"), summarize_df = TRUE)
  prop_extract[[id_col]] <- prop_extract$id
  prop_extract <- prop_extract %>%
    mutate(year = as.numeric(substr(lyr, 6,9)))
  
  return(prop_extract)
}