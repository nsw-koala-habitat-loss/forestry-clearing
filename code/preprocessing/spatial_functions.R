
library(terra)

dirname <- getwd()
ref_raster <- terra::rast(file.path( "data/risk_analysis/clearing_spatial_units_data_prep/woody_veg_loss_prep/veg_loss/agr_loss_1119.tif"))
nsw = terra::vect(file.path('data/Admin_boundaries/nsw_lga.shp'))

loadShp <- function(name, input_shp) {
  return(terra::vect(input_shp))
}

projectShp <- function(input_shp, name = NA) {
  print(paste("Start projecting shapefile", name))
  if (is.na(name)) {
    stop("Name must be specified")
  }
  fname = file.path("intermediate_data", paste0(name, "_proj.shp"))
  projected_shp <- terra::vect(input_shp) %>%
    project(y = ref_raster)
  return(projected_shp)
}

projectRast <- function(input_raster, name = NA, overwrite = F) {
  print(paste("Start projecting raster", name))
  if (is.na(name)) stop("Name must be specified")
  fname = file.path("intermediate_data", paste0(name, "_proj.tif"))
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  
  projected_rast <- terra::rast(input_raster) %>%
    project(y = ref_raster, filename = fname, method = 'near', threads = TRUE, overwrite = overwrite)
  return(fname)
}

shpToRast <- function(input_shp, name = NA, field_name = "", to_output = FALSE, overwrite = FALSE) {
  print(paste("Start converting Shp to Rast for", name))
  if (is.na(name)) stop("Name must be specified")
  if (to_output) {
    fname = file.path("output", paste0(name, ".tif"))
  } else {
    fname = file.path("intermediate_data", paste0(name, "_rasterised.tif"))
  }
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  rasterised_shp <- terra::rasterize(x = input_shp, y = ref_raster, field = field_name, filename = fname, overwrite = overwrite)
  return(fname)
}

clipRast <- function(input_raster, name = NA, to_output = TRUE, overwrite = FALSE, apply_mask = TRUE) {
  print(paste("Start clipping raster", name))
  if (is.na(name)) stop("Name must be specified")
  if (to_output) {
    fname = file.path("output", paste0(name, ".tif"))
  } else {
    fname = file.path("intermediate_data", paste0(name, "_clip.tif"))
  }
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  clipped_rast <- terra::rast(input_raster) %>%
    terra::crop(y = nsw, mask = apply_mask, filename = fname, overwrite = overwrite)
  return(fname)
}

resampleRast <- function(input_raster, name = NA, to_output = FALSE, overwrite = FALSE, method = 'near') {
  print(paste("Start resampling raster", name))
  if (is.na(name)) stop("Name must be specified")
  if (to_output) {
    fname = file.path("output", paste0(name, ".tif"))
  } else {
    fname = file.path("intermediate_data", paste0(name, "_resampled.tif"))
  }
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  
  resample_rast <- terra::rast(input_raster) %>%
    terra::resample(y = ref_raster, filename = fname, overwrite = overwrite, method = method)
  return(fname)
}

classifyRast <- function(input_raster, name = NA, to_output = FALSE, rcl = NULL, overwrite = FALSE) {
  print(paste("Start classifying raster", name))
  if (is.na(name)) stop("Name must be specified")
  if (is.null(rcl)) stop("rcl argument must be specified")
  if (to_output) {
    fname = file.path("output", paste0(name, ".tif"))
  } else {
    fname = file.path("intermediate_data", paste0(name, "_reclassify.tif"))
  }
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  resample_rast <- terra::rast(input_raster) %>%
    terra::classify(rcl = rcl, others = NA, filename = fname, overwrite = overwrite)
  return(fname)
}


slats_batch <- function(path_obj) {
  in_file <- path_obj$path
  out_file <- path_obj$out_name
  field <- path_obj$field
  change_code <- path_obj$change_code
  start_year <- path_obj$year
  name <- tools::file_path_sans_ext(path_obj$out_name)
  
  # Reclassification matrices for SLATS data 1988-2007 and 2008-2014
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
                    62, 4)
  rcl_19882008 <- matrix(rcl_19882008, ncol = 2, byrow = T)
  
  rcl_20082014 <- c(11, 12, 1,
                    70, 79, 2,
                    80, 89, 3,
                    90, 99, 4)
  rcl_20082014 <- matrix(rcl_20082014, ncol = 3, byrow = T)
  
  if (start_year %in% as.character(seq(1988, 2007, 1))) {
    rcl <- rcl_19882008
  } else if (start_year %in% as.character(seq(2008, 2014, 1))) {
    rcl <- rcl_20082014
  } else {
    rcl <- NULL
  }
  
  rasterizeShp <- function() {
    
    proj_shp <- projectShp(in_file, name = name)
    rast <- shpToRast(proj_shp, name = name, field_name = field, overwrite = ow, to_output = is.null(rcl))
    
    if (!is.null(rcl)) {
      rast <- classifyRast(rast, name = name, rcl = rcl, overwrite = ow, to_output = TRUE)
    }
    
    return(rast)
  }
  
  reprojectRaster <- function() {
    proj_raster <- projectRast(in_file, name = name, overwrite = ow)
    rast <- resampleRast(proj_raster, name = name, to_output = is.null(rcl))
    if (!is.null(rcl)) {
      rast <- classifyRast(rast, name = name, rcl = rcl, overwrite = ow, to_output = TRUE)
    }
    
    return(rast)
  }
  
  ## Main -------------
  main <- function() {
    ext <- file_ext(in_file)
    if (ext == 'shp') {
      rasterizeShp()
    } else {
      reprojectRaster()
    }
  }
  
  main()
}