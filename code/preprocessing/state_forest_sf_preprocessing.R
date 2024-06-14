## Create combined dataset of revoked and current state forests
library(sf)
library(terra)
library(tidyverse)
library(data.table)

# Load and reproject data
revoked_sf_np <- st_read("output/revoked_state_forests/revoked_sf_np.shp")
current_sf <- st_read("data/NSW_Dedicated_State_Forests/NSW_Dedicated_State_Forests.shp") %>%
  st_transform(3308)
current_sf$CurrentTen <- "FCNSW"

# Combine datasets and generate unique ID
sf_full <- bind_rows(current_sf, revoked_sf_np)
sf_full <- sf_full %>%
  mutate(revoked = !is.na(Revocation)) %>%
  mutate(revocation_year = as.numeric(format(Revocation, '%Y'))) %>%
  group_by(SFNo) %>%
  arrange(revoked) %>%
  mutate(sf_id = paste0(SFNo, "_", row_number()) ) %>%
  ungroup() %>%
  mutate(sf_num_id = row_number()) %>% 
  mutate(area_km2 = st_area(geometry) %>% units::set_units(km^2))

print(paste0("sf_id is unique: ", !any(duplicated(sf_full))))

st_write(sf_full, "output/state_forests/state_forests_all.shp", append=F)
fwrite(st_drop_geometry(sf_full), "output/state_forests/state_forests_all.csv")

ref_raster <- "../risk-model-covariates/output/elev.tif"
sf_rast <- rasterize(terra::vect(sf_full), 
                     terra::rast(ref_raster), 'sf_num_id', background = 0, 
                     filename= 'output/state_forests/sf_full.tif', overwrite=T)

## NSW Admin boundaries/ KMR and revoked state forests lookup
lga <- st_read("data/Admin_boundaries/NSW_LGA_GDA2020/NSW_LGA_GDA2020.shp")
kmr <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/KMRs.shp")
timber_supply <- st_read("data/TimberSupplyZones/TimberSZ_LL.shp")




lga_intersects <- sf_full %>%
  st_intersects(st_transform(lga, st_crs(sf_full)))

# Extract the first LGA that intersects
lga_table <- st_drop_geometry(lga)[sapply(lga_intersects, \(x) x[[1]][1]),] %>%
  select(LGA_CODE21, LGA_NAME21)

kmr_intersects <- sf_full %>%
  st_intersects(st_transform(kmr, st_crs(sf_full)))

kmr_table <- st_drop_geometry(kmr)[sapply(kmr_intersects, \(x) x[[1]][1]),] %>%
  select(Region, KMR)

timber_supply_intersects <- sf_full %>%
  st_intersects(st_transform(timber_supply, st_crs(sf_full)))

timber_supply_table <- st_drop_geometry(timber_supply)[sapply(timber_supply_intersects, \(x) x[[1]][1]),] %>%
  select(TSZ)

sf_lga_kmr <- cbind(sf_full, lga_table, kmr_table, timber_supply_table)

st_write(sf_lga_kmr, "output/state_forests/state_forests_lga_kmr.shp", append = F)
fwrite(st_drop_geometry(sf_lga_kmr), "output/state_forests/state_forests_lga_kmr.csv")

## State forest distance to closest 
wood_processing_facilities <- st_read('https://spatial.industry.nsw.gov.au/arcgis/rest/services/PUBLIC/Forestry_NSW_Wood_Processing_Facilities/MapServer/0/query?where=1%3D1&text=&objectIds=&time=&timeRelation=esriTimeRelationOverlaps&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=OBJECTID%2C+NAME%2C+ADDRESS%2C+LGA%2C+WOOD_TYPE%2C+LATITUDE%2C+LONGITUDE&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&sqlFormat=none&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson')

wood_processing_facilities <- wood_processing_facilities %>%
  mutate(softwood = grepl("Softwood", WOOD_TYPE),
         hardwood = grepl("Hardwood", WOOD_TYPE),
         cypress = grepl("Cypress", WOOD_TYPE))



