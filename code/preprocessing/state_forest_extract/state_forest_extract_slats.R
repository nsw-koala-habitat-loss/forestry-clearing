library(data.table)
library(sf)

source("code/preprocessing/extract_functions.R")

sf_full <- st_read("output/state_forests/state_forests_all.shp")

sf_slats <- extract_slats(sf_full, 'sf_id')
fwrite(sf_slats, 'output/state_forests/state_forest_slats.csv')