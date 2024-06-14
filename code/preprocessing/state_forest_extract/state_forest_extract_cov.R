library(data.table)
library(sf)

source("code/preprocessing/extract_functions.R")

sf_full <- st_read("output/state_forests/state_forests_all.shp")

sf_cov <- extract_cov(sf_full)
fwrite(sf_cov, 'output/state_forests/state_forest_cov.csv', append=F)