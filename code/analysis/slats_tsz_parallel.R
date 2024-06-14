library(data.table)
library(sf)

source("code/preprocessing/extract_functions.R")

args <- commandArgs(trailingOnly = T)
if (length(args) > 0) {
  job_id <- as.numeric(args[1])
}

tsz <- c("Far North Coast", "Eden","Tumut","Western Cypress","Riverina","Coffs-Grafton",
         "Mid North Coast", "Taree","Hunter","Walcha-Styx","Batemans Bay","Narooma","Queanbeyan")

tsz_i <- tsz[job_id]

timber_supply_zones <- st_read("data/TimberSupplyZones/TimberSZ_LL.shp") %>%
  st_transform(3308)

timber_supply_zones_pnf <- st_read("output/slats_tsz/timber_supply_zones_pnf.shp")

slats_tsz <- extract_slats(timber_supply_zones[timber_supply_zones$TSZName == tsz_i,], "TSZName")

fwrite(slats_tsz, paste0("output/slats_tsz/slats_tsz_", tsz_i, ".csv"))

slats_tsz_pnf <- extract_slats(timber_supply_zones_pnf[timber_supply_zones_pnf$TSZName == tsz_i,], "TSZName")

fwrite(slats_tsz_pnf, paste0("output/slats_tsz/slats_tsz_pnf_", tsz_i, ".csv"))