## SLATS by timber supply zones
library(data.table)
library(sf)

source("code/preprocessing/extract_functions.R")

timber_supply_zones <- st_read("data/TimberSupplyZones/TimberSZ_LL.shp") %>%
  st_transform(3308)

slats_tsz <- extract_slats(timber_supply_zones, "TSZName")

fwrite(slats_tsz, "output/slats_tsz/slats_tsz.csv")

npws <- st_read("data/npws_estateinternalboundaries/NPWS_EstateInternalBoundaries.shp")%>%
  st_transform(3308) %>%
  summarise(geometry = st_union(geometry))
current_state_forests <- st_read('data/r_data/current_state_forests.shp') %>%
  st_transform(3308) %>%
  summarise(geometry = st_union(geometry))
revoked_state_forests <- st_read("data/Revoked_State_forest_Areas/RevokedStateForestAreas/RevokedStateForestAreas.shp") %>%
  st_transform(3308) %>%
  summarise(geometry = st_union(geometry))
excluded_areas <- st_union(current_state_forests, revoked_state_forests) %>%
  st_union(npws)

st_write(excluded_areas, "output/slats_tsz/sf_npws_union.shp")

timber_supply_zones_pnf <- st_difference(timber_supply_zones, excluded_areas)
st_write(timber_supply_zones_pnf, "output/slats_tsz/timber_supply_zones_pnf.shp")

slats_tsz_pnf <- extract_slats(timber_supply_zones_pnf, "TSZName")
fwrite(slats_tsz_pnf, "output/slats_tsz/slats_tsz_pnf.csv")



