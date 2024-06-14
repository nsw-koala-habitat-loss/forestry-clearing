# Extract SLATS data to revoked state forests
library(exactextractr)
library(terra)
library(sf)
library(dplyr)

# Revoked state forests
revoked_sf <- st_read("data/Revoked_State_Forest_Areas/RevokedStateForestAreas/RevokedStateForestAreas.shp")

revoked_sf

