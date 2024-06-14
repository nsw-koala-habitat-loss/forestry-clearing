# Find lots within buffer of recently revoked state forests
library(exactextractr)
library(terra)
library(sf)
library(dplyr)
library(mapview)
library(tidyr)
library(purrr)

# Revoked state forests ------------
revoked_sf <- st_read("data/Revoked_State_Forest_Areas/RevokedStateForestAreas/RevokedStateForestAreas.shp")

revoked_sf_transformed <- revoked_sf %>%
  st_transform(3308)

# Select only revoked SF and those converted to protected areas
revoked_sf_np <- revoked_sf_transformed %>%
  filter(CurrentTen %in% c("Flora Reserve", "National park", "National Park", "Nature Reserve",
                           "Regional Park", "State Conservation Area")) %>%
  filter(!is.na(Revocation))

## Union if revocation is in the same year and located within 50km from each other and revocation is within 180 days ---------------
dist <- 20000
days_threshold <- 180

dist_mat <- st_is_within_distance(revoked_sf_np, dist = dist)

# Calculate days apart
dates_apart_close <- lapply(revoked_sf_np$Revocation, \(x) which(abs(x - revoked_sf_np$Revocation) < days_threshold))

# Check to see if SF meets criteria for unioning (i.e. close to each other and revoked within 180 days of each other)
meet_union_criteria <- list()
for (i in 1:nrow(revoked_sf_np)) {
  meet_union_criteria[[i]] <- dist_mat[[i]][dist_mat[[i]] %in% dates_apart_close[[i]]]
}

revoked_sf_np$idx <- 1:nrow(revoked_sf_np)
revoked_sf_np$group_idx <- NA
grouped_idx <- c()
for (i in 1:length(meet_union_criteria)) {
  to_group <- meet_union_criteria[[i]][!(meet_union_criteria[[i]] %in% grouped_idx)]
  revoked_sf_np[to_group, 'group_idx'] <- i
  grouped_idx <- c(grouped_idx, to_group)
}
unique(revoked_sf_np$group_idx) %>% length()

# Get grouped state forest revocations
revoked_sf_np_grouped <- revoked_sf_np %>%
  group_by(group_idx) %>%
  summarise(Revocation = mean(Revocation)) %>%
  mutate(area = st_area(geometry) %>% units::set_units(km^2))

revoked_sf_np_grouped_buffer <- revoked_sf_np_grouped %>% st_buffer(10000)

# Write shapefiles to output
st_write(revoked_sf_np, 'output/revoked_state_forests/revoked_sf_np.shp', append=F)
st_write(revoked_sf_np_grouped_buffer, 'output/revoked_state_forests/revoked_sf_np_grouped_buffer.shp', append=F)

