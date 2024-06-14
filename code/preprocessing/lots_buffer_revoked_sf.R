## Intersect lots with buffer of revoked state forests --------

library(sf)
library(exactextractr)
library(data.table)
library(dplyr)


revoked_sf_np <- st_read('output/revoked_state_forests/revoked_sf_np.shp')
revoked_sf_np_grouped_buffer <- st_read('output/revoked_state_forests/revoked_sf_np_grouped_buffer.shp')

kmr = list(CC = "Central_Coast",
           CST = "Central_Southern_Tablelands",
           DRP = "Darling_Riverine_Plains",
           FW = "Far_West",
           NC = "North_Coast",
           NT = "Northern_Tablelands",
           NS = "Northwest_Slopes",
           R = "Riverina",
           SC = "South_Coast")

lyr = kmr[[kmr_id]]
kmr_abbr <- names(kmr)[kmr_id]

lots <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/lots_kmrs.gdb", layer = lyr)
lots$lot_id = paste(kmr_abbr, 1:nrow(lots), sep = '_')

## Falls within current protected area

lot_intersect_sf <- function(lots, y) {
  # lots: sf obj with lot_id column
  # y: sf obj with group_idx column
  intersect_obj <- st_intersects(lots, y)
  intersects_df <- data.frame(lot_id = c(), group_idx = c())
  for (i in 1:nrow(intersect_obj)) {
    if (length(intersect_obj[[i]]) == 0) {
      next
    } else {
      for (j in 1:length(intersect_obj[[i]])) {
        lot_id <- lots$lot_id[i]
        group_idx <- y$group_idx[intersect_obj[[i]][j]]
        intersects_df <- rbind(intersects_df, c(lot_id, group_idx))
      }
    }
  }
  colnames(intersects_df) <- c("lot_id", "group_idx")
  return(intersects_df)
}

inside_np <- lot_intersect_sf(lots, revoked_sf_np)

fwrite(inside_np, paste0('output/revoked_state_forests/lots_revoked_sf/lots_inside_revoked_sf_', kmr_abbr, '.csv'))

inside_buffer <- lot_intersect_sf(lots, revoked_sf_np_grouped_buffer)

fwrite(inside_buffer, paste0('output/revoked_state_forests/lots_revoked_sf/lots_buffer_revoked_sf_', kmr_abbr, '.csv'))

current_sf <- st_read("output/state_forests/state_forests_all.shp") %>%
  filter(CrrntTn == "FCNSW") %>%
  mutate(group_idx = sf_id)

inside_current_sf <- lot_intersect_sf(lots, current_sf)
fwrite(inside_current_sf, paste0('output/revoked_state_forests/lots_revoked_sf/lots_current_sf_', kmr_abbr, '.csv'))
