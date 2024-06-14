## Plots ----
library(ggplot2)
library(tidyverse)
library(ggsflabel)
library(ggrepel)
library(ggspatial)

boundary <- st_read("data/Admin_boundaries/nsw_lga.shp")
kmr <- st_read("data/risk_analysis/clearing_spatial_units_data_prep/risk_model_spatial_units_prep/KMRs.shp")
timber_supply <- st_read("data/TimberSupplyZones/TimberSZ_LL.shp")
timber_supply$area <- as.numeric(st_area(timber_supply))
timber_supply_centroid <- st_centroid(timber_supply)
timber_supply_centroid_coord <- cbind(st_coordinates(timber_supply_centroid), st_drop_geometry(timber_supply))

revoked_state_forests_tsz <- read_csv("data/r_data/revoked_state_forests_tsz.csv")

large_revocation_size <- 100

revoked_state_forests_tsz_max_year <- revoked_state_forests_tsz %>% 
  filter(!is.na(ha_revoked_tsz)) %>%
  group_by(TSZName) %>% 
  filter(ha_revoked_tsz > large_revocation_size) %>% 
  summarise(max_year= max(year, na.rm = T))

tsz_plt <- ggplot(timber_supply) +
  geom_sf(data = boundary) +
  geom_sf(linewidth = 0.5) +
  geom_label_repel(data = timber_supply_centroid_coord, aes(x = X, y = Y, label = TSZName))+
  theme_void() +
  theme(panel.border = element_rect(fill = NA)) +
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "br", which_north = "true")

ggsave("plots/tsz_plt.png", tsz_plt)

timber_supply_subset <- st_drop_geometry(timber_supply[!(timber_supply$TSZName %in% c("Western Cypress")),])
tsz <- timber_supply_subset$TSZName
tsz_area <- timber_supply_subset$area

tsz <- c("Batemans Bay","Coffs-Grafton","Eden","Far North Coast","Mid North Coast","Narooma","Queanbeyan","Riverina",
         "Taree","Tumut","Walcha-Styx", "Hunter")
slats_tsz <- lapply(tsz, \(t) read_csv(paste0("output/slats_tsz/slats_tsz_", t, ".csv"), show_col_types = F)) %>%
  bind_rows()
slats_tsz_pnf <- lapply(tsz, \(t) read_csv(paste0("output/slats_tsz/slats_tsz_pnf_", t, ".csv"), show_col_types = F)) %>%
  bind_rows()
slats_tsz_sf <-slats_tsz
slats_tsz_sf[,c('natural', 'agriculture', 'infrastructure', 'forestry')] <- 
  slats_tsz[,c('natural', 'agriculture', 'infrastructure', 'forestry')] -
  slats_tsz_pnf[,c('natural', 'agriculture', 'infrastructure', 'forestry')]
slats_tsz_comb <- bind_rows(list(all = slats_tsz, sf = slats_tsz_sf, pnf = slats_tsz_pnf), .id = "source")

slats_tsz_plt <- slats_tsz_comb %>%
  filter(source != "all") %>%
  left_join(revoked_state_forests_tsz_max_year, by = "TSZName") %>%
  mutate(year = start_year) %>%
  mutate(post_treatment = year >= max_year) %>%
  left_join(timber_supply_subset, by = 'TSZName') %>%
  #mutate(TSZName = factor(TSZName, levels = c("Queanbeyan", "Narooma", "Batemans Bay",
  #                                            "Coffs-Grafton", "Eden", "Far North Coast",
  #                                            "Hunter", "Mid North Coast", "Taree", "Tumut", "Walcha-Styx"))) %>%
  mutate(source = factor(source, levels = c("sf", 'pnf'), labels = c("State\nForests/\nProtected\nAreas", "Private\nNative\nForestry"))) %>%
  #filter(!(id %in% c("Queanbeyan", "Narooma", "Batemans Bay"))) %>%
  ggplot(aes(x = start_year, y = forestry / (end_year - start_year), color = source)) +
  geom_line() +
  geom_smooth(method = 'lm', formula = y ~ 1, aes(color = source, group = paste0(source,post_treatment)), se = F) +
  facet_wrap(vars(reorder(TSZName, max_year)), ncol = 3, scales = 'free_y') +
  scale_x_continuous("Year") +
  scale_y_continuous("Forestry clearing (ha/year)") +
  scale_color_discrete("") +
  theme_bw()
slats_tsz_plt
ggsave('plots/slats_tsz_plt.png', slats_tsz_plt)


## Cleveland plot ----------

revoked_state_forests_tsz_plt_df <- revoked_state_forests_tsz %>%
  filter(!is.na(ha_revoked_tsz)) %>%
  mutate(large_revocation = ifelse(ha_revoked_tsz > large_revocation_size, "Yes", "No")) %>%
  left_join(revoked_state_forests_tsz_max_year, by = 'TSZName')

clev_plt <- revoked_state_forests_tsz_plt_df %>%
  ggplot(aes(x = year, y = reorder(TSZName, -max_year))) +
  geom_segment(data=revoked_state_forests_tsz_plt_df %>% filter(large_revocation == 'Yes'), aes(yend = TSZName), xend = 0, colour = "grey50") +
  geom_point(aes(colour = large_revocation, size = ha_revoked_tsz)) +
  scale_colour_brewer('More than\n100ha\nrevoked', palette = "Set1") +
  scale_y_discrete("Timber Supply Zone") +
  theme_bw() +
  coord_cartesian(xlim = c(1988, 2023)) +
  guides(size = "none") +
  theme(
    panel.grid.major.y = element_blank(),   # No horizontal grid lines
    #legend.position = c(1, 0.55),           # Put legend inside plot area
    legend.justification = c(1, 0.5)
  )
clev_plt
ggsave("plots/revocations_tsz_clev_plt.png", clev_plt)

## Matching estimates plot TSZ -----
tsz <- unique(revoked_state_forests_tsz$TSZName)
tsz <- tsz[tsz != 'Western Cypress' & tsz != 'Hunter']
match_model_tsz <- lapply(tsz, function(t) {
  path <- paste0("output/match_data/match_model_tsz/match_model_tsz_",t,".rds")
  if (file.exists(path)) {
    return(read_rds(path))
  }
})

match_data_tsz <- lapply(tsz, function(t) {
  path <- paste0("output/match_data/match_model_tsz/match_data_tsz_",t,".rds")
  if (file.exists(path)) {
    return(read_rds(path))
  }
})

match_data_size <- sapply(match_data_tsz, \(m) unique(m$lot_id) %>% length())

data.frame(tsz, match_data_size/2)

names(match_model_tsz) <- tsz

coefs <- lapply(match_model_tsz, function(m) {
  coefs <- m$summary.fixed 
  coefs$coef <- rownames(coefs)
  rownames(coefs) <- NULL
  return(coefs)
})

lapply(match_model_tsz, function(m) {
  coefs <- m$summary.fixed 
  coefs$coef <- rownames(coefs)
  rownames(coefs) <- NULL
  return(coefs)
})

purrr::map(names(match_model_tsz), \(i) match_coef_plt(match_model_tsz[[i]]))

baci_tsz_plt <- purrr::map(coefs[[1]]$coef, function(coef_name) {
  df <- bind_rows(coefs, .id = "TSZName")
  df$N <- match_data_size[match(df$TSZName, tsz)]
  df %>%
    filter(coef == coef_name) %>%
    mutate(TSZName = paste0(TSZName, " (N=",N, ")")) %>%
    ggplot(aes(x = reorder(TSZName, mean))) +
    geom_linerange(aes(ymin = `0.025quant`, ymax = `0.975quant`), color = 'gray20', linewidth = 1) +
    geom_hline(yintercept = 0) +
    geom_point(aes(y = mean), size = 2) +
    ggpubr::theme_pubr() +
    coord_flip() +
    scale_x_discrete("Timber Supply Zone") +
    scale_y_continuous(paste(coef_name, "estimate")) +
    ggtitle(paste(coef_name, "estimates"))
})
names(baci_tsz_plt) <- coefs[[1]]$coef

lapply(seq_along(baci_tsz_plt), \(i) ggsave(paste0('plots/baci_tsz',gsub(":", "", names(baci_tsz_plt)[i]) ,'.png'),baci_tsz_plt[[i]]))

## Plot matched lot locations ----------
lots_xy <- arrow::open_csv_dataset('output/lots_xy') %>% collect()
lots_xy_sf <- st_as_sf(lots_xy, coords = c('X', 'Y'), crs = 3308)
tsz_sub <- tsz[tsz!="Hunter"]
match_data <- lapply(tsz_sub, \(t) read_rds(paste0('output/match_data/match_model_tsz/match_data_tsz_', t, '.rds')))
names(match_data) <- tsz_sub

match_loc_plt <- lapply(tsz_sub, \(t) {
  treated <- revoked_state_forests_tsz_max_year[revoked_state_forests_tsz_max_year$TSZName == t,]
  control <- revoked_state_forests_tsz_max_year[revoked_state_forests_tsz_max_year$max_year <= (treated$max_year - 2),]
  match_data_unique <- match_data[[t]] %>%
    group_by(lot_id) %>%
    summarise(treated = first(treated))
  lots_xy_sf %>%
    inner_join(match_data_unique, by = 'lot_id') %>%
    mutate(treated = factor(treated, c(0,1), c("Control", "Treated"))) %>%
    ggplot() +
    geom_sf(data = boundary, fill = 'NA', linewidth = 0.5) +
    geom_sf(data = timber_supply[timber_supply$TSZName %in% rbind(control, treated)$TSZName,], linewidth = .5) +
    geom_sf(aes(color = treated)) +
    scale_color_discrete("") +
    ggtitle(paste(treated$TSZName[1]))+
    theme_void()
})

ggsave("plots/match_loc_plt.png", patchwork::wrap_plots(match_loc_plt) + patchwork::plot_layout(guides = 'collect'), height = 15, width = 25, units = 'cm')
