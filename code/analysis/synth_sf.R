## Synthetic controls of state forests after revocation --------

# 1. Verify that revoked state forests have lower deforestation compared to synthetic controls
# 2. Use synthetic controls on state forests in the same KMR, after revocation of another state forest in the area

library(tidysynth)
library(mapview)
library(future)
library(tidyverse)
library(sf)
library(gsynth)
library(patchwork)
library(future)
library(future.apply)
library(doFuture)

source('code/preprocessing/extract_functions.R')

slats_sf <- read_csv("output/state_forests/state_forest_slats.csv")
sf_full <- read_csv('output/state_forests/state_forests_all.csv')
sf_cov <- read_csv('output/state_forests/state_forest_cov.csv') %>%
  mutate(sf_id = sf_full$sf_id)
sf_woody <- read_csv('output/state_forests/state_forest_woody_veg.csv')
sf_kmr_lga <- read_csv("output/state_forests/state_forests_lga_kmr.csv") %>%
  select(sf_id, LGA_NAME21, KMR)
sf_woody_1988 <- sf_woody %>%
  filter(lyr == 'woody_veg_1988') %>%
  select(-lyr, -year)


hist(slats_sf$forestry)

# Convert biannual data to annual data by assuming equal amount across years
slats_sf_annual <- slats_sf %>%
  mutate(num_years = end_year - start_year) %>%
  uncount(num_years) %>%
  mutate(natural = natural/(end_year-start_year),
         agriculture  = agriculture /(end_year-start_year),
         infrastructure  = infrastructure /(end_year-start_year),
         forestry  = forestry /(end_year-start_year)) %>%
  group_by(id) %>%
  mutate(year = 1987 + row_number())

sc_model_data <- slats_sf_annual %>%
  left_join(sf_full, by = join_by('id' == 'sf_id')) %>%
  left_join(sf_cov, by = join_by('id' == 'sf_id')) %>%
  left_join(sf_kmr_lga, by = join_by('id' == 'sf_id')) %>%
  inner_join(sf_woody_1988, by = join_by('id' == 'sf_id')) %>%
  mutate(annual_deforestation_cells = round(forestry / 625)) %>% # 25m x 25m cells, 625m2 area
  filter(is.na(revocation_year) | revocation_year > 1988) %>%
  mutate(revoked = ifelse(is.na(revocation_year), 0, ifelse(year >= revocation_year, 1, 0))) %>%
  mutate(relative_loss = forestry / (forest + sparse_woody)) %>%
  filter(!is.na(relative_loss)) %>%
  group_by(id) %>%
  mutate(cum_forestry = cumsum(forestry))

## Descriptive statistics -------------

# Deforestation rate in current SF
area_cleared_kmr <- sc_model_data %>%
  mutate(is_revoked = CurrentTen!="FCNSW") %>%
  group_by(KMR, year, is_revoked) %>%
  summarise(area_cleared = sum(forestry), pct_cleared = sum(forestry/area_km2)) %>%
  ggplot(aes(x = year, y = area_cleared, color = is_revoked)) +
  geom_line() +
  facet_wrap(~ KMR, scales = 'free_y') +
  ggpubr::theme_pubr() 
ggsave("plots/area_cleared_kmr.png", area_cleared_kmr)

# Areas revoked by KMR
areas_revoked_kmr <- sc_model_data %>%
  group_by(KMR, year) %>%
  summarise(area_revoked = sum(area_km2 * revoked),
            pct_revoked = sum(revoked*area_km2) / sum(area_km2)) %>%
  ggplot(aes(x = year, y = area_revoked, color = KMR)) +
  geom_line() +
  facet_wrap(~ KMR, scales = 'free_y') +
  ggpubr::theme_pubr()
ggsave("plots/areas_revoked_kmr.png", areas_revoked_kmr, scale = 1.3)

# Cumulative deforestation before and after t
sc_model_data %>%
  mutate(t = year - revocation_year) %>%
  filter(CurrentTen != 'FCNSW' & revocation_year < 2015) %>%
  group_by(id, t) %>%
  summarise(forest_cleared = sum(forestry),
            ) %>%
  ggplot(aes(x = t, y = forest_cleared, group = id)) +
  geom_line() +
  ggpubr::theme_pubr()

# Area revoked vs clearing rate in SF by KMR
sc_model_data %>%
  mutate(is_revoked = CurrentTen!="FCNSW", is_revocation_year = ifelse(is_revoked, year==revocation_year, F)) %>%
  group_by(KMR, year) %>%
  summarise(clearing = sum(forestry), area_revoked = sum(area_km2*is_revocation_year)) %>%
  ggplot(aes(x = clearing, y = area_revoked)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggpubr::theme_pubr() +
  facet_wrap(~KMR)

## Assign the year with the largest revocation to the treatment year
max_revoke_kmr <- sc_model_data %>%
  group_by(KMR, year) %>%
  summarise(area_revoked = sum(area_km2 * revoked),
            pct_revoked = sum(revoked*area_km2) / sum(area_km2)) %>%
  ungroup() %>%
  group_by(KMR) %>%
  slice(which.max(area_revoked))

## Construct synthetic control
plan(multisession)
plt_list <- list()
foreach (i= 1:9) %dofuture% {
  kmr_i <- max_revoke_kmr[i, "KMR"][[1]]
  rev_year <- max_revoke_kmr[i, "year"][[1]]
  df <- sc_model_data %>%
    left_join(max_revoke_kmr %>% transmute(year_most_revoked= year), by = 'KMR')
  whole_group <- df %>%
    group_by(KMR, year) %>%
    summarise(forestry = sum(forestry),
              temp = mean(temp), 
              prec = mean(prec),
              slope = mean(slope),
              elev = mean(elev))
  treated <- whole_group %>%
    filter(KMR == kmr_i) %>%
    mutate(id = 'TREATED') %>%
    ungroup() %>%
    select(-KMR)
  donor <- df %>%
    filter(KMR != kmr_i & area_km2 > 50) %>%
    select(all_of(names(treated)))%>%
    ungroup()
  
  out <- rbind(treated, donor) %>%
    synthetic_control(outcome = forestry, 
                      unit = id,
                      time = year,
                      i_unit = 'TREATED',
                      i_time = rev_year,
                      generate_placebos=T) %>%
    generate_predictor(time_window = 1988:2021,
                       slope = mean(slope, na.rm=T),
                       elev = mean(elev, na.rm=T),
                       temp = mean(temp, na.rm=T),
                       prec = mean(prec, na.rm=T)) %>%
    generate_weights(optimization_window = 1988:rev_year, 
                     margin_ipop = .02,
                     sigf_ipop = 7,bound_ipop = 6) %>%
    generate_control()
  
  trend_plt <- out %>% plot_trends()
  
  weight_plt <- out %>% plot_weights()
  
  placebo_plt <- out %>% plot_placebos()
  
  plt <- list(trend_plt, weight_plt, placebo_plt)
  
  out_plt <- wrap_plots(plt) + 
    plot_annotation(
      title = kmr_i
    ) +
    plot_layout(guides = 'collect') & theme(legend.position='bottom') 
  ggsave(paste0('plots/synth_kmr_small_', kmr_i, '.png'), out_plt, width = 2000, height = 1000, units = 'px', scale = 2, dpi = 300)
}

for (i in 1:9) {

}


sc_glm <- sc_model_data %>%
  filter(CurrentTen=="FCNSW") %>%
  mutate(fire = ifelse(is.na(fire), 0, fire)) %>%
  INLA::inla(annual_deforestation_cells ~ elev + income + pop_den + prec + temp + drought + slope + as.character(fire) + as.character(forest_code) + as.character(remoteness) + soil_fert + soil_nitrogen +
               as.character(forest_tenure) + f(sf_id, model = 'iid') + f(year, model = 'iid'), data= .)
summary(sc_glm)
match_coef_plt(sc_glm)

validate <- sc_model_data %>%
  group_by(id) %>%
  summarise(sum_forestry = sum(forestry), woody = first(forest+sparse_woody))
validate$id[validate$sum_forestry > validate$woody]

sc_model_data <- sc_model_data %>%
  filter(!(id %in% validate$id[validate$sum_forestry > validate$woody]))

hist(sc_model_data$relative_loss)

out <- gsynth(relative_loss ~ revoked, data = sc_model_data,
              index = c("id","year"), force = "two-way", 
              CV = TRUE, r = c(0, 5), se = TRUE, 
              inference = "parametric", nboots = 1000, 
              parallel = TRUE, cores = 4, min.T0=7)

plot(out, type='gap')
plot(out, type='counterfactual', id = most_deforested[30,'id'][[1]])

most_deforested <- sc_model_data %>%
  filter(!is.na(revocation_year)) %>%
  filter(revocation_year < 2015) %>%
  group_by(id, revoked) %>%
  summarise(forestry = sum(forestry)) %>%
  filter(revoked==0) %>%
  arrange(-forestry)

treated_units <- unique(sc_model_data$id[!is.na(sc_model_data$revocation_year)])
plot(out, type = "counterfactual", id=most_deforested[11,'id'][[1]])


## Tidysynth
set.seed(14839489)
i_unit <- most_deforested[3,'id'][[1]]
rev_year <- sc_model_data[sc_model_data$id == i_unit,'revocation_year'][[1]][1]
treated <- sc_model_data %>%
  filter(id == i_unit)
donor <- sc_model_data %>%
  filter( is.na(revocation_year))
sample_donor <- donor %>%
  group_by(id) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  slice_max(n=100, order_by = area_km2)
donor <- donor[donor$id %in% sample_donor$id,]

out <- rbind(treated, donor) %>%
  synthetic_control(outcome = relative_loss, 
                    unit = id,
                    time = year,
                    i_unit = i_unit,
                    i_time = rev_year) %>%
  generate_predictor(time_window = 1988:rev_year, 
                     elev = mean(elev, na.rm=T), slope = mean(elev, na.rm=T), 
                     temp = mean(temp, na.rm=T), prec=mean(prec, na.rm=T),
                     forest_tenure = first(forest_tenure, na.rm=T),
                     forest_code = first(forest_code, na.rm = T),
                     forest_tenure_type = first(forest_tenure_type, na.rm = T)) %>%
  generate_weights(optimization_window = 1988:rev_year, margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6)%>%
  generate_control()

out %>% plot_trends()

out %>% plot_weights()

out %>% plot_placebos()



## Construct synthetic controls --------------
buffer_revoked_sf_year <- buffer_revoked_sf %>%
  left_join(revoked_sf_np_buffer, by = 'group_idx') %>%
  st_drop_geometry() %>%
  select(-geometry) %>%
  group_by(lot_id) %>%
  arrange(Revocation) %>%
  filter(row_number()==1)

sc_model_data <- slats_data %>%
  mutate(start_year = as.numeric(substr(lyr, 7, 10))) %>%
  mutate(end_year = as.numeric(substr(lyr, 12, 15))) %>%
  mutate(annual_forestry = forestry / (end_year-start_year)) %>%
  group_by(lot_id) %>%
  arrange(start_year) %>%
  mutate(cum_forestry = cumsum(annual_forestry * (end_year-start_year))) %>%
  select(lot_id, start_year, annual_forestry, cum_forestry) %>%
  left_join(cov_data, by = 'lot_id') %>%
  left_join(buffer_revoked_sf_year, by = 'lot_id') %>%
  mutate(Revocation_Year = format(Revocation, "%Y")) %>%
  mutate(near_revoked_sf = ifelse(is.na(Revocation), 0, ifelse(start_year > Revocation_Year, 1, 0))) %>%
  filter(!(lot_id %in% inside_revoked_sf$lot_id))

# Reduce donor pool
set.seed(5843589)
N = 500
sc_model_treated <- sc_model_data %>%
  filter(!is.na(Revocation_Year)) %>%
  filter(Revocation_Year > 2000)
sc_model_treated_sample <- sc_model_treated$lot_id %>%
  unique() %>%
  sample(N, replace = F)
sc_model_treated <- sc_model_treated %>%
  filter(lot_id %in% sc_model_treated_sample)

sc_model_donor <- sc_model_data %>%
  filter(is.na(Revocation_Year))
sc_model_donor_sample <- sc_model_donor$lot_id %>%
  unique() %>%
  sample(N*5, replace = F)
sc_model_donor <- sc_model_donor %>%
  filter(lot_id %in% sc_model_donor_sample)

out <- gsynth(annual_forestry ~ near_revoked_sf, data = rbind(sc_model_treated, sc_model_donor), index = c("lot_id", "start_year"), min.T0 = 7, EM = TRUE, force = "two-way", inference = "parametric", 
              se = TRUE, nboots = 500, r = c(0, 5), 
              CV = TRUE, parallel = TRUE, cores = 4)

plot(out, type = "counterfactual", raw = "none", main="")
