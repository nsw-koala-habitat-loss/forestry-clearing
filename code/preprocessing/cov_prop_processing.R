library(tidyverse)
library(arrow)
library(future)
library(data.table)
library(purrr)
library(future.apply)
library(lfe)

cov_prop_comb <- fread("output/model_data/cov_prop_comb.csv")
slats_prop_df <- fread("output/model_data/slats_prop_df.csv")

cov_prop_comb[, year := as.numeric(cov_prop_comb[,year])]

# Inside buffer SF or not
sf_all <- fread('output/state_forests/state_forests_all.csv')

lots_buffer_revoked_sf <- lapply(list.files('output/revoked_state_forests/lots_revoked_sf', pattern = "^lots_buffer_revoked_sf", full.names = T), fread) %>% do.call(rbind, .) %>%
  left_join(sf_all, by = join_by('group_idx' == 'group_idx')) %>%
  group_by(lot_id) %>%
  summarise(revocation_year = min(revocation_year))

lots_revoked_sf <- lapply(list.files('output/revoked_state_forests/lots_revoked_sf', 
                                     pattern = "^lots_inside_revoked_sf", full.names = T), fread) %>% 
  do.call(rbind, .) %>%
  left_join(sf_all, by = join_by('group_idx' == 'group_idx')) %>%
  group_by(lot_id) %>%
  arrange(revocation_year) %>%
  summarise(revocation_year = min(revocation_year), group_idx = first(group_idx))

lots_current_sf <- lapply(list.files('output/revoked_state_forests/lots_revoked_sf', 
                                     pattern = "^lots_current_sf", full.names = T), fread) %>% do.call(rbind, .) %>%
  group_by(lot_id) %>%
  summarise()

buffer_revoked_sf_lot_id <- lots_buffer_revoked_sf$lot_id[!(lots_buffer_revoked_sf$lot_id %in% lots_revoked_sf$lot_id) & !(lots_buffer_revoked_sf$lot_id %in% lots_current_sf$lot_id)]
revoked_sf_lot_id <- lots_revoked_sf$lot_id[!(lots_revoked_sf$lot_id %in% lots_current_sf$lot_id) & !(lots_revoked_sf$lot_id %in% buffer_revoked_sf_lot_id)]
current_sf_lot_id <- lots_current_sf$lot_id[!(lots_current_sf$lot_id %in% lots_revoked_sf$lot_id) & !(lots_current_sf$lot_id %in% lots_buffer_revoked_sf$lot_id)]

lots_sf_association <- list(buffer_revoked_sf = data.frame(lot_id = buffer_revoked_sf_lot_id),
                             revoked_sf = data.frame(lot_id = revoked_sf_lot_id),
                             current_sf = data.frame(lot_id = current_sf_lot_id, revocation_year = NA))

lots_sf_association$buffer_revoked_sf <- lots_sf_association$buffer_revoked_sf %>%
  left_join(lots_buffer_revoked_sf, by = 'lot_id') %>%
  select(lot_id, revocation_year)
lots_sf_association$revoked_sf <- lots_sf_association$revoked_sf %>%
  left_join(lots_revoked_sf, by = 'lot_id') %>%
  select(lot_id, revocation_year)

lots_sf_association <- bind_rows(lots_sf_association, .id='treatment')

fwrite(lots_sf_association, 'output/model_data/lots_sf_association.csv')

# Deforestation model
model_data <- inner_join(slats_prop_df, cov_prop_comb, by = c('lot_id', 'year')) %>%
  filter(woody_nsw > 0)
#model_data <- model_data %>%
#  inner_join(lot_info, by = 'lot_id')

model_data$buffer_revoked_sf <- ifelse(model_data$lot_id %in% lots_buffer_revoked_sf$lot_id, 1, 0) * ifelse(model_data$lot_id %in% lots_revoked_sf$lot_id, 0, 1) * ifelse(model_data$lot_id %in% lots_current_sf$lot_id, 0, 1)

model_data$revoked_sf <- ifelse(model_data$lot_id %in% lots_revoked_sf$lot_id, 1, 0)

model_data$current_sf <- ifelse(model_data$lot_id %in% lots_current_sf$lot_id, 1, 0)

model_data[, forestry_relative_loss := model_data[, forestry] / ((model_data[,woody_nsw]*model_data[,lot_area])*(model_data[,end_year] - model_data[,year]))]

model_data <- model_data %>%
  group_by(lot_id) %>%
  arrange(year) %>%
  mutate(cum_forestry_loss = cumsum(forestry)) %>%
  mutate(cum_forestry_loss_prop = min(1, cum_forestry_loss / lot_area))

# Select those in buffer of revoked SF and not in revoked
model_data <- model_data %>%
  left_join(rbind(lots_revoked_sf),by = 'lot_id') %>%
  mutate(years_revoked = revocation_year - year) %>%
  mutate(treated = year < revocation_year) %>%
  separate(lot_id, c('kmr','id'))

fwrite(model_data, "output/model_data/model_data.csv")

model_data_revoked_sf <- model_data %>%
  filter(revoked_sf > 0)


model_data_revoked_sf %>%
  #filter(kmr=='CST') %>%
  ggplot(aes(y = forestry_relative_loss, x = years_revoked)) +
  facet_wrap(vars(kmr)) +
  geom_smooth(method = 'loess')
