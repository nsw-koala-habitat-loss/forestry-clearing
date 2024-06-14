library(tidyverse)
library(exactextractr)
library(sf)
library(terra)
library(lubridate)
library(ggplot2)
library(units)

source("code/load_slats.R")
source("code/slats_functions.R")

npws_estate <- sf::st_read('data/tenure_npws_estate/NPWS_Estate.shp') %>%
  filter(GAZ_DATE > "2015-01-01") %>%
  st_transform(3308)

slats <- c(fcn_slats_2015_2017(), fcn_slats_2017_2020())

deforestation_area <- lapply(slats, \(x) fcn_slats_extract_raster(x, npws_estate))

names(deforestation_area) <- paste0('year', 2015:2020)
npws_estate_def <- cbind(npws_estate, bind_rows(deforestation_area, .id = 'year'))

npws_estate_def %>%
  pivot_longer(names(deforestation_area), names_to = 'year', values_to = 'clearing', names_prefix = "year") %>%
  mutate(gazette_year = lubridate::year(GAZ_DATE)) %>%
  mutate(year_gazetted = as.numeric(year) - gazette_year) %>%
  ggplot(aes(x = year_gazetted, y = clearing / st_area(geometry))) +
  geom_line(aes(color = RES_NO)) +
  #geom_smooth() +
  theme_bw()


## Harvest history and SLATS detection

hfd <- sf::st_read("data/HFDHarvestHistory/HFDHarvestHistory.shp") %>%
  filter(EventDate > "2017-01-01" & EventDate < "2018-01-01")  %>%
  st_transform(3308)

slats <- c(fcn_slats_2015_2017(), fcn_slats_2017_2020())

deforestation_area <- lapply(slats, \(x) fcn_slats_extract_raster(x, hfd[1:100,], "GlobalID"))
names(deforestation_area) <- paste0('year', 2015:2020)

hfd[1:100,] %>%
  cbind(bind_rows(deforestation_area, .id = 'year')) %>%
  pivot_longer(names(deforestation_area), names_to = 'year', values_to = 'clearing', names_prefix = "year") %>%
  group_by(GlobalID) %>%
  mutate(cum_clearing = cumsum(clearing)) %>%
  mutate(gazette_year = lubridate::year(EventDate)) %>%
  mutate(year_gazetted = as.numeric(year) - gazette_year) %>%
  ggplot(aes(x = year_gazetted, y = cum_clearing / st_area(geometry))) +
  geom_line(aes(group = GlobalID), alpha = 0.1) +
  geom_smooth() +
  theme_bw()
