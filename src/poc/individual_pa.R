library(tidyverse)
library(sf)
library(patchwork)

cpad_strava_coarse <- st_read(here::here("data", "processed_data", "cpad_strava_coarse.shp")) %>%
  sf::st_transform("EPSG:4326")

cpad_ghm <- st_read(here::here("data", "processed_data", "gHM_masked.tif")) %>%
  sf::st_transform("EPSG:4326")

cpad_hfp <- st_read(here::here("data", "processed_data", "HFP2009_masked.tif")) %>%
  sf::st_transform("EPSG:4326")

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")