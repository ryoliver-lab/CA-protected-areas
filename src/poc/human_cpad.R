library(tidyverse)
library(patchwork)
library(sf)
library(terra)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

# strava <- rast(here::here("data", "raw_data", "strava_heatmap.tif")) %>%
#   project("EPSG:4326")
# 
# strava_masked = mask(strava, cpad_super)
# 
# plot(strava_masked)
# 
# writeRaster(strava_masked, here::here("data", "raw_data", "strava_masked.tif"))

strava <- rast(here::here("data", "raw_data", "strava_masked.tif")) %>%
  project("EPSG:4326")

cpad_strava_values <- terra::extract(x = strava, y = cpad_super)

#write_csv(cpad_strava_values, here::here("data", "processed_data", "strava_cpad.csv"))
