library(tidyverse)
library(sf)
library(terra)

# Data Processing ---------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) %>%
  filter(shape_area >= 10000) %>%
  filter(!grepl("cemet", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("sport", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("farm", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("playground", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("golf", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("dog park", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("zoo", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("baseball", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("soccer", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("softball", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("athel", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("community center", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("school", park_name, ignore.case = TRUE))

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project(crs(cpad_super))

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

rm(strava_coarse_cropped)

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project(crs(cpad_super))

hfp <- resample(hfp, strava_coarse, method = "cubic")

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)

rm(hfp_cropped, hfp)

bio <- st_read(here::here("data", "raw_data", "ds2739.gdb")) %>%
  sf::st_transform(crs(cpad_super)) %>%
  select(NativeCount, RareCount) %>%
  janitor::clean_names()

bio_raster <- rasterize(bio, strava_coarse, field = "native_count")
rare_raster <- rasterize(bio, strava_coarse, field = "rare_count")

rm(bio, strava_coarse)

bio_cropped <- crop(bio_raster, cpad_super)
bio_masked <- mask(bio_cropped, cpad_super)

rm(bio_raster, bio_cropped)

rare_cropped <- crop(rare_raster, cpad_super)
rare_masked <- mask(rare_cropped, cpad_super)

rm(rare_raster, rare_cropped)

hfp <- extract(hfp_masked, cpad_super, xy = TRUE)
strava <- extract(strava_coarse_masked, cpad_super) %>%
  select(-ID)
bio <- extract(bio_masked, cpad_super) %>%
  select(-ID)
rare <- extract(rare_masked, cpad_super) %>%
  select(-ID)

rm(hfp_masked, strava_coarse_masked, bio_masked, rare_masked)

# Combine Data ------------------------------------------------------------

spatial_data <- list(hfp, strava, bio, rare) %>%
  reduce(cbind) %>%
  full_join(cpad_super) %>%
  st_drop_geometry() %>%
  filter(!is.na(strava_coarse_1)) %>%
  mutate(gray_scale = (0.299 * strava_coarse_1) + (0.587 * strava_coarse_2) + (0.114 * strava_coarse_3)) %>%
  mutate(
    hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255),
  ) %>%
  mutate(
    hex_int = strtoi(str_remove(hex_mean, "#"), 16L)
  ) %>%
  select(-hex_mean, -strava_coarse_1, -strava_coarse_2, -strava_coarse_3, -strava_coarse_4) %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  select(-ID, -geometry)

write_csv(spatial_data, here::here("data", "processed_data", "spatial_analysis.csv"))
