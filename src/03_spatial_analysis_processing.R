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

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

ghm_cropped <- crop(ghm, cpad_super)
ghm_masked <- mask(ghm_cropped, cpad_super)

rm(ghm, ghm_cropped)

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)

rm(hfp, hfp_cropped)

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

cpad_raster <- rasterize(cpad_super, strava_coarse_masked, field = "ID")

ghm <- resample(ghm_masked, cpad_raster, metho = "cubic")
hfp <- resample(hfp_masked, cpad_raster, metho = "cubic")
strava <- resample(strava_coarse_masked, cpad_raster, metho = "cubic")

cpad <- cpad_raster %>%
  as.data.frame(xy = TRUE)
ghm <- ghm %>%
  as.data.frame(xy = TRUE)
hfp <- hfp %>%
  as.data.frame(xy = TRUE)
strava <- strava %>%
  as.data.frame(xy = TRUE)

spatial_data <- list(cpad, ghm, hfp, strava) %>%
  reduce(full_join) %>%
  mutate(
    hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255),
  ) %>%
  mutate(
    hex_int = strtoi(str_remove(hex_mean, "#"), 16L)
  ) %>%
  select(-hex_mean, -strava_coarse_1, -strava_coarse_2, -strava_coarse_3, -strava_coarse_4) %>% 
  group_by(ID) %>% 
  mutate(count = n()) %>% 
  filter(count > 2)

write_csv(spatial_data, here::here("data", "processed_data", "spatial_analysis.csv"))
