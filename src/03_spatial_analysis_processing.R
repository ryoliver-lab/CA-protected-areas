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

# ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
#   project("EPSG:4326")
# 
# ghm_cropped <- crop(ghm, cpad_super)
# ghm_masked <- mask(ghm_cropped, cpad_super)
# 
# rm(ghm, ghm_cropped)

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)

rm(hfp_cropped)

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")
res(strava_coarse)

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

cpad_raster <- rasterize(cpad_super, hfp_masked, field = "suid_nma")

bio = st_read(here::here("data", "raw_data", "ds2739.gdb")) %>% 
  sf::st_transform("EPSG:4326") %>% 
  select(NativeCount, RareCount) %>% 
  janitor::clean_names()

bio_raster <- rasterize(bio, hfp, field = "native_count")
rare_raster <- rasterize(bio, hfp, field = "rare_count")

bio_cropped <- crop(bio_raster, cpad_super)
bio_masked <- mask(bio_cropped, cpad_super)

rare_cropped <- crop(rare_raster, cpad_super)
rare_masked <- mask(rare_cropped, cpad_super)

# ghm_masked <- resample(ghm_masked, cpad_raster, metho = "cubic")
hfp_masked <- resample(hfp_masked, cpad_raster, metho = "cubic")
strava_coarse_masked <- resample(strava_coarse_masked, cpad_raster, metho = "cubic")
bio_masked <- resample(bio_masked, cpad_raster, metho = "cubic")
rare_masked <- resample(rare_masked, cpad_raster, metho = "cubic")

# ghm <- extract(ghm_masked, cpad_super)
hfp <- extract(hfp_masked, cpad_super)
strava <- extract(strava_coarse_masked, cpad_super, xy=TRUE) %>% 
  mutate(strava_coarse_1 = case_when(
    strava_coarse_1 < 0 ~ 0,
    strava_coarse_1 > 255 ~ 255,
    TRUE ~ strava_coarse_1
  )) %>% 
  mutate(strava_coarse_2 = case_when(
    strava_coarse_2 < 0 ~ 0,
    strava_coarse_2 > 255 ~ 255,
    TRUE ~ strava_coarse_2
  )) %>% 
  mutate(strava_coarse_3 = case_when(
    strava_coarse_3 < 0 ~ 0, 
    strava_coarse_3 > 255 ~ 255,
    TRUE ~ strava_coarse_3
  )) %>% 
  select(-ID)
bio <- extract(bio_masked, cpad_super) %>% 
  select(-ID)
rare <- extract(rare_masked, cpad_super) %>% 
  select(-ID)


# Combine Data ------------------------------------------------------------

spatial_data <- list(hfp, strava, bio, rare) %>%
  reduce(cbind) %>% 
  full_join(cpad_super) %>%
  st_drop_geometry() %>% 
  filter(!is.na(strava_coarse_1)) %>% 
  mutate(
    hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255),
  ) %>%
  mutate(
    hex_int = strtoi(str_remove(hex_mean, "#"), 16L)
  ) %>%
  select(-hex_mean, -strava_coarse_1, -strava_coarse_2, -strava_coarse_3, -strava_coarse_4) %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>% 
  select(-ID, -geometry) %>% 
  filter(count >= 2)

write_csv(spatial_data, here::here("data", "processed_data", "spatial_analysis.csv"))
