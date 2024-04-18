library(tidyverse)
library(sf)
library(terra)
library(tidycensus)
library(fuzzyjoin)

# census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

census_block <- get_decennial(
  state = "CA",
  geography = "block",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>%
  mutate(census_block_area = units::drop_units(st_area(geometry))) %>%
  select(-variable, -NAME) %>%
  sf::st_transform("EPSG:4326") %>%
  st_buffer(dist = 0)

block <- census_block %>%
  st_drop_geometry()

census_blockgroup <- get_decennial(
  state = "CA",
  geography = "cbg",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>%
  mutate(census_blockgroup_area = units::drop_units(st_area(geometry))) %>%
  select(-variable, -NAME) %>%
  sf::st_transform("EPSG:4326") %>%
  st_buffer(dist = 0) 

# Global Human Modification ----------------------------------------------

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

ghm_cropped <- crop(ghm, cpad_super)
ghm_masked <- mask(ghm_cropped, cpad_super)

rm(ghm, ghm_cropped)

cpad_ghm_values <- terra::extract(x = ghm_masked, y = cpad_super)

cpad_ghm_summary <- cpad_ghm_values %>%
  group_by(ID) %>%
  summarize(across(gHM, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm <- full_join(cpad_super, cpad_ghm_summary) %>%
  mutate(
    gHM_range = gHM_max - gHM_min,
    gHM_cv = gHM_sd / gHM_mean
  ) %>%
  mutate(gHM_sd = case_when(
    !is.na(gHM_sd) ~ gHM_sd,
    is.na(gHM_sd) ~ 0
  )) %>%
  mutate(gHM_cv = case_when(
    !is.na(gHM_cv) ~ gHM_cv,
    is.na(gHM_cv) ~ 0
  ))

st_write(cpad_ghm, here::here("data", "processed_data", "cpad_ghm.shp"))

rm(ghm_masked, cpad_ghm_values, cpad_ghm_summary, cpad_ghm)

# Human Footprint Index ---------------------------------------------------

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)

rm(hfp, hfp_cropped)

cpad_hfp_values <- terra::extract(x = hfp_masked, y = cpad_super)

cpad_hfp_summary <- cpad_hfp_values %>%
  rename(HFP = HFP2009) %>%
  group_by(ID) %>%
  summarize(across(HFP, list(min = min, mean = mean, max = max, median = median, sd = sd))) %>%
  mutate(HFP_sd = case_when(
    !is.na(HFP_sd) ~ HFP_sd,
    is.na(HFP_sd) ~ 0
  ))

cpad_hfp <- full_join(cpad_super, cpad_hfp_summary) %>%
  mutate(
    HFP_range = HFP_max - HFP_min,
    HFP_cv = HFP_sd / HFP_mean
  ) %>%
  mutate(HFP_sd = case_when(
    !is.na(HFP_sd) ~ HFP_sd,
    is.na(HFP_sd) ~ 0
  )) %>%
  mutate(HFP_cv = case_when(
    !is.na(HFP_cv) ~ HFP_cv,
    is.na(HFP_cv) ~ 0
  ))

st_write(cpad_hfp, here::here("data", "processed_data", "cpad_hfp.shp"))

rm(hfp_masked, cpad_hfp_values, cpad_hfp_summary, cpad_hfp)

# Strava ------------------------------------------------------------------

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_coarse_masked <- mask(strava_coarse, cpad_super)

# writeRaster(strava_coarse_masked, here::here("data", "raw_data", "strava_coarse_masked.tif"))

block_id_raster = rasterize(census_block, strava_coarse_masked, field = "GEOID")
block_id_masked <- mask(block_id_raster, cpad_super)

rm(block_id_raster)

block_size <- cellSize(block_id_masked, unit="m")
block_size_masked <- mask(block_size, cpad_super)

rm(block_size)

cpad_strava_coarse_values <- terra::extract(x = strava_coarse_masked, y = cpad_super)

rm(strava_coarse)

block_id_cpad = terra::extract(x = block_id_masked, y = cpad_super)

rm(block_id_masked)

block_area_cpad = terra::extract(x = block_size_masked, y = cpad_super)

rm(block_size_masked)

block_cpad = list(block_id_cpad, block, block_area_cpad) %>% 
  reduce(full_join) %>% 
  filter(!is.na(ID)) %>% 
  group_by(ID, GEOID, value) %>% 
  summarize(area = sum(area))

rm(block, block_id_cpad, block_area_cpad)

strava_block = full_join(cpad_strava_coarse_values, block_cpad) %>% 
  distinct()

rm(block_cpad, cpad_strava_coarse_values)

strava_block_sum = strava_block %>% 
  filter(!is.na(strava_coarse_1)) %>% 
  filter(value != 0) %>% 
  group_by(ID, GEOID, value, area) %>% 
  summarize(
    mean_1 = mean(strava_coarse_1),
    mean_2 = mean(strava_coarse_2),
    mean_3 = mean(strava_coarse_3),
    max_1 = max(strava_coarse_1),
    max_2 = max(strava_coarse_2),
    max_3 = max(strava_coarse_3),
    min_1 = min(strava_coarse_1),
    min_2 = min(strava_coarse_2),
    min_3 = min(strava_coarse_3)
  ) %>%
  mutate(
    hex_mean = rgb(mean_1, mean_2, mean_3, maxColorValue = 255),
    hex_max = rgb(max_1, max_2, max_3, maxColorValue = 255),
    hex_min = rgb(min_1, min_2, min_3, maxColorValue = 255)
  ) %>%
  distinct() %>% 
  mutate(
    hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L),
    hex_max_int = strtoi(str_remove(hex_max, "#"), 16L),
    hex_min_int = strtoi(str_remove(hex_min, "#"), 16L)
  ) %>%
  mutate(hex_mean_int_pop = hex_mean_int / value,
         hex_max_int_pop = hex_max_int / value,
         hex_min_int_pop = hex_min_int / value) %>% 
  select(GEOID, ID, value, area, hex_mean_int_pop, hex_max_int_pop, hex_min_int_pop)

rm(strava_block)

cpad_strava <- full_join(cpad_super, strava_block_sum)%>%
  mutate(percent_pa = area / shape_area) %>%
  group_by(ID) %>% 
  mutate(hex_pop_area = weighted.mean(hex_mean_int_pop, percent_pa, na.rm = TRUE),
         max_hex_pop_area = weighted.mean(hex_max_int_pop, percent_pa, na.rm = TRUE),
         min_hex_pop_area = weighted.mean(hex_min_int_pop, percent_pa, na.rm = TRUE)) %>% 
  select(-GEOID, -ID, -value, -area, -hex_mean_int_pop, -hex_max_int_pop,-hex_min_int_pop,-percent_pa) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  full_join(cpad_super)

rm(strava_block_sum)

st_write(cpad_strava, here::here("data", "processed_data", "cpad_strava.shp"), append = FALSE)


# Safegraph ---------------------------------------------------------------
