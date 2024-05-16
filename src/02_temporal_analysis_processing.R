library(tidyverse)
library(sf)
library(terra)
library(tidycensus)

# census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

# Data Processing ---------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_coarse_masked <- mask(strava_coarse, cpad_super)

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
  st_buffer(dist = 0) %>% 
  mutate(cbg = GEOID)

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
  st_buffer(dist = 0) %>% 
  mutate(cbg = substring(GEOID, 1, 12))

list_of_files <- list.files(
  path = here::here("data", "raw_data", "counties-dates-2-10-22-reformatted", "cali"),
  recursive = TRUE,
  pattern = "\\.csv$",
  full.names = TRUE
)

safegraph <- read_csv(list_of_files, id = "file_name") %>% 
  select(-file_name) %>% 
  mutate(year = lubridate::year(date)) %>%
  full_join(census_blockgroup) 

safegraph_block <- safegraph_sum %>% 
  full_join(census_block) %>% 
  st_as_sf() %>% 
  select(-cbg)

rm(census_block, census_blockgroup)

safegraph_block_sum = safegraph_block %>%
  mutate(block_percent = census_block_area / census_blockgroup_area) %>% 
  mutate(mean_count = mean_count * block_percent,
         value = value * block_percent) %>% 
  filter(!is.na(value)) %>% 
  filter(!is.na(count)) %>% 
  select(-census_blockgroup_area) %>% 
  mutate(value = case_when(
    value == 0 ~ 1,
    TRUE ~ value,
  )) %>% 
  mutate(pop_count = count / value) %>% 
  select(-count, -value)

rm(safegraph_block)

pop_raster = rasterize(safegraph_block_sum, strava_coarse_masked, field = "pop_count")
pop_masked <- mask(pop_raster, cpad_super)

rm(pop_raster)

max_raster = rasterize(safegraph_block_sum, strava_coarse_masked, field = "max_pop_count")
max_masked  <- mask(max_raster, cpad_super)

rm(max_raster)

min_raster  = rasterize(safegraph_block_sum, strava_coarse_masked, field = "min_pop_count")
min_masked  <- mask(min_raster, cpad_super)

rm(min_raster, safegraph_block_sum, strava_coarse_masked)

block_size2 <- cellSize(pop_masked, unit="m")
block_size_masked2 <- mask(block_size2, cpad_super)

rm(block_size2)

pop_cpad = terra::extract(x = pop_masked, y = cpad_super) %>% 
  rename(pop = pop_count)

rm(pop_masked)

max_cpad = terra::extract(x = max_masked, y = cpad_super) %>% 
  rename(max_pop = max_pop_count) %>% 
  select(-ID)

rm(max_masked)

min_cpad = terra::extract(x = min_masked, y = cpad_super) %>% 
  rename(min_pop = min_pop_count) %>% 
  select(-ID)

rm(min_masked)

area_cpad = terra::extract(x = block_size_masked2, y = cpad_super) %>% 
  select(-ID)

rm(block_size_masked2)

safegraph_cpad = list(area_cpad, max_cpad, min_cpad, pop_cpad) %>% 
  reduce(cbind) %>% 
  full_join(cpad_super) %>% 
  st_drop_geometry()

rm(area_cpad, max_cpad, min_cpad, pop_cpad)

safegraph_pa_sum = safegraph_cpad %>%
  mutate(percent_pa = area / shape_area) %>% 
  group_by(ID, suid_nma) %>% 
  summarize(sg = weighted.mean(pop, percent_pa, na.rm = TRUE),
            max_sg = weighted.mean(max_pop, percent_pa, na.rm = TRUE),
            min_sg = weighted.mean(min_pop, percent_pa, na.rm = TRUE))

