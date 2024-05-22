library(tidyverse)
library(sf)
library(terra)
library(tidycensus)

# census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

ghm_cropped <- crop(ghm, cpad_super)
ghm_masked <- mask(ghm_cropped, cpad_super)

cpad_raster <- rasterize(cpad_super, ghm_masked, field = "suid_nma")

rm(ghm, ghm_cropped, ghm_masked)

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

rm(strava_coarse, strava_coarse_cropped)

strava_coarse_masked <- resample(strava_coarse_masked, cpad_raster, method = "cubic")

census_blockgroup <- get_decennial(
  state = "CA",
  geography = "cbg",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>%
  mutate(census_blockgroup_area = units::drop_units(st_area(geometry))) %>%
  select(-variable, -NAME, -value) %>%
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
  mutate(cbg = substring(GEOID, 1, 12)) %>% 
  select(-GEOID) %>% 
  st_drop_geometry()

census = full_join(census_block, census_blockgroup)

rm(census_block, census_blockgroup)

list_of_files <- list.files(
  path = here::here("data", "raw_data", "counties-dates-2-10-22-reformatted", "cali"),
  recursive = TRUE,
  pattern = "\\.csv$",
  full.names = TRUE
)

safegraph <- read_csv(list_of_files, id = "file_name") %>% 
  select(-file_name) %>% 
  mutate(year = lubridate::year(date)) %>%
  full_join(census) 

rm(list_of_files, census)

safegraph_block_sum = safegraph %>%
  mutate(block_percent = census_block_area / census_blockgroup_area) %>% 
  mutate(count = count * block_percent,
         value = value * block_percent) %>% 
  select(-census_blockgroup_area) %>% 
  mutate(value = case_when(
    value == 0 ~ 1,
    TRUE ~ value,
  )) %>% 
  mutate(pop_count = count / value) %>% 
  select(-count, -value) %>% 
  distinct() %>% 
  filter(!is.na(pop_count)) %>% 
  as.data.frame()

rm(safegraph)

pop_raster <- rasterize(safegraph_block_sum, cpad_raster, field = "pop_count")
pop_masked <- mask(pop_raster, cpad_super)

pop_masked <- resample(pop_masked, cpad_raster, method = "cubic")

rm(pop_raster)

max_raster <- rasterize(safegraph_block_sum, cpad_raster, field = "max_pop_count")
max_masked <- mask(max_raster, cpad_super)

max_masked <- resample(max_masked, cpad_raster, method = "cubic")

rm(max_raster)

min_raster <- rasterize(safegraph_block_sum, cpad_raster, field = "min_pop_count")
min_masked <- mask(min_raster, cpad_super)

min_masked <- resample(min_masked, cpad_raster, method = "cubic")

rm(min_raster, safegraph_block_sum)

block_size2 <- cellSize(pop_masked, unit = "m")
block_size_masked2 <- mask(block_size2, cpad_super)

block_size_masked2 <- resample(block_size_masked2, cpad_raster, method = "cubic")

rm(block_size2, cpad_raster)

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

