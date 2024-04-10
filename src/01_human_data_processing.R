library(tidyverse)
library(patchwork)
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

cpad_geom <- cpad_super %>%
  select(ID)

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
  rename(cbg = GEOID)

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

st_write(cpad_ghm,here::here("data", "processed_data", "cpad_ghm.shp"))

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
  summarize(across(HFP, list(min = min, mean = mean, max = max, median = median, sd = sd)))%>% 
  mutate(HFP_sd = case_when(
    !is.na(HFP_sd) ~ HFP_sd,
    is.na(HFP_sd) ~ 0
  ))

cpad_hfp <- full_join(cpad_super, cpad_hfp_summary) %>%
  mutate(
    HFP_range = HFP_max - HFP_min,
    HFP_cv = HFP_sd / HFP_mean
  )%>% 
  mutate(HFP_sd = case_when(
    !is.na(HFP_sd) ~ HFP_sd,
    is.na(HFP_sd) ~ 0
  ))%>% 
  mutate(HFP_cv = case_when(
    !is.na(HFP_cv) ~ HFP_cv,
    is.na(HFP_cv) ~ 0
  ))

st_write(cpad_hfp,here::here("data", "processed_data", "cpad_hfp.shp"))

rm(hfp_masked, cpad_hfp_values, cpad_hfp_summary, cpad_hfp)

# Strava ------------------------------------------------------------------

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_coarse_masked <- mask(strava_coarse, cpad_super)

# writeRaster(strava_coarse_masked, here::here("data", "raw_data", "strava_coarse_masked.tif"))

cpad_census_block = st_intersection(census_block, cpad_geom)

cpad_strava_coarse_values <- terra::extract(x = strava_coarse_masked, y = cpad_super)

cpad_strava_geom <- cpad_census_block %>%
  full_join(cpad_strava_coarse_values) %>%
  filter(!is.na(strava_coarse_1))

rm(strava_coarse_masked, strava_coarse, cpad_strava_coarse_values)

st_write(cpad_strava_geom, here::here("data", "strava_census.shp"))

rm(cpad_strava_geom, cpad_census_block)

strava_census_sum = strava_census %>%
  group_by(GEOID) %>% # NEED TRACT GROUPER HERE SOMETHING ELSE HERE BY BLOCK
  mutate(
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
  select(GEOID, ID, hex_mean, hex_min, hex_max, value) %>%
  distinct() %>% 
  mutate(
    hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L),
    hex_max_int = strtoi(str_remove(hex_max, "#"), 16L),
    hex_min_int = strtoi(str_remove(hex_min, "#"), 16L)
  )%>%
  mutate(hex_mean_int_pop = hex_mean_int / value) %>% 
  mutate(combine_area = units::drop_units(st_area(geometry))) %>%
  mutate(percent_pa = combine_area / shape_area) %>%
  mutate(hex_pop_area = weighted.mean(hex_mean_int_pop, percent_pa, na.rm = TRUE))

### SELECT AND DISTINCT VARIABLES TIL THERES JUST ONE PER PA

names(strava_census_sum)

cpad_strava <- full_join(cpad_super, strava_census_sum)

st_write(cpad_strava, here::here("cpad_strava.shp"), append = FALSE)


# Safegraph ---------------------------------------------------------------

list_of_files <- list.files(
  path = here::here("data", "raw_data", "counties-dates-2-10-22-reformatted", "cali"),
  recursive = TRUE,
  pattern = "\\.csv$",
  full.names = TRUE
)

safegraph = data.frame()

for(i in 1:length(list_of_files)) {
  file = read_csv(list_of_files[i])
  safegraph = rbind(safegraph, file)
}

safegraph = safegraph %>%
  mutate(year = lubridate::year(date)) %>%
  full_join(census_blockgroup) %>%
  select(-county_id, -date) %>% 
  st_as_sf() %>% 
  st_cast("MULTIPOLYGON")

rm(census_blockgroup, list_of_files)

safegraph_block <- st_intersection(census_block, safegraph) 

lapply(safegraph_block, class)

# st_write(safegraph_block, here::here("safegraph_cb.shp"), append = FALSE)

rm(safegraph)

safegraph_block_sum = safegraph_block %>%
  mutate(block_percent = cnss_b_ / cns_bg_) %>% 
  mutate(count = count * block_percent) %>% 
  group_by(GEOID, year, value) %>%
  summarise(
    mean_count = mean(count),
    sd_count = sd(count),
    max_count = max(count),
    min_count = min(count),
    cv_count = sd_count / mean_count
  )%>% 
  filter(!is.na(value)) %>% 
  filter(!is.na(mean_count)) %>% 
  mutate(pop_count = mean_count / value)

rm(safegraph_block)

lapply(safegraph_block_sum, class)

# st_write(safegraph_block_sum, here::here("safegraph_cb_sum.shp"), append = FALSE)

safegraph_pa <- st_intersection(cpad_geom, safegraph_block_sum) 

lapply(safegraph_pa, class)

# st_write(safegraph_pa, here::here("safegraph_pa.shp"), append = FALSE)

safegraph_pa_sum = safegraph_pa %>%
  mutate(combine_area = units::drop_units(st_area(geometry))) %>%
  mutate(percent_pa = combine_area / shape_area) %>%
  mutate(sg_pop_area = weighted.mean(pop_count, percent_pa, na.rm = TRUE))

names(safegraph_pa_sum)

cpad_sg <- full_join(cpad_super, safegraph_pa_sum)

st_write(cpad_sg, here::here("data", "cpad_sg.shp"))


# Final Data Processing ----------------------------------------------
