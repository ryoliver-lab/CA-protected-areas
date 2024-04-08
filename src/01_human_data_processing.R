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
hfp_masked <- mask(hfp_cropped, hfp_super)

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

cpad_strava_coarse_values <- terra::extract(x = strava_coarse_masked, y = cpad_super)

cpad_strava_geom <- cpad_geom %>%
  full_join(cpad_strava_coarse_values)%>%
  filter(!is.na(strava_coarse_1))

rm(strava_coarse_masked, strava_coarse, cpad_strava_coarse_values)

#### UPDATE


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
  st_as_sf()

rm(file)


### UPDATE

# Final Data Processing ----------------------------------------------
