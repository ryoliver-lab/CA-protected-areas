library(tidyverse)
library(ggridges)
library(sf)
library(terra)
library(patchwork)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:32610") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) %>% 
  filter(park_name != "BLM")%>%
  mutate(log_size = log10(shape_area)) %>% 
  filter(log_size >= 5)

cpad_buffer = st_buffer(cpad_super, dist = 10000) %>% 
  select(suid_nma)

ggplot(data = cpad_buffer) +
  geom_sf()

buffer = st_difference(cpad_buffer, cpad_super)

# GHM ---------------------------------------------------------------------

cpad_ghm <- rast(here::here("data", "processed_data", "gHM_masked.tif"))  %>%
  project("EPSG:4326")
cpad_ghm = terra::extract(cpad_ghm, cpad_super, xy=TRUE) %>% 
  filter(!is.na(gHM)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

ghm_cropped = crop(ghm, cpad_super)

ghm_masked = mask(ghm_cropped, cpad_buffer, inverse = TRUE)
ghm_masked = mask(ghm_masked, ca)

ghm_masked = terra::extract(ghm_masked, ca, xy=TRUE) %>% 
  filter(!is.na(gHM)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

rm(ghm, ghm_cropped)

ghm = list(ghm_masked, cpad_ghm) %>% 
  reduce(rbind) %>% 
  st_set_crs("EPSG:4326")

rm(ghm_masked, cpad_ghm)


# HFP ---------------------------------------------------------------------

cpad_hfp <- rast(here::here("data", "processed_data", "HFP2009_masked.tif")) %>%
  project("EPSG:4326")
cpad_hfp = terra::extract(cpad_hfp, cpad_super, xy=TRUE) %>% 
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

hfp_cropped = crop(hfp, cpad_super)

hfp_masked = mask(hfp_cropped, cpad_buffer, inverse = TRUE)
hfp_masked = mask(hfp_masked, ca)

hfp_masked = terra::extract(hfp_masked, ca, xy=TRUE) %>% 
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

rm(hfp, hfp_cropped)

hfp = list(hfp_masked, cpad_hfp) %>% 
  reduce(rbind) %>% 
  st_set_crs("EPSG:4326")

rm(hfp_masked, cpad_hfp)

# Strava ---------------------------------------------------------------------

cpad_strava <- rast(here::here("data", "raw_data", "strava_coarse_masked.tif")) %>%
  project("EPSG:4326")
cpad_strava = terra::extract(cpad_strava, cpad_super, xy=TRUE) %>% 
  filter(!is.na(strava_coarse_1)) %>% 
  mutate(hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255)) %>% 
  mutate(hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L)) %>% 
  select(hex_mean, hex_mean_int, x, y, ID) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

strava <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_cropped = crop(strava, cpad_super)

strava_masked = mask(strava_cropped, cpad_buffer, inverse = TRUE)
strava_masked = mask(strava_masked, ca)

strava_masked = terra::extract(strava_masked, ca, xy=TRUE) %>% 
  filter(!is.na(strava_coarse_1)) %>% 
  mutate(hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255)) %>% 
  mutate(hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L)) %>% 
  select(hex_mean, hex_mean_int, x, y, ID) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

rm(strava, strava_cropped)

strava = list(strava_masked, cpad_strava) %>% 
  reduce(rbind) %>% 
  st_set_crs("EPSG:4326")

rm(strava_masked, cpad_strava)