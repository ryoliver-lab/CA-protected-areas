library(tidyverse)
library(patchwork)
library(sf)
library(terra)

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")
hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
 project("EPSG:4326")

hfp_sampled = resample(hfp, ghm)

ext(ghm)
ext(hfp_sampled)

origin(ghm)
origin(hfp_sampled)

ghm_norm <- minmax(ghm)    
ghm_normalized <- (ghm - ghm_norm[1,]) / (ghm_norm[2,] - ghm_norm[1,])

hfp_norm <- minmax(hfp_sampled)    
hfp_normalized <- (hfp_sampled - hfp_norm[1,]) / (hfp_norm[2,] - hfp_norm[1,])

raster_diff = hfp_normalized - ghm_normalized

plot(raster_diff)

writeRaster(raster_diff, here::here("data", "processed_data", "ghm_hfp_diff.tif"))

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

diff_cropped = crop(raster_diff, cpad_super)

plot(diff_cropped)

diff_masked = mask(diff_cropped, cpad_super)

plot(diff_masked)
