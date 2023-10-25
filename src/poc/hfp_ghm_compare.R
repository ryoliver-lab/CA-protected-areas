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
