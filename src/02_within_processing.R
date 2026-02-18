library(tidyverse)
library(sf)
library(terra)

# Data Processing ---------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project(crs(cpad_super))

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

rm(strava_coarse_cropped)

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project(crs(cpad_super))

hfp <- resample(hfp, strava_coarse, method = "cubic")

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)

rm(hfp_cropped, hfp)

bio <- st_read(here::here("data", "raw_data", "Terrestrial_Biodiversity_Summary_-_ACE_[ds2739].shp")) %>%
  sf::st_transform(crs(cpad_super)) %>%
  select(NtvPlnt, RarPlnt, RarAmph, RarRept, RarBird, RarMamm, NtvAmph, NtvRept, NtvBird, NtvMamm) %>%
  mutate(
    ntv_anm = NtvAmph + NtvRept + NtvBird + NtvMamm,
    rare_anm = RarAmph + RarRept + RarBird + RarMamm
  ) %>%
  select(-RarAmph, -RarRept, -RarBird, -RarMamm, -NtvAmph, -NtvRept, -NtvBird, -NtvMamm) %>%
  janitor::clean_names()

bio_anm_raster <- rasterize(bio, strava_coarse, field = "ntv_anm")
rare_anm_raster <- rasterize(bio, strava_coarse, field = "rare_anm")

bio_plt_raster <- rasterize(bio, strava_coarse, field = "ntv_plnt")
rare_plt_raster <- rasterize(bio, strava_coarse, field = "rar_plnt")

rm(bio, strava_coarse)

bio_anm_cropped <- crop(bio_anm_raster, cpad_super)
bio_anm_masked <- mask(bio_anm_cropped, cpad_super)

bio_plt_cropped <- crop(bio_plt_raster, cpad_super)
bio_plt_masked <- mask(bio_plt_cropped, cpad_super)

rare_anm_cropped <- crop(rare_anm_raster, cpad_super)
rare_anm_masked <- mask(rare_anm_cropped, cpad_super)

rare_plt_cropped <- crop(rare_plt_raster, cpad_super)
rare_plt_masked <- mask(rare_plt_cropped, cpad_super)

rm(
  bio_anm_cropped, bio_anm_raster, rare_anm_cropped, rare_anm_raster,
  bio_plt_cropped, bio_plt_raster, rare_plt_cropped, rare_plt_raster
)

hfp <- extract(hfp_masked, cpad_super, xy = TRUE)
strava <- extract(strava_coarse_masked, cpad_super) %>%
  select(-ID)
bio_anm <- extract(bio_anm_masked, cpad_super) %>%
  select(-ID)
rare_anm <- extract(rare_anm_masked, cpad_super) %>%
  select(-ID)
bio_plt <- extract(bio_plt_masked, cpad_super) %>%
  select(-ID)
rare_plt <- extract(rare_plt_masked, cpad_super) %>%
  select(-ID)

rm(
  hfp_masked, strava_coarse_masked,
  bio_anm_masked, rare_anm_masked,
  bio_plt_masked, rare_plt_masked
)

# Combine Data ------------------------------------------------------------

spatial_data <- list(hfp, strava, bio_anm, rare_anm, bio_plt, rare_plt) %>%
  reduce(cbind) %>%
  filter(!is.na(strava_coarse_1)) %>%
  mutate(gray_scale = (0.299 * strava_coarse_1) + (0.587 * strava_coarse_2) + (0.114 * strava_coarse_3)) %>%
  select(-strava_coarse_1, -strava_coarse_2, -strava_coarse_3, -strava_coarse_4) %>%
  full_join(cpad_super %>% select(ID, suid_nma)) %>%
  st_drop_geometry() %>%
  select(-ID, -geometry)

write_csv(spatial_data, here::here("data", "processed_data", "spatial_analysis.csv"))
