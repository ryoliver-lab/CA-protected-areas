library(tidyverse)
library(sf)
library(terra)

sf_use_s2(FALSE)

# census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

# Strava ------------------------------------------------------------------

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project(crs(cpad_super))

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

rm(strava_coarse_cropped)

cpad_strava_coarse_values <- terra::extract(x = strava_coarse_masked, y = cpad_super)

strava <- cpad_strava_coarse_values %>%
  select(-ID)

strava_sum <- cpad_strava_coarse_values %>%
  group_by(ID) %>%
  summarize(
    mean_1 = median(strava_coarse_1, na.rm = TRUE),
    mean_2 = median(strava_coarse_2, na.rm = TRUE),
    mean_3 = median(strava_coarse_3, na.rm = TRUE),
  ) %>%
  distinct() %>%
  filter(!is.na(mean_1)) %>%
  mutate(gray_scale = (0.299 * mean_1) + (0.587 * mean_2) + (0.114 * mean_3)) %>%
  select(ID, gray_scale)

cpad_strava <- full_join(cpad_super %>% select(ID, suid_nma), strava_sum) %>%
  select(-ID) %>% 
  distinct() 

st_write(cpad_strava, here::here("data", "processed_data", "cpad_strava.shp"), append = FALSE)

rm(strava_sum, strava_coarse_masked, cpad_strava_coarse_values)

# Human Footprint Index ---------------------------------------------------

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project(crs(cpad_super))

hfp <- resample(hfp, strava_coarse, method = "cubic")

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)

rm(hfp_cropped, hfp)

cpad_hfp_values <- terra::extract(x = hfp_masked, y = cpad_super)

hfp <- extract(hfp_masked, cpad_super, xy = TRUE)

cpad_hfp <- cpad_hfp_values %>%
  rename(HFP = HFP2009) %>%
  group_by(ID) %>%
  summarize(across(HFP, list(median = median))) %>%
  full_join(cpad_super) 

st_write(cpad_hfp, here::here("data", "processed_data", "cpad_hfp.shp"), append = FALSE)

rm(hfp_masked, cpad_hfp_values, cpad_hfp_summary)

# Biodiversity ------------------------------------------------------------

bio <- st_read(here::here("data", "raw_data", "Terrestrial_Biodiversity_Summary_-_ACE_[ds2739].shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super)) %>%
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

rm(strava_coarse, bio)

bio_cropped_anm <- crop(bio_anm_raster, cpad_super)
bio_masked_anm <- mask(bio_cropped_anm, cpad_super)

rare_cropped_anm <- crop(rare_anm_raster, cpad_super)
rare_masked_anm <- mask(rare_cropped_anm, cpad_super)

bio_cropped_plt <- crop(bio_plt_raster, cpad_super)
bio_masked_plt <- mask(bio_cropped_plt, cpad_super)

rare_cropped_plt <- crop(rare_plt_raster, cpad_super)
rare_masked_plt <- mask(rare_cropped_plt, cpad_super)

rm(
  bio_cropped_anm, bio_anm_raster, rare_cropped_anm, rare_anm_raster,
  bio_cropped_plt, bio_plt_raster, rare_cropped_plt, rare_plt_raster
)

cpad_bio_anm <- terra::extract(x = bio_masked_anm, y = cpad_super, ID = TRUE)
cpad_rare_anm <- terra::extract(x = rare_masked_anm, y = cpad_super, ID = TRUE)%>%
  select(-ID)
cpad_bio_plt <- terra::extract(x = bio_masked_plt, y = cpad_super, ID = TRUE) %>%
  select(-ID)
cpad_rare_plt <- terra::extract(x = rare_masked_plt, y = cpad_super, ID = TRUE)%>%
  select(-ID)

rm(bio_masked_anm, rare_masked_anm, bio_masked_plt, rare_masked_plt)

bio_anm <- cpad_bio_anm %>%
  select(-ID)
rare_anm <- cpad_rare_anm 
bio_plt <- cpad_bio_plt
rare_plt <- cpad_rare_plt

cpad_bio = list(cpad_bio_plt, cpad_rare_plt, cpad_bio_anm, cpad_rare_anm) %>% 
  reduce(cbind) %>%
  mutate(native = ntv_anm + ntv_plnt,
         rare = rar_plnt + rare_anm) %>% 
  group_by(ID) %>%
  summarize(ntv_med = median(native, na.rm=TRUE),
            rare_med = median(rare, na.rm=TRUE)) %>% 
  full_join(cpad_super %>% select(suid_nma, ID)) %>% 
  select(-ID) %>% 
  st_as_sf() 

st_write(cpad_bio, here::here("data", "processed_data", "cpad_bio.shp"), append = FALSE)

# Pixel Data combine ------------------------------------------------------

spatial_data <- list(hfp, strava, bio_anm, rare_anm, bio_plt, rare_plt) %>%
  reduce(cbind) %>%
  filter(!is.na(strava_coarse_1)) %>%
  mutate(gray_scale = (0.299 * strava_coarse_1) + (0.587 * strava_coarse_2) + (0.114 * strava_coarse_3)) %>%
  select(-strava_coarse_1, -strava_coarse_2, -strava_coarse_3, -strava_coarse_4) %>%
  full_join(cpad_super %>% select(ID, suid_nma)) %>%
  st_drop_geometry() %>%
  select(-ID, -geometry)

write_csv(spatial_data, here::here("data", "processed_data", "spatial_analysis.csv"))

human <- list(cpad_hfp, cpad_strava %>% st_drop_geometry(), cpad_bio %>% st_drop_geometry()) %>%
  reduce(full_join) %>%
  st_as_sf()

st_write(human, here::here("data", "processed_data", "human.shp"), append = FALSE)
