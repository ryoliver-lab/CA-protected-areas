library(tidyverse)
library(patchwork)
library(sf)
library(terra)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

cpad_strava_coarse <- st_read(here::here("data", "processed_data", "cpad_strava_coarse.shp")) %>%
  sf::st_transform("EPSG:4326")

cpad_strava_fine <- st_read(here::here("data", "processed_data", "cpad_strava_fine.shp")) %>%
  sf::st_transform("EPSG:4326")

p1 = ggplot(cpad_strava_coarse) +
  geom_sf(aes(fill = hex_mean, color = hex_mean)) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values = as.character(cpad_strava_coarse$hex_mean)) +
  scale_fill_manual(values = as.character(cpad_strava_coarse$hex_mean))

ggsave(p1, file = paste0("strava_coarse.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# strava_fine = rast(here::here("data", "raw_data", "strava_fine_masked.tif"))

# plotRGB(strava_fine)

cpad_strava_fine_values <- terra::extract(x = strava_fine, y = cpad_super)

write_csv(cpad_strava_fine_values, here::here("data", "processed_data", "strava_fine_cpad.csv"))

cpad_strava_fine = cpad_strava_fine_values %>%
  filter(!is.na(strava_fine_1)) %>%
  group_by(ID) %>%
  summarize(mean_1 = mean(strava_fine_1),
         mean_2 = mean(strava_fine_2),
         mean_3 = mean(strava_fine_3),
         max_1 = max(strava_fine_1),
         max_2 = max(strava_fine_2),
         max_3 = max(strava_fine_3),
         min_1 = min(strava_fine_1),
         min_2 = min(strava_fine_2),
         min_3 = min(strava_fine_3)) %>%
  mutate(hex_mean = rgb(mean_1, mean_2, mean_3, maxColorValue = 255),
         hex_max = rgb(max_1, max_2, max_3, maxColorValue = 255),
         hex_min = rgb(min_1, min_2, min_3, maxColorValue = 255)) %>%
  select(ID, hex_mean, hex_min, hex_max)

cpad_strava_fine = full_join(cpad_super, cpad_strava_fine)

st_write(cpad_strava_fine, here::here("data", "processed_data", "cpad_strava_fine.shp"))

# strava_fine <- rast(here::here("data", "raw_data", "strava_fine.tif")) %>%
#   project("EPSG:4326")
# 
# strava_fine_masked = mask(strava_fine, cpad_super)
# 
# writeRaster(strava_fine_masked, here::here("data", "raw_data", "strava_fine_masked.tif"))

# strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse_masked.tif")) %>%
#   project("EPSG:4326")
# 
# plotRGB(strava_coarse)
# 
# cpad_strava_coarse_values <- terra::extract(x = strava_coarse, y = cpad_super)

# write_csv(cpad_strava_coarse_values, here::here("data", "processed_data", "strava_coarse_cpad.csv"))

# cpad_strava_coarse = cpad_strava_coarse_values %>%
#   filter(!is.na(strava_coarse_1)) %>% 
#   group_by(ID) %>% 
#   summarize(mean_1 = mean(strava_coarse_1),
#          mean_2 = mean(strava_coarse_2),
#          mean_3 = mean(strava_coarse_3),
#          max_1 = max(strava_coarse_1),
#          max_2 = max(strava_coarse_2),
#          max_3 = max(strava_coarse_3),
#          min_1 = min(strava_coarse_1),
#          min_2 = min(strava_coarse_2),
#          min_3 = min(strava_coarse_3)) %>% 
#   mutate(hex_mean = rgb(mean_1, mean_2, mean_3, maxColorValue = 255),
#          hex_max = rgb(max_1, max_2, max_3, maxColorValue = 255),
#          hex_min = rgb(min_1, min_2, min_3, maxColorValue = 255)) %>% 
#   select(ID, hex_mean, hex_min, hex_max) 

# cpad_strava_coarse = full_join(cpad_super, cpad_strava_coarse)

# st_write(cpad_strava_coarse, here::here("data", "processed_data", "cpad_strava_coarse.shp"))

# strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
#   project("EPSG:4326")
# 
# strava_coarse_masked = mask(strava_coarse, cpad_super)
# 
# writeRaster(strava_coarse_masked, here::here("data", "raw_data", "strava_coarse_masked.tif"))
