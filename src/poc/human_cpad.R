library(tidyverse)
library(patchwork)
library(sf)
library(terra)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

strava <- rast(here::here("data", "raw_data", "strava_masked.tif")) %>%
  project("EPSG:4326")

strava_csv = read_csv(here::here("data", "processed_data", "strava.csv")) %>%
  drop_na()

cpad_strava = cpad_super %>% 
  st_drop_geometry() %>% 
  select(access_typ, layer, mng_agncy, park_name, ID) %>% 
  full_join(strava)
  
access_sum <- cpad_strava %>%
  group_by(access_typ) %>%
  summarize(
    mean_strava1 = mean(strava_heatmap_1, na.rm = TRUE),
    min_strava1 = min(strava_heatmap_1, na.rm = TRUE),
    max_strava1 = max(strava_heatmap_1, na.rm = TRUE),
    mean_strava2 = mean(strava_heatmap_2, na.rm = TRUE),
    min_strava2 = min(strava_heatmap_2, na.rm = TRUE),
    max_strava2 = max(strava_heatmap_2, na.rm = TRUE),
    mean_strava3 = mean(strava_heatmap_3, na.rm = TRUE),
    min_strava3 = min(strava_heatmap_3, na.rm = TRUE),
    max_strava3 = max(strava_heatmap_3, na.rm = TRUE)
  ) %>% 
  mutate(mean_hex_col = rgb(mean_strava1, mean_strava2, mean_strava3, maxColorValue = 255),
         min_hex_col = rgb(min_strava1, min_strava2, min_strava3, maxColorValue = 255),
         max_hex_col = rgb(max_strava1, max_strava2, max_strava3, maxColorValue = 255))

write_csv(access_sum, here::here("analysis", "strava_access_summary.csv"))

layer_sum <- cpad_strava %>%
  group_by(layer) %>%
  summarize(
    mean_strava1 = mean(strava_heatmap_1, na.rm = TRUE),
    min_strava1 = min(strava_heatmap_1, na.rm = TRUE),
    max_strava1 = max(strava_heatmap_1, na.rm = TRUE),
    mean_strava2 = mean(strava_heatmap_2, na.rm = TRUE),
    min_strava2 = min(strava_heatmap_2, na.rm = TRUE),
    max_strava2 = max(strava_heatmap_2, na.rm = TRUE),
    mean_strava3 = mean(strava_heatmap_3, na.rm = TRUE),
    min_strava3 = min(strava_heatmap_3, na.rm = TRUE),
    max_strava3 = max(strava_heatmap_3, na.rm = TRUE)
  ) %>% 
  mutate(mean_hex_col = rgb(mean_strava1, mean_strava2, mean_strava3, maxColorValue = 255),
         min_hex_col = rgb(min_strava1, min_strava2, min_strava3, maxColorValue = 255),
         max_hex_col = rgb(max_strava1, max_strava2, max_strava3, maxColorValue = 255))

write_csv(layer_sum, here::here("analysis", "strava_mgn_type_summary.csv"))

mng_sum <- cpad_strava %>%
  group_by(mng_agncy) %>%
  summarize(
    mean_strava1 = mean(strava_heatmap_1, na.rm = TRUE),
    min_strava1 = min(strava_heatmap_1, na.rm = TRUE),
    max_strava1 = max(strava_heatmap_1, na.rm = TRUE),
    mean_strava2 = mean(strava_heatmap_2, na.rm = TRUE),
    min_strava2 = min(strava_heatmap_2, na.rm = TRUE),
    max_strava2 = max(strava_heatmap_2, na.rm = TRUE),
    mean_strava3 = mean(strava_heatmap_3, na.rm = TRUE),
    min_strava3 = min(strava_heatmap_3, na.rm = TRUE),
    max_strava3 = max(strava_heatmap_3, na.rm = TRUE)
  ) %>% 
  mutate(mean_hex_col = rgb(mean_strava1, mean_strava2, mean_strava3, maxColorValue = 255),
         min_hex_col = rgb(min_strava1, min_strava2, min_strava3, maxColorValue = 255),
         max_hex_col = rgb(max_strava1, max_strava2, max_strava3, maxColorValue = 255))

write_csv(mng_sum, here::here("analysis", "strava_mng_agency_summary.csv"))

park_sum <- cpad_strava %>%
  group_by(park_name) %>%
  summarize(
    mean_strava1 = mean(strava_heatmap_1, na.rm = TRUE),
    min_strava1 = min(strava_heatmap_1, na.rm = TRUE),
    max_strava1 = max(strava_heatmap_1, na.rm = TRUE),
    mean_strava2 = mean(strava_heatmap_2, na.rm = TRUE),
    min_strava2 = min(strava_heatmap_2, na.rm = TRUE),
    max_strava2 = max(strava_heatmap_2, na.rm = TRUE),
    mean_strava3 = mean(strava_heatmap_3, na.rm = TRUE),
    min_strava3 = min(strava_heatmap_3, na.rm = TRUE),
    max_strava3 = max(strava_heatmap_3, na.rm = TRUE)
  ) %>% 
  mutate(mean_hex_col = rgb(mean_strava1, mean_strava2, mean_strava3, maxColorValue = 255),
         min_hex_col = rgb(min_strava1, min_strava2, min_strava3, maxColorValue = 255),
         max_hex_col = rgb(max_strava1, max_strava2, max_strava3, maxColorValue = 255))

write_csv(park_sum, here::here("analysis", "strava_park_summary.csv"))

ggplot(access_sum) +
  geom_point(aes(access_typ, mean_strava1, color = mean_hex_col)) +
  theme_bw() +
  scale_color_manual(values = c(access_sum$mean_hex_col))

ggplot(layer_sum) +
  geom_point(aes(layer, mean_strava1, color = mean_hex_col)) +
  theme_bw() +
  scale_color_manual(values = c(layer_sum$mean_hex_col))

ggplot(mng_sum) +
  geom_point(aes(mng_agncy, mean_strava1, color = mean_hex_col))+
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values = c(mng_sum$mean_hex_col))

ggplot(park_sum) +
  geom_point(aes(park_name, mean_strava1, color = mean_hex_col)) +
  theme_bw() +
  scale_color_manual(values = c(park_sum$mean_hex_col)) +
  theme(legend.position = "none")

# strava <- rast(here::here("data", "raw_data", "strava_heatmap.tif")) %>%
#   project("EPSG:4326")
# 
# strava_masked = mask(strava, cpad_super)
# 
# plot(strava_masked)
# 
# writeRaster(strava_masked, here::here("data", "raw_data", "strava_masked.tif"))


# 
# cpad_strava_values <- terra::extract(x = strava, y = cpad_super)

#write_csv(cpad_strava_values, here::here("data", "processed_data", "strava_cpad.csv"))

# strava = strava %>% 
#   mutate(hex_col = rgb(strava_heatmap_1, strava_heatmap_2, strava_heatmap_3, maxColorValue = 255))
# 
# write_csv(strava, here::here("data", "processed_data", "strava_cpad.csv"))
