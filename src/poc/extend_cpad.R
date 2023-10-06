library(tidyverse)
library(patchwork)
library(sf)
library(terra)

# Spatial Processing ------------------------------------------------------
cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())
# 
# ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
#   project("EPSG:4326")

# ggplot(data = cpad_super) +
#   geom_sf(aes(color = access_typ, fill = access_typ)) +
#   theme_bw() +
#   theme(legend.position = "none")

# plot(ghm)
#
# ghm_cropped = crop(ghm, cpad_super)
#
# plot(ghm_cropped)
#
# ghm_masked = mask(ghm_cropped, cpad_super)
#
# plot(ghm_masked)

# writeRaster(ghm_masked, here::here("data", "raw_data", "gHM_masked.tif"))

ghm <- rast(here::here("data", "raw_data", "gHM_masked.tif")) %>%
  project("EPSG:4326")

cpad_ghm_values = terra::extract(x = ghm, y = cpad_super)

cpad_ghm_summary = cpad_ghm_values %>%
  group_by(ID) %>%
  summarize(across(gHM, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm = full_join(cpad_super, cpad_ghm_summary) %>%
  mutate(gHM_range = gHM_max - gHM_min)

# st_write(cpad_ghm, here::here("data", "processed_data", "cpad_gHM.shp"))

# cpad_ghm <- st_read(here::here("data", "processed_data", "cpad_gHM.shp"))

# Plotting ----------------------------------------------------------------
p1 <- ggplot(data = cpad_ghm) +
  geom_sf(aes(color = gHM_min, fill = gHM_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_ghm) +
  geom_sf(aes(color = gHM_mean, fill = gHM_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_ghm) +
  geom_sf(aes(color = gHM_max, fill = gHM_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_ghm) +
  geom_sf(aes(color = gHM_median, fill = gHM_median)) +
  theme_bw() 

p5 <- ggplot(data = cpad_ghm) +
  geom_sf(aes(color = gHM_range, fill = gHM_range)) +
  theme_bw() 

p6 <- ggplot(data = cpad_ghm) +
  geom_sf(aes(color = gHM_sd, fill = gHM_sd)) +
  theme_bw() 

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.pdf"), path = here::here("analysis"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.pdf"), path = here::here("analysis"), height = 10, width = 12)

# Data Summaries ----------------------------------------------------------

cpad_values = cpad_ghm_values %>% 
  drop_na()

cpad_values = full_join(cpad_values, cpad_super) %>% 
  select(-ID) %>% 
  st_drop_geometry()

access_sum = cpad_values %>% 
  group_by(access_typ) %>%
  summarize(mean_gHM = mean(gHM, na.rm = TRUE),
            min_gHM = min(gHM, na.rm = TRUE),
            max_gHM = max(gHM, na.rm = TRUE),
            median_gHM = median(gHM, na.rm = TRUE))

write_csv(access_sum, here::here("analysis", "HDI_access_summary.csv"))

layer_sum = cpad_values %>% 
  group_by(layer) %>%
  summarize(mean_gHM = mean(gHM, na.rm = TRUE),
            min_gHM = min(gHM, na.rm = TRUE),
            max_gHM = max(gHM, na.rm = TRUE),
            median_gHM = median(gHM, na.rm = TRUE))

write_csv(layer_sum, here::here("analysis", "HDI_mgn_type_summary.csv"))

mng_sum = cpad_values %>% 
  group_by(mng_agncy) %>%
  summarize(mean_gHM = mean(gHM, na.rm = TRUE),
            min_gHM = min(gHM, na.rm = TRUE),
            max_gHM = max(gHM, na.rm = TRUE),
            median_gHM = median(gHM, na.rm = TRUE))

write_csv(mng_sum, here::here("analysis", "HDI_mng_agency_summary.csv"))

park_sum = cpad_values %>% 
  group_by(park_name) %>%
  summarize(mean_gHM = mean(gHM, na.rm = TRUE),
            min_gHM = min(gHM, na.rm = TRUE),
            max_gHM = max(gHM, na.rm = TRUE),
            median_gHM = median(gHM, na.rm = TRUE))

write_csv(park_sum, here::here("analysis", "HDI_park_summary.csv"))

ggplot(data = cpad_values) +
  geom_histogram(aes(x = gHM)) +
  theme_bw()

p7 = ggplot(data = cpad_values) +
  geom_boxplot(aes(x = access_typ, y = gHM)) +
  theme_bw() +
  coord_flip() +
  labs(y = "Human Modification Index",
       x = "Access Level")

p8 = ggplot(data = cpad_values) +
  geom_boxplot(aes(x = layer, y = gHM), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(y = "Human Modification Index",
       x = "Agency Type")

plot3 = p7 + p8

ggsave(plot3, file = paste0("plot3.pdf"), path = here::here("analysis"), height = 10, width = 12)
