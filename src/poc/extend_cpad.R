library(tidyverse)
library(patchwork)
library(sf)
library(terra)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

cpad_ghm <- st_read(here::here("data", "processed_data", "cpad_join3.shp")) %>%
  sf::st_transform("EPSG:4326")

# ggplot(data = cpad_super) +
#   geom_sf(aes(color = access_typ, fill = access_typ)) +
#   theme_bw() +
#   theme(legend.position = "none")

# GHM Layer 1 ------------------------------------------------------

#
# ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
#   project("EPSG:4326")

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

cpad_ghm_values <- terra::extract(x = ghm, y = cpad_super)

cpad_ghm_summary <- cpad_ghm_values %>%
  group_by(ID) %>%
  summarize(across(gHM, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm <- full_join(cpad_super, cpad_ghm_summary) %>%
  mutate(gHM_range = gHM_max - gHM_min)

# st_write(cpad_ghm, here::here("data", "processed_data", "cpad_gHM.shp"))

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


cpad_values <- cpad_ghm_values %>%
  drop_na()

cpad_values <- full_join(cpad_values, cpad_super) %>%
  select(-ID) %>%
  st_drop_geometry()

access_sum <- cpad_values %>%
  group_by(access_typ) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  )

write_csv(access_sum, here::here("analysis", "HDI_access_summary.csv"))

layer_sum <- cpad_values %>%
  group_by(layer) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  )

write_csv(layer_sum, here::here("analysis", "HDI_mgn_type_summary.csv"))

mng_sum <- cpad_values %>%
  group_by(mng_agncy) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  )

write_csv(mng_sum, here::here("analysis", "HDI_mng_agency_summary.csv"))

park_sum <- cpad_values %>%
  group_by(park_name) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  )

write_csv(park_sum, here::here("analysis", "HDI_park_summary.csv"))

ggplot(data = cpad_values) +
  geom_histogram(aes(x = gHM)) +
  theme_bw()

p7 <- ggplot(data = cpad_values) +
  geom_boxplot(aes(x = access_typ, y = gHM)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Access Level"
  )

p8 <- ggplot(data = cpad_values) +
  geom_boxplot(aes(x = layer, y = gHM), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Agency Type"
  )

plot3 <- p7 + p8

ggsave(plot3, file = paste0("plot3.pdf"), path = here::here("analysis"), height = 10, width = 12)

# GHM Layer 2 -------------------------------------------------------------

ghm2 <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

# ghm_cropped2 = crop(ghm2, cpad_super)
#
# plot(ghm_cropped2)
#
# ghm_masked2 = mask(ghm_cropped2, cpad_super)
#
# plot(ghm_masked2)

# writeRaster(ghm_masked2, here::here("data", "raw_data", "HFP2009_masked.tif"))

ghm2 <- rast(here::here("data", "raw_data", "HFP2009_masked.tif")) %>%
  project("EPSG:4326")

cpad_ghm_values2 <- terra::extract(x = ghm2, y = cpad_super)

cpad_ghm_summary2 <- cpad_ghm_values2 %>%
  group_by(ID) %>%
  summarize(across(HFP2009, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm2 <- full_join(cpad_ghm, cpad_ghm_summary2) %>%
  mutate(HFP2009_range = HFP2009_max - HFP2009_min)

#st_write(cpad_ghm, here::here("data", "processed_data", "cpad_join2.shp"), append = FALSE)

p1 <- ggplot(data = cpad_ghm2) +
  geom_sf(aes(color = HFP2009_min, fill = HFP2009_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_ghm2) +
  geom_sf(aes(color = HFP2009_mean, fill = HFP2009_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_ghm2) +
  geom_sf(aes(color = HFP2009_max, fill = HFP2009_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_ghm2) +
  geom_sf(aes(color = HFP2009_median, fill = HFP2009_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_ghm2) +
  geom_sf(aes(color = HFP2009_range, fill = HFP2009_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_ghm2) +
  geom_sf(aes(color = HFP2009_sd, fill = HFP2009_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.2.pdf"), path = here::here("analysis"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.2.pdf"), path = here::here("analysis"), height = 10, width = 12)

cpad_values2 <- cpad_ghm_values2 %>%
  drop_na()

cpad_values2 <- full_join(cpad_values2, cpad_ghm) %>%
  select(-ID) %>%
  st_drop_geometry()

access_sum <- cpad_values2 %>%
  group_by(access_typ) %>%
  summarize(
    mean_HFP2009 = mean(HFP2009, na.rm = TRUE),
    min_HFP2009 = min(HFP2009, na.rm = TRUE),
    max_HFP2009 = max(HFP2009, na.rm = TRUE),
    median_HFP2009 = median(HFP2009, na.rm = TRUE)
  )

write_csv(access_sum, here::here("analysis", "HFP_access_summary.csv"))

layer_sum <- cpad_values2 %>%
  group_by(layer) %>%
  summarize(
    mean_HFP2009 = mean(HFP2009, na.rm = TRUE),
    min_HFP2009 = min(HFP2009, na.rm = TRUE),
    max_HFP2009 = max(HFP2009, na.rm = TRUE),
    median_HFP2009 = median(HFP2009, na.rm = TRUE)
  )

write_csv(layer_sum, here::here("analysis", "HFP_mgn_type_summary.csv"))

mng_sum <- cpad_values2 %>%
  group_by(mng_agncy) %>%
  summarize(
    mean_HFP2009 = mean(HFP2009, na.rm = TRUE),
    min_HFP2009 = min(HFP2009, na.rm = TRUE),
    max_HFP2009 = max(HFP2009, na.rm = TRUE),
    median_HFP2009 = median(HFP2009, na.rm = TRUE)
  )

write_csv(mng_sum, here::here("analysis", "HFP_mng_agency_summary.csv"))

park_sum <- cpad_values2 %>%
  group_by(park_name) %>%
  summarize(
    mean_HFP2009 = mean(HFP2009, na.rm = TRUE),
    min_HFP2009 = min(HFP2009, na.rm = TRUE),
    max_HFP2009 = max(HFP2009, na.rm = TRUE),
    median_HFP2009 = median(HFP2009, na.rm = TRUE)
  )

write_csv(park_sum, here::here("analysis", "HFP_park_summary.csv"))

ggplot(data = cpad_values2) +
  geom_histogram(aes(x = HFP2009)) +
  theme_bw()

p7 <- ggplot(data = cpad_values2) +
  geom_boxplot(aes(x = access_typ, y = HFP2009)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Access Level"
  )

p8 <- ggplot(data = cpad_values2) +
  geom_boxplot(aes(x = layer, y = HFP2009), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Agency Type"
  )

plot3 <- p7 + p8

ggsave(plot3, file = paste0("plot3.2.pdf"), path = here::here("analysis"), height = 10, width = 12)


# GHM Layer 3 -------------------------------------------------------------

ghm3 <- rast(here::here("data", "raw_data", "gHM_landLakeReservoirOcean300m-0000000000-0000000000.tif")) %>%
  project("EPSG:4326")

# ghm_cropped3 = crop(ghm3, cpad_super)
# 
# plot(ghm_cropped3)
# 
# ghm_masked3 = mask(ghm_cropped3, cpad_super)
# 
# plot(ghm_masked3)
# 
# writeRaster(ghm_masked3, here::here("data", "raw_data", "gHM_land_masked.tif"))

ghm3 <- rast(here::here("data", "raw_data", "gHM_land_masked.tif")) %>%
  project("EPSG:4326")

cpad_ghm_values3 <- terra::extract(x = ghm3, y = cpad_super)

cpad_ghm_summary3 <- cpad_ghm_values3 %>%
  group_by(ID) %>%
  summarize(across(LandLakeReservoirOcean, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm3 <- full_join(cpad_ghm, cpad_ghm_summary3) %>%
  mutate(LandLakeReservoirOcean_range = LandLakeReservoirOcean_max - LandLakeReservoirOcean_min)

#st_write(cpad_ghm, here::here("data", "processed_data", "cpad_join3.shp"), append = FALSE)

p1 <- ggplot(data = cpad_ghm3) +
  geom_sf(aes(color = LandLakeReservoirOcean_min, fill = LandLakeReservoirOcean_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_ghm3) +
  geom_sf(aes(color = LandLakeReservoirOcean_mean, fill = LandLakeReservoirOcean_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_ghm3) +
  geom_sf(aes(color = LandLakeReservoirOcean_max, fill = LandLakeReservoirOcean_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_ghm3) +
  geom_sf(aes(color = LandLakeReservoirOcean_median, fill = LandLakeReservoirOcean_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_ghm3) +
  geom_sf(aes(color = LandLakeReservoirOcean_range, fill = LandLakeReservoirOcean_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_ghm3) +
  geom_sf(aes(color = LandLakeReservoirOcean_sd, fill = LandLakeReservoirOcean_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.3.pdf"), path = here::here("analysis"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.3.pdf"), path = here::here("analysis"), height = 10, width = 12)

cpad_values3 <- cpad_ghm_values3 %>%
  drop_na()

cpad_values3 <- full_join(cpad_values3, cpad_ghm) %>%
  select(-ID) %>%
  st_drop_geometry()

access_sum <- cpad_values3 %>%
  group_by(access_typ) %>%
  summarize(
    mean_LandLakeReservoirOcean = mean(LandLakeReservoirOcean, na.rm = TRUE),
    min_LandLakeReservoirOcean = min(LandLakeReservoirOcean, na.rm = TRUE),
    max_LandLakeReservoirOcean = max(LandLakeReservoirOcean, na.rm = TRUE),
    median_LandLakeReservoirOcean = median(LandLakeReservoirOcean, na.rm = TRUE)
  )

write_csv(access_sum, here::here("analysis", "LLRO_access_summary.csv"))

layer_sum <- cpad_values3 %>%
  group_by(layer) %>%
  summarize(
    mean_LandLakeReservoirOcean = mean(LandLakeReservoirOcean, na.rm = TRUE),
    min_LandLakeReservoirOcean = min(LandLakeReservoirOcean, na.rm = TRUE),
    max_LandLakeReservoirOcean = max(LandLakeReservoirOcean, na.rm = TRUE),
    median_LandLakeReservoirOcean = median(LandLakeReservoirOcean, na.rm = TRUE)
  )

write_csv(layer_sum, here::here("analysis", "LLRO_mgn_type_summary.csv"))

mng_sum <- cpad_values3 %>%
  group_by(mng_agncy) %>%
  summarize(
    mean_LandLakeReservoirOcean = mean(LandLakeReservoirOcean, na.rm = TRUE),
    min_LandLakeReservoirOcean = min(LandLakeReservoirOcean, na.rm = TRUE),
    max_LandLakeReservoirOcean = max(LandLakeReservoirOcean, na.rm = TRUE),
    median_LandLakeReservoirOcean = median(LandLakeReservoirOcean, na.rm = TRUE)
  )

write_csv(mng_sum, here::here("analysis", "LLRO_mng_agency_summary.csv"))

park_sum <- cpad_values3 %>%
  group_by(park_name) %>%
  summarize(
    mean_LandLakeReservoirOcean = mean(LandLakeReservoirOcean, na.rm = TRUE),
    min_LandLakeReservoirOcean = min(LandLakeReservoirOcean, na.rm = TRUE),
    max_LandLakeReservoirOcean = max(LandLakeReservoirOcean, na.rm = TRUE),
    median_LandLakeReservoirOcean = median(LandLakeReservoirOcean, na.rm = TRUE)
  )

write_csv(park_sum, here::here("analysis", "LLRO_park_summary.csv"))

ggplot(data = cpad_values3) +
  geom_histogram(aes(x = LandLakeReservoirOcean)) +
  theme_bw()

p7 <- ggplot(data = cpad_values3) +
  geom_boxplot(aes(x = access_typ, y = LandLakeReservoirOcean)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Access Level"
  )

p8 <- ggplot(data = cpad_values3) +
  geom_boxplot(aes(x = layer, y = LandLakeReservoirOcean), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Agency Type"
  )

plot3 <- p7 + p8

ggsave(plot3, file = paste0("plot3.3.pdf"), path = here::here("analysis"), height = 10, width = 12)

# Land Cover --------------------------------------------------------------



# Night Lights ------------------------------------------------------------
