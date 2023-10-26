library(tidyverse)
library(patchwork)
library(sf)
library(terra)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

cpad_extended <- st_read(here::here("data", "processed_data", "cpad_extended.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names()

# Raster Cropping ----------------------------------------------------------

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

plot(ghm)

ghm_cropped = crop(ghm, cpad_super)

plot(ghm_cropped)

ghm_masked = mask(ghm_cropped, cpad_super)

plot(ghm_masked)

writeRaster(ghm_masked, here::here("data", "processed_data", "gHM_masked.tif"))

ghm2 <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

ghm_cropped2 = crop(ghm2, cpad_super)

plot(ghm_cropped2)

ghm_masked2 = mask(ghm_cropped2, cpad_super)

plot(ghm_masked2)

writeRaster(ghm_masked2, here::here("data", "processed_data", "HFP2009_masked.tif"))

ghm3 <- rast(here::here("data", "raw_data", "gHM_landLakeReservoirOcean300m-0000000000-0000000000.tif")) %>%
 project("EPSG:4326")

ghm_cropped3 = crop(ghm3, cpad_super)

plot(ghm_cropped3)

ghm_masked3 = mask(ghm_cropped3, cpad_super)

plot(ghm_masked3)

writeRaster(ghm_masked3, here::here("data", "processed_data", "gHM_land_masked.tif"))

lc <- rast(here::here("data", "raw_data", "C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"))

lc_cropped = crop(lc, cpad_super)

plot(lc_cropped)

lc_masked = mask(lc_cropped, cpad_super) %>%
  project("EPSG:4326")

plot(lc_masked)

writeRaster(lc_masked, here::here("data", "processed_data", "LC_masked.tif"))


# Data Combining ----------------------------------------------------------

cpad_ghm_values <- terra::extract(x = ghm_masked, y = cpad_super)

cpad_ghm_summary <- cpad_ghm_values %>%
  group_by(ID) %>%
  summarize(across(gHM, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm <- full_join(cpad_super, cpad_ghm_summary) %>%
  mutate(gHM_range = gHM_max - gHM_min)

cpad_ghm_values2 <- terra::extract(x = ghm_masked2, y = cpad_super) 

cpad_ghm_summary2 <- cpad_ghm_values2 %>%
  rename(HFP = HFP2009) %>% 
  group_by(ID) %>%
  summarize(across(HFP, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm2 <- full_join(cpad_ghm, cpad_ghm_summary2) %>%
  mutate(HFP_range = HFP_max - HFP_min)

cpad_ghm_values3 <- terra::extract(x = ghm_masked3, y = cpad_super) %>% 
  rename(LRO = LandLakeReservoirOcean)

cpad_ghm_summary3 <- cpad_ghm_values3 %>%
  group_by(ID, LRO) %>%
  summarize(count = n()) %>% 
  mutate(freq_LRO = count / sum(count)) %>% 
  select(ID, LRO, freq_LRO) %>% 
  ungroup()

cpad_ghm3 <- full_join(cpad_ghm2, cpad_ghm_summary3)

cpad_lc_values <- terra::extract(x = lc_masked, y = cpad_super) 

cpad_lc_summary <- cpad_lc_values %>%
  select(ID, lccs_class) %>% 
  rename(LC = lccs_class) %>% 
  group_by(ID, LC) %>%
  summarize(count = n()) %>% 
  mutate(freq_LC = count / sum(count)) %>% 
  select(ID, LC, freq_LC) %>% 
  ungroup()

cpad_extend <- full_join(cpad_ghm3, cpad_lc_summary, relationship = "many-to-many")

st_write(cpad_extend, here::here("data", "processed_data", "cpad_extended.shp"))

# Plots ------------------------------------------------------

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_min, fill = g_hm_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_mean, fill = g_hm_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_max, fill = g_hm_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_median, fill = g_hm_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_range, fill = g_hm_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_sd, fill = g_hm_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_min, fill = hfp_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_mean, fill = hfp_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_max, fill = hfp_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_median, fill = hfp_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_range, fill = hfp_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_sd, fill = hfp_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = freq_lro)) +
  facet_wrap(~ lro) +
  theme_bw()

ggsave(p1, file = paste0("plot3.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = freq_lc)) +
  facet_wrap(~ lc)
  theme_bw()

ggsave(p1, file = paste0("plot3.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# Summary -----------------------------------------------------------------

cpad_values1 <- cpad_ghm_values %>%
  drop_na()

cpad_values1 <- full_join(cpad_values1, cpad_super) %>%
  select(-ID) %>%
  st_drop_geometry()

cpad_values2 <- cpad_ghm_values2 %>%
  drop_na()

cpad_values2 <- full_join(cpad_values2, cpad_super) %>%
  select(-ID) %>%
  st_drop_geometry()

cpad_values3 <- cpad_ghm_values3 %>%
  drop_na()

cpad_values3 <- full_join(cpad_values3, cpad_super) %>%
  select(-ID) %>%
  st_drop_geometry()

cpad_values4 <- cpad_lc_values %>%
  drop_na()

cpad_values4 <- full_join(cpad_values4, cpad_super) %>%
  select(-ID) %>%
  st_drop_geometry() %>% 
  rename(LC = lccs_class)

access_sum1 <- cpad_values1 %>%
  group_by(access_typ) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  ) 

access_sum2 <- cpad_values2 %>%
  group_by(access_typ) %>%
  summarize(
    mean_HFP = mean(HFP2009, na.rm = TRUE),
    min_HFP = min(HFP2009, na.rm = TRUE),
    max_HFP = max(HFP2009, na.rm = TRUE),
    median_HFP = median(HFP2009, na.rm = TRUE),
  ) 

access_sum3 <- cpad_values3 %>%
  group_by(access_typ, LRO) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LRO = count / sum(count)) %>% 
  select(access_typ, LRO, freq_LRO) %>% 
  ungroup() %>% 
  distinct()

access_sum4 <- cpad_values4 %>%
  group_by(access_typ, LC) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LC = count / sum(count)) %>% 
  select(access_typ, LC, freq_LC) %>% 
  ungroup() %>% 
  distinct()

access_sum = list(access_sum1, access_sum2, access_sum3, access_sum4) %>% 
  reduce(full_join)

write_csv(access_sum, here::here("analysis", "access_summary.csv"))

layer_sum1 <- cpad_values1 %>%
  group_by(layer) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  ) 

layer_sum2 <- cpad_values2 %>%
  group_by(layer) %>%
  summarize(
    mean_HFP = mean(HFP2009, na.rm = TRUE),
    min_HFP = min(HFP2009, na.rm = TRUE),
    max_HFP = max(HFP2009, na.rm = TRUE),
    median_HFP = median(HFP2009, na.rm = TRUE),
  ) 

layer_sum3 <- cpad_values3 %>%
  group_by(layer, LRO) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LRO = count / sum(count)) %>% 
  select(layer, LRO, freq_LRO) %>% 
  ungroup() %>% 
  distinct()

layer_sum4 <- cpad_values4 %>%
  group_by(layer, LC) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LC = count / sum(count)) %>% 
  select(layer, LC, freq_LC) %>% 
  ungroup() %>% 
  distinct()

layer_sum = list(layer_sum1, layer_sum2, layer_sum3, layer_sum4) %>% 
  reduce(full_join)

write_csv(layer_sum, here::here("analysis", "mgn_type_summary.csv"))

mng_sum1 <- cpad_values1 %>%
  group_by(mng_agncy) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  ) 

mng_sum2 <- cpad_values2 %>%
  group_by(mng_agncy) %>%
  summarize(
    mean_HFP = mean(HFP2009, na.rm = TRUE),
    min_HFP = min(HFP2009, na.rm = TRUE),
    max_HFP = max(HFP2009, na.rm = TRUE),
    median_HFP = median(HFP2009, na.rm = TRUE),
  ) 

mng_sum3 <- cpad_values3 %>%
  group_by(mng_agncy, LRO) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LRO = count / sum(count)) %>% 
  select(mng_agncy, LRO, freq_LRO) %>% 
  ungroup() %>% 
  distinct() 

mng_sum4 <- cpad_values4 %>%
  group_by(mng_agncy, LC) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LC = count / sum(count)) %>% 
  select(mng_agncy, LC, freq_LC) %>% 
  ungroup() %>% 
  distinct() 

mng_sum = list(mng_sum1, mng_sum2, mng_sum3, mng_sum4) %>% 
  reduce(full_join)

write_csv(mng_sum, here::here("analysis", "mng_agency_summary.csv"))

park_sum1 <- cpad_values1 %>%
  group_by(park_name) %>%
  summarize(
    mean_gHM = mean(gHM, na.rm = TRUE),
    min_gHM = min(gHM, na.rm = TRUE),
    max_gHM = max(gHM, na.rm = TRUE),
    median_gHM = median(gHM, na.rm = TRUE)
  ) 

park_sum2 <- cpad_values2 %>%
  group_by(park_name) %>%
  summarize(
    mean_HFP = mean(HFP2009, na.rm = TRUE),
    min_HFP = min(HFP2009, na.rm = TRUE),
    max_HFP = max(HFP2009, na.rm = TRUE),
    median_HFP = median(HFP2009, na.rm = TRUE),
  ) 

park_sum3 <- cpad_values3 %>%
  group_by(park_name, LRO) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LRO = count / sum(count)) %>% 
  select(park_name, LRO, freq_LRO) %>% 
  ungroup() %>% 
  distinct() 

park_sum4 <- cpad_values4 %>%
  group_by(park_name, LC) %>% 
  summarize(count = n()) %>% 
  mutate(freq_LC = count / sum(count)) %>% 
  select(park_name, LC, freq_LC) %>% 
  ungroup() %>% 
  distinct() 

park_sum = list(park_sum1, park_sum2, park_sum3, park_sum4) %>% 
  reduce(full_join)

write_csv(park_sum, here::here("analysis", "park_summary.csv"))


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

ggsave(plot3, file = paste0("plot4.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p7 <- ggplot(data = cpad_values) +
  geom_boxplot(aes(x = access_typ, y = HFP)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Access Level"
  )

p8 <- ggplot(data = cpad_values) +
  geom_boxplot(aes(x = layer, y = HFP), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Agency Type"
  )

plot3 <- p7 + p8

ggsave(plot3, file = paste0("plot2.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# Night Lights ------------------------------------------------------------
