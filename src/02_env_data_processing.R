library(tidyverse)
library(sf)
library(terra)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

# Mean Temperature ----------------------------------------------------------

temp1 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_01.tif"))
temp2 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_02.tif"))
temp3 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_03.tif"))
temp4 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_04.tif"))
temp5 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_05.tif"))
temp6 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_06.tif"))
temp7 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_07.tif"))
temp8 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_08.tif"))
temp9 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_09.tif"))
temp10 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_10.tif"))
temp11 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_11.tif"))
temp12 <- rast(here::here("data", "raw_data", "wc2.1_30s_tavg", "wc2.1_30s_tavg_12.tif"))

temp = mean(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12) %>%
  project("EPSG:4326")

rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12)

temp_cropped <- crop(temp, cpad_super)
temp_masked <- mask(temp_cropped, cpad_super)

rm(temp, temp_cropped)

cpad_temp_values <- terra::extract(x = temp_masked, y = cpad_super) %>% 
  rename(mean_tp = wc2.1_30s_tavg_01)

cpad_temp_summary <- cpad_temp_values %>%
  group_by(ID) %>%
  summarize(across(mean_tp, list(mean = mean,  sd = sd)))

cpad_temp <- full_join(cpad_super, cpad_temp_summary) %>%
  mutate(
    mean_tp_cv = mean_tp_sd / mean_tp_mean
  ) %>% 
  mutate(mean_tp_sd = case_when(
    !is.na(mean_tp_sd) ~ mean_tp_sd,
    is.na(mean_tp_sd) ~ 0
  )) %>% 
  mutate(mean_tp_cv = case_when(
    !is.na(mean_tp_cv) ~ mean_tp_cv,
    is.na(mean_tp_cv) ~ 0
  ))

st_write(cpad_temp, here::here("data", "processed_data", "cpad_mean_temp.shp"))

rm(temp_masked, cpad_temp_values, cpad_temp_summary, cpad_temp)

# Max Temperature ----------------------------------------------------------

temp1 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_01.tif"))
temp2 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_02.tif"))
temp3 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_03.tif"))
temp4 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_04.tif"))
temp5 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_05.tif"))
temp6 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_06.tif"))
temp7 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_07.tif"))
temp8 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_08.tif"))
temp9 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_09.tif"))
temp10 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_10.tif"))
temp11 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_11.tif"))
temp12 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmax", "wc2.1_30s_tmax_12.tif"))

temp = max(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12) %>%
  project("EPSG:4326")

rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12)

temp_cropped <- crop(temp, cpad_super)
temp_masked <- mask(temp_cropped, cpad_super)

rm(temp, temp_cropped)

cpad_temp_values <- terra::extract(x = temp_masked, y = cpad_super) %>% 
  rename(max_tp = wc2.1_30s_tmax_01)

cpad_temp_summary <- cpad_temp_values %>%
  group_by(ID) %>%
  summarize(across(max_tp, list(mean = mean, sd = sd)))

cpad_max_temp <- full_join(cpad_super, cpad_temp_summary) %>%
  mutate(
    max_tp_cv = max_tp_sd / max_tp_mean
  ) %>% 
  mutate(max_tp_sd = case_when(
    !is.na(max_tp_sd) ~ max_tp_sd,
    is.na(max_tp_sd) ~ 0
  )) %>% 
  mutate(max_tp_cv = case_when(
    !is.na(max_tp_cv) ~ max_tp_cv,
    is.na(max_tp_cv) ~ 0
  ))

st_write(cpad_max_temp, here::here("data", "processed_data", "cpad_max_temp.shp"))

rm(temp_masked, cpad_temp_values, cpad_temp_summary, cpad_max_temp)

# Min Temperature ----------------------------------------------------------

temp1 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_01.tif"))
temp2 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_02.tif"))
temp3 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_03.tif"))
temp4 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_04.tif"))
temp5 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_05.tif"))
temp6 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_06.tif"))
temp7 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_07.tif"))
temp8 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_08.tif"))
temp9 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_09.tif"))
temp10 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_10.tif"))
temp11 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_11.tif"))
temp12 <- rast(here::here("data", "raw_data", "wc2.1_30s_tmin", "wc2.1_30s_tmin_12.tif"))

temp = min(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12) %>%
  project("EPSG:4326")

rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12)

temp_cropped <- crop(temp, cpad_super)
temp_masked <- mask(temp_cropped, cpad_super)

rm(temp, temp_cropped)

cpad_temp_values <- terra::extract(x = temp_masked, y = cpad_super) %>% 
  rename(min_tp = wc2.1_30s_tmin_01)

cpad_temp_summary <- cpad_temp_values %>%
  group_by(ID) %>%
  summarize(across(min_tp, list(mean = mean,  sd = sd)))

cpad_min_temp <- full_join(cpad_super, cpad_temp_summary) %>%
  mutate(
    min_tp_cv = min_tp_sd / min_tp_mean
  ) %>% 
  mutate(min_tp_sd = case_when(
    !is.na(min_tp_sd) ~ min_tp_sd,
    is.na(min_tp_sd) ~ 0
  )) %>% 
  mutate(min_tp_cv = case_when(
    !is.na(min_tp_cv) ~ min_tp_cv,
    is.na(min_tp_cv) ~ 0
  ))

st_write(cpad_min_temp, here::here("data", "processed_data", "cpad_min_temp.shp"))

rm(temp_masked, cpad_temp_values, cpad_temp_summary, cpad_min_temp)

# Precipitation ----------------------------------------------------------

prec1 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_01.tif"))
prec2 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_02.tif"))
prec3 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_03.tif"))
prec4 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_04.tif"))
prec5 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_05.tif"))
prec6 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_06.tif"))
prec7 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_07.tif"))
prec8 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_08.tif"))
prec9 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_09.tif"))
prec10 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_10.tif"))
prec11 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_11.tif"))
prec12 <- rast(here::here("data", "raw_data", "wc2.1_30s_prec", "wc2.1_30s_prec_12.tif"))

prec = mean(prec1, prec2, prec3, prec4, prec5, prec6, prec7, prec8, prec9, prec10, prec11, prec12) %>%
  project("EPSG:4326")

rm(prec1, prec2, prec3, prec4, prec5, prec6, prec7, prec8, prec9, prec10, prec11, prec12)

prec_cropped <- crop(prec, cpad_super)
prec_masked <- mask(prec_cropped, cpad_super)

rm(prec, prec_cropped)

cpad_prec_values <- terra::extract(x = prec_masked, y = cpad_super) %>% 
  rename(prec = wc2.1_30s_prec_01)

cpad_prec_summary <- cpad_prec_values %>%
  group_by(ID) %>%
  summarize(across(prec, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_prec <- full_join(cpad_super, cpad_prec_summary) %>%
  mutate(
    prec_range = prec_max - prec_min,
    prec_cv = prec_sd / prec_mean
  ) %>% 
  mutate(prec_sd = case_when(
    !is.na(prec_sd) ~ prec_sd,
    is.na(prec_sd) ~ 0
  )) %>% 
  mutate(prec_cv = case_when(
    !is.na(prec_cv) ~ prec_cv,
    is.na(prec_cv) ~ 0
  ))

st_write(cpad_prec, here::here("data", "processed_data", "cpad_prec.shp"))

rm(prec_masked, cpad_prec_values, cpad_prec_summary, cpad_prec)

# NDVI --------------------------------------------------------------------

ndvi <- rast(here::here("data", "raw_data", "VIIRS-Land_v001_NPP13C1_S-NPP_20210101_c20240126162033.nc")) %>%
  project("EPSG:4326")

ndvi_cropped <- crop(ndvi, cpad_super)
ndvi_masked <- mask(ndvi_cropped, cpad_super)

ndvi_masked <- ndvi_masked$NDVI

rm(ndvi, ndvi_cropped)

cpad_ndvi_values <- terra::extract(x = ndvi_masked, y = cpad_super)

cpad_ndvi_summary <- cpad_ndvi_values %>%
  group_by(ID) %>%
  summarize(across(NDVI, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ndvi <- full_join(cpad_super, cpad_ndvi_summary) %>%
  mutate(
    NDVI_range = NDVI_max - NDVI_min,
    NDVI_cv = NDVI_sd / NDVI_mean
  ) %>% 
  mutate(NDVI_sd = case_when(
    !is.na(NDVI_sd) ~ NDVI_sd,
    is.na(NDVI_sd) ~ 0
  )) %>% 
  mutate(NDVI_cv = case_when(
    !is.na(NDVI_cv) ~ NDVI_cv,
    is.na(NDVI_cv) ~ 0
  ))

st_write(cpad_ndvi, here::here("data", "processed_data", "cpad_ndvi.shp"))

rm(ndvi_masked, cpad_ndvi_values, cpad_ndvi_summary, cpad_ndvi)

# Canopy Cover ------------------------------------------------------------

cc <- rast(here::here("data", "raw_data", "science_tcc_CONUS_2021_v2021-4", "science_tcc_conus_2021_v2021-4.tif")) %>%
  project("EPSG:4326")

cc_cropped <- crop(cc, cpad_super)
cc_masked <- mask(cc_cropped, cpad_super)

rm(cc, cc_cropped)

cpad_cc_values <- terra::extract(x = cc_masked, y = cpad_super)  %>%
  rename(cc = Layer_1)  %>%
  filter(!is.na(cc))  %>%
  mutate(cc = as.numeric(cc))

cpad_cc_summary <- cpad_cc_values %>%
  group_by(ID) %>%
  summarize(across(cc, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_cc <- full_join(cpad_super, cpad_cc_summary) %>%
  mutate(
    cc_range = cc_max - cc_min,
    cc_cv = cc_sd / cc_mean
  ) %>% 
  mutate(cc_sd = case_when(
    !is.na(cc_sd) ~ cc_sd,
    is.na(cc_sd) ~ 0
  )) %>% 
  mutate(cc_cv = case_when(
    !is.na(cc_cv) ~ cc_cv,
    is.na(cc_cv) ~ 0
  ))

st_write(cpad_cc, here::here("data", "processed_data", "cpad_cc.shp"))

rm(cc_masked, cpad_cc_values, cpad_cc_summary, cpad_cc)

# Final processing ---------------------------------------------------------