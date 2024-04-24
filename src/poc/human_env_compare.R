library(tidyverse)
library(sf)
library(vegan)

# Load Data ---------------------------------------------------------------

sg <- read_csv(here::here("data", "processed_data", "cpad_sg.csv"))
strava <- st_read(here::here("data", "processed_data", "cpad_strava.shp"))
ghm <- st_read(here::here("data", "processed_data", "cpad_ghm.shp"))
hfp <- st_read(here::here("data", "processed_data", "cpad_hfp.shp"))

human <- list(sg, strava, ghm, hfp) %>%
  reduce(full_join) %>%
  st_as_sf() %>%
  select(
    max_sg_2019, min_sg_2019, sg_2019, max_sg_2020, min_sg_2020, sg_2020, hx_pp_r, mx_hx__, mn_hx__, suid_nma,
    access_typ, park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    gHM_min, gHM_mean, gHM_max, gHM_median, gHM_sd, gHM_range, gHM_cv, HFP_min,
    HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv
  )

rm(sg, strava, ghm, hfp)

cc <- st_read(here::here("data", "processed_data", "cpad_cc.shp")) %>%
  st_drop_geometry()
max_temp <- st_read(here::here("data", "processed_data", "cpad_max_temp.shp")) %>%
  st_drop_geometry()
min_temp <- st_read(here::here("data", "processed_data", "cpad_min_temp.shp")) %>%
  st_drop_geometry() %>% 
  rename(min_mean_temp = mn_tp_m,
         min_sd_temp = mn_tp_s,
         min_cv_temp = mn_tp_c)
mean_temp <- st_read(here::here("data", "processed_data", "cpad_mean_temp.shp")) %>%
  st_drop_geometry()
prec <- st_read(here::here("data", "processed_data", "cpad_prec.shp")) %>%
  st_drop_geometry()
ndvi <- st_read(here::here("data", "processed_data", "cpad_ndvi.shp"))

env <- list(cc, max_temp, min_temp, mean_temp, prec, ndvi) %>%
  reduce(full_join) %>%
  st_as_sf() %>%
  select(
    suid_nma, access_typ, park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    mx_tp_m, mx_tp_s, mx_tp_c, mn_tp_m, mn_tp_s, mn_tp_c, prec_min, prec_men, prec_mx,
    prc_mdn, prec_sd, prc_rng, prec_cv, NDVI_min, NDVI_men, NDVI_mx, NDVI_md, NDVI_sd,
    NDVI_rn, NDVI_cv, cc_min, cc_mean, cc_max, cc_median, cc_sd, cc_range, cc_cv, min_mean_temp,
    min_sd_temp, min_cv_temp
  ) %>%
  st_drop_geometry()

rm(cc, max_temp, min_temp, mean_temp, prec, ndvi)

full_cpad <- full_join(human, env) %>%
  st_as_sf() %>%
  mutate(
    sg_2019_range = max_sg_2019 - min_sg_2019,
    sg_2020_range = max_sg_2020 - min_sg_2020,
    hex_range = mx_hx__ - mn_hx__
  )

# Compare Plots -----------------------------------------------------------

cpad <- full_cpad %>%
  st_drop_geometry() %>%
  select(
    sg_2019, sg_2020, hx_pp_r,
    gHM_mean, gHM_cv,
    HFP_mean, HFP_cv, suid_nma,
    mx_tp_m, mx_tp_c, mn_tp_m, mn_tp_c, prec_men, min_mean_temp,
    min_sd_temp, min_cv_temp,
    prec_cv, NDVI_men, NDVI_cv, cc_mean, cc_cv, access_typ,
    park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
  )

ggplot(cpad) +
  geom_point(aes(gHM_mean, mn_tp_m))
ggplot(cpad) +
  geom_point(aes(gHM_mean, mx_tp_m))
ggplot(cpad) +
  geom_point(aes(gHM_mean, min_mean_temp))
ggplot(cpad) +
  geom_point(aes(gHM_mean, prec_men))
ggplot(cpad) +
  geom_point(aes(gHM_mean, NDVI_men))
ggplot(cpad) +
  geom_point(aes(gHM_mean, cc_mean))

ggplot(cpad) +
  geom_point(aes(HFP_mean, mn_tp_m))
ggplot(cpad) +
  geom_point(aes(HFP_mean, mx_tp_m))
ggplot(cpad) +
  geom_point(aes(HFP_mean, min_mean_temp))
ggplot(cpad) +
  geom_point(aes(HFP_mean, prec_men))
ggplot(cpad) +
  geom_point(aes(HFP_mean, NDVI_men))
ggplot(cpad) +
  geom_point(aes(HFP_mean, cc_mean))

ggplot(cpad) +
  geom_point(aes(hx_pp_r, mn_tp_m)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(hx_pp_r, mx_tp_m)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(hx_pp_r, min_mean_temp)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(hx_pp_r, prec_men)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(hx_pp_r, NDVI_men)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(hx_pp_r, cc_mean)) +
  scale_x_log10()

ggplot(cpad) +
  geom_point(aes(sg_2019, mn_tp_m)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2019, mx_tp_m)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2019, min_mean_temp)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2019, prec_men)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2019, NDVI_men)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2019, cc_mean)) +
  scale_x_log10()

ggplot(cpad) +
  geom_point(aes(sg_2020, mn_tp_m)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2020, mx_tp_m)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2020, min_mean_temp)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2020, prec_men)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2020, NDVI_men)) +
  scale_x_log10()
ggplot(cpad) +
  geom_point(aes(sg_2020, cc_mean)) +
  scale_x_log10()
