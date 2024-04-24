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
  st_drop_geometry()
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
    NDVI_rn, NDVI_cv, cc_min, cc_mean, cc_max, cc_median, cc_sd, cc_range, cc_cv
  ) %>%
  st_drop_geometry()

rm(cc, max_temp, min_temp, mean_temp, prec, ndvi)

full_cpad <- full_join(human, env) %>%
  st_as_sf() %>% 
  mutate(sg_2019_range = max_sg_2019 - min_sg_2019,
         sg_2020_range = max_sg_2020 - min_sg_2020,
         hex_range = mx_hx__ - mn_hx__)