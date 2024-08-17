library(tidyverse)
library(patchwork)
library(sf)
library(gtsummary)
library(broom)
library(marginaleffects)


# Load in Data ------------------------------------------------------------

sg <- read_csv(here::here("data", "processed_data", "cpad_sg.csv"))
strava <- st_read(here::here("data", "processed_data", "cpad_strava.shp"))
ghm <- st_read(here::here("data", "processed_data", "cpad_ghm.shp"))
hfp <- st_read(here::here("data", "processed_data", "cpad_hfp.shp"))
park_pred <- read_csv(here::here("data", "processed_data", "predictability.csv"))
periods = read_csv(here::here("analysis", "periodicity_max.csv"))
pca = read_csv(here::here("analysis", "pca.csv"))

human <- list(sg, strava, ghm, hfp, park_pred, periods, pca) %>%
  reduce(full_join) %>%
  st_as_sf()

rm(sg, strava, ghm, hfp, park_pred, periods, pca)

# take out hoas

human <- human %>%
  filter(shape_area >= 10000) %>%
  filter(!grepl("cemet", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("sport", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("farm", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("playground", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("golf", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("dog park", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("zoo", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("baseball", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("soccer", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("softball", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("athel", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("community center", park_name, ignore.case = TRUE)) %>%
  filter(!grepl("school", park_name, ignore.case = TRUE)) %>%
  select(
    max_sg, min_sg, sg, hx_pp_r, mx_hx__, mn_hx__, suid_nma,
    access_typ, park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    gHM_min, gHM_mean, gHM_max, gHM_median, gHM_sd, gHM_range, gHM_cv, HFP_min,
    HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv, outside_pop, peopcolorpct, lowincpct,
    period, pred, pred_mod, pred_temp, Dim.1, Dim.2
  ) %>%
  mutate(
    sg_range = max_sg - min_sg,
    hex_range = mx_hx__ - mn_hx__,
    HFP_mean = case_when(
      HFP_mean < 0 ~ 0,
      TRUE ~ HFP_mean
    )
  ) %>% 
  rowwise() %>% 
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>% 
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4"
  )) %>% 
  select(-gap1_acres, -gap2_acres, -gap3_acres, -gap4_acres)

# PCA Regression --------------------------------------------------

pca_mod = lm(Dim.1 ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop + peopcolorpct + lowincpct, data = human)

summary(pca_mod)
plot(pca_mod)

plot_predictions(pca_mod, by = "access_typ", variable = "access_typ")
plot_predictions(pca_mod, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(pca_mod, by = "gap_status", variable = "gap_status")
plot_predictions(pca_mod, by = "shape_leng", variable = "shape_leng")
plot_predictions(pca_mod, by = "outside_pop", variable = "outside_pop")
plot_predictions(pca_mod, by = "peopcolorpct", variable = "peopcolorpct")
plot_predictions(pca_mod, by = "lowincpct", variable = "lowincpct")

# Peak Regression ---------------------------------------------------------

peak_mod = glm(period ~ access_typ + mng_ag_lev+ gap_status + shape_leng + outside_pop + peopcolorpct + lowincpct, data = human, family = poisson)

summary(peak_mod)

plot_predictions(peak_mod, by = "access_typ", variable = "access_typ")
plot_predictions(peak_mod, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(peak_mod, by = "gap_status", variable = "gap_status")
plot_predictions(peak_mod, by = "shape_leng", variable = "shape_leng")
plot_predictions(peak_mod, by = "outside_pop", variable = "outside_pop")
plot_predictions(peak_mod, by = "peopcolorpct", variable = "peopcolorpct")
plot_predictions(peak_mod, by = "lowincpct", variable = "lowincpct")

# Predictability Regression - Spatial -------------------------------------

ps_mod = lm(pred ~ access_typ + mng_ag_lev + gap_status+ shape_leng + outside_pop + peopcolorpct + lowincpct, data = human)

summary(ps_mod)

plot_predictions(ps_mod, by = "access_typ", variable = "access_typ")
plot_predictions(ps_mod, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(ps_mod, by = "gap_status", variable = "gap_status")
plot_predictions(ps_mod, by = "shape_leng", variable = "shape_leng")
plot_predictions(ps_mod, by = "outside_pop", variable = "outside_pop")
plot_predictions(ps_mod, by = "peopcolorpct", variable = "peopcolorpct")
plot_predictions(ps_mod, by = "lowincpct", variable = "lowincpct")

# Predictability Regression - Mod -----------------------------------------

pm_mod = lm(pred_mod ~ access_typ + mng_ag_lev+ gap_status + shape_leng + outside_pop + peopcolorpct + lowincpct, data = human)

summary(pm_mod)

plot_predictions(pm_mod, by = "access_typ", variable = "access_typ")
plot_predictions(pm_mod, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(pm_mod, by = "gap_status", variable = "gap_status")
plot_predictions(pm_mod, by = "shape_leng", variable = "shape_leng")
plot_predictions(pm_mod, by = "outside_pop", variable = "outside_pop")
plot_predictions(pm_mod, by = "peopcolorpct", variable = "peopcolorpct")
plot_predictions(pm_mod, by = "lowincpct", variable = "lowincpct")

# Predictability Regression - Temporal ------------------------------------

pt_mod = lm(pred_temp ~ access_typ + mng_ag_lev+ gap_status + shape_leng + outside_pop + peopcolorpct + lowincpct, data = human)

summary(pt_mod)

plot_predictions(pt_mod, by = "access_typ", variable = "access_typ")
plot_predictions(pt_mod, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(pt_mod, by = "gap_status", variable = "gap_status")
plot_predictions(pt_mod, by = "shape_leng", variable = "shape_leng")
plot_predictions(pt_mod, by = "outside_pop", variable = "outside_pop")
plot_predictions(pt_mod, by = "peopcolorpct", variable = "peopcolorpct")
plot_predictions(pt_mod, by = "lowincpct", variable = "lowincpct")

