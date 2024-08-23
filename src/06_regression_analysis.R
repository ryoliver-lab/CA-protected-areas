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
  st_drop_geometry() %>% 
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
    period, pred, pred_mod, pred_temp, Dim.1, Dim.2, rank
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
  select(-gap1_acres, -gap2_acres, -gap3_acres, -gap4_acres) %>% 
  filter(rank == 1) %>% 
  distinct()

# PCA Regression --------------------------------------------------

pca_mod = lm(Dim.1 ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(pca_mod, direction = "both")

summary(pca_mod)
anova(pca_mod)

plot_predictions(pca_mod, by = "access_typ", variable = "access_typ")
plot_predictions(pca_mod, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(pca_mod, by = "gap_status", variable = "gap_status")
plot_predictions(pca_mod, by = "shape_leng", variable = "shape_leng")
plot_predictions(pca_mod, by = "outside_pop", variable = "outside_pop")

# Peak Regression ---------------------------------------------------------

peak_mod = glm(period ~ access_typ + mng_ag_lev+ gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(peak_mod, direction = "both")

peak_mod2 = glm(period ~ mng_ag_lev + outside_pop, data = human)
peak_mod3 = glm(period ~ mng_ag_lev, data = human)

anova(peak_mod2, peak_mod3, test = "LRT")

summary(peak_mod3)
anova(peak_mod3, test = "Chisq")
AIC(peak_mod)

# plot_predictions(peak_mod, by = "access_typ", variable = "access_typ")
plot_predictions(peak_mod3, by = "mng_ag_lev", variable = "mng_ag_lev")
# plot_predictions(peak_mod, by = "gap_status", variable = "gap_status")
# plot_predictions(peak_mod, by = "shape_leng", variable = "shape_leng")
# plot_predictions(peak_mod, by = "outside_pop", variable = "outside_pop")
# plot_predictions(peak_mod, by = "peopcolorpct", variable = "peopcolorpct")
# plot_predictions(peak_mod, by = "lowincpct", variable = "lowincpct")

# Predictability Regression - Spatial -------------------------------------

ps_mod = lm(log(pred) ~ access_typ + mng_ag_lev + gap_status+ shape_leng + outside_pop, data = human)

MASS::stepAIC(ps_mod, direction = "both")

ps_mod2 = lm(log(pred) ~ access_typ + mng_ag_lev + gap_status, 
   data = human)
ps_mod3 = lm(log(pred) ~ access_typ + mng_ag_lev + gap_status + outside_pop, 
             data = human)

anova(ps_mod2, ps_mod3)

summary(ps_mod2)

plot_predictions(ps_mod2, by = "access_typ", variable = "access_typ")
plot_predictions(ps_mod2, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(ps_mod2, by = "gap_status", variable = "gap_status")
# plot_predictions(ps_mod, by = "shape_leng", variable = "shape_leng")
# plot_predictions(ps_mod, by = "outside_pop", variable = "outside_pop")

# Predictability Regression - Mod -----------------------------------------

pm_mod = lm(log(pred_mod) ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(pm_mod, direction = "both")

pm_mod2 = lm(log(pred_mod) ~ access_typ + mng_ag_lev + gap_status + shape_leng, data = human)

anova(pm_mod, pm_mod2, test = "LRT")

summary(pm_mod2)

plot_predictions(pm_mod2, by = "access_typ", variable = "access_typ")
plot_predictions(pm_mod2, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(pm_mod2, by = "gap_status", variable = "gap_status")
plot_predictions(pm_mod2, by = "shape_leng", variable = "shape_leng")
# plot_predictions(pm_mod, by = "outside_pop", variable = "outside_pop")

# Predictability Regression - Temporal ------------------------------------

pt_mod = lm(log(pred_temp) ~ access_typ + mng_ag_lev+ gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(pt_mod, direction = "both")

pt_mod2 = lm(log(pred_temp) ~ mng_ag_lev + gap_status + outside_pop, 
data = human)

anova(pt_mod, pt_mod2)

summary(pt_mod2)

# plot_predictions(pt_mod, by = "access_typ", variable = "access_typ")
plot_predictions(pt_mod2, by = "mng_ag_lev", variable = "mng_ag_lev")
plot_predictions(pt_mod2, by = "gap_status", variable = "gap_status")
# plot_predictions(pt_mod, by = "shape_leng", variable = "shape_leng")
plot_predictions(pt_mod2, by = "outside_pop", variable = "outside_pop")
