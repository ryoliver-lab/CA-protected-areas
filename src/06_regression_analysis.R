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
periods <- read_csv(here::here("analysis", "periodicity_max.csv"))
pca <- read_csv(here::here("analysis", "pca.csv"))

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
  filter(year == 2019) %>% 
  distinct()

# PCA Regression --------------------------------------------------

pca_mod <- lm(Dim.1 ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(pca_mod, direction = "both")

summary(pca_mod)
anova(pca_mod)

a_pred_pca <- avg_slopes(pca_mod, variable = "access_typ")
mng_pred_pca <- avg_slopes(pca_mod, variable = "mng_ag_lev")
gap_pred_pca <- avg_slopes(pca_mod, variable = "gap_status")
leng_pred_pca <- avg_slopes(pca_mod, variable = "shape_leng")
pop_pred_pca <- avg_slopes(pca_mod, variable = "outside_pop")

pred_pca <- list(a_pred_pca, mng_pred_pca, gap_pred_pca, leng_pred_pca, pop_pred_pca) %>%
  reduce(full_join) %>%
  mutate(signficant = case_when(
    p.value < 0.05 ~ "Signifcant",
    p.value >= 0.05 ~ "Non-Signficant"
  )) %>%
  mutate(change = case_when(
    estimate > 0 ~ "Increased Modification Relative to Mobility",
    estimate < 0 ~ "Increased Mobility Relative to Modification"
  )) %>%
  mutate(model = "PCA")

# Peak Regression ---------------------------------------------------------

peak_mod <- glm(period ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(peak_mod, direction = "both")

peak_mod2 <- glm(period ~ mng_ag_lev + outside_pop, data = human)
peak_mod3 <- glm(period ~ mng_ag_lev, data = human)

anova(peak_mod2, peak_mod3, test = "LRT")

summary(peak_mod3)
anova(peak_mod3, test = "Chisq")
AIC(peak_mod)

pred_peak <- avg_slopes(peak_mod3, variable = "mng_ag_lev") %>%
  mutate(signficant = case_when(
    p.value < 0.05 ~ "Signifcant",
    p.value >= 0.05 ~ "Non-Signficant"
  )) %>%
  mutate(model = "Peak")

# Predictability Regression - Spatial -------------------------------------

ps_mod <- lm(log(pred) ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(ps_mod, direction = "both")

ps_mod2 <- lm(log(pred) ~ access_typ + mng_ag_lev + gap_status,
  data = human
)
ps_mod3 <- lm(log(pred) ~ access_typ + mng_ag_lev + gap_status + outside_pop,
  data = human
)

anova(ps_mod2, ps_mod3)

summary(ps_mod2)

a_pred_sp <- avg_slopes(ps_mod2, variable = "access_typ")
mng_pred_sp <- avg_slopes(ps_mod2, variable = "mng_ag_lev")
gap_pred_sp <- avg_slopes(ps_mod2, variable = "gap_status")

pred_sp <- list(a_pred_sp, mng_pred_sp, gap_pred_sp) %>%
  reduce(full_join) %>%
  mutate(signficant = case_when(
    p.value < 0.05 ~ "Signifcant",
    p.value >= 0.05 ~ "Non-Signficant"
  )) %>%
  mutate(model = "Spatial")

# Predictability Regression - Mod -----------------------------------------

pm_mod <- lm(log(pred_mod) ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(pm_mod, direction = "both")

pm_mod2 <- lm(log(pred_mod) ~ access_typ + mng_ag_lev + gap_status + shape_leng, data = human)

anova(pm_mod, pm_mod2, test = "LRT")

summary(pm_mod2)

a_pred_mod <- avg_slopes(pm_mod2, variable = "access_typ")
mng_pred_mod <- avg_slopes(pm_mod2, variable = "mng_ag_lev")
gap_pred_mod <- avg_slopes(pm_mod2, variable = "gap_status")
leng_pred_mod <- avg_slopes(pm_mod2, variable = "shape_leng")

pred_mod <- list(a_pred_mod, mng_pred_mod, gap_pred_mod, leng_pred_mod) %>%
  reduce(full_join) %>%
  mutate(signficant = case_when(
    p.value < 0.05 ~ "Signifcant",
    p.value >= 0.05 ~ "Non-Signficant"
  )) %>%
  mutate(model = "Modification")

# Predictability Regression - Temporal ------------------------------------

pt_mod <- lm(log(pred_temp) ~ access_typ + mng_ag_lev + gap_status + shape_leng + outside_pop, data = human)

MASS::stepAIC(pt_mod, direction = "both")

pt_mod2 <- lm(log(pred_temp) ~ mng_ag_lev + gap_status + outside_pop,
  data = human
)

anova(pt_mod, pt_mod2)

summary(pt_mod2)

mng_pred_pt <- avg_slopes(pt_mod2, variable = "mng_ag_lev")
gap_pred_pt <- avg_slopes(pt_mod2, variable = "gap_status")
pop_pred_pt <- avg_slopes(pt_mod2, variable = "outside_pop")

pred_pt <- list(mng_pred_pt, gap_pred_pt, pop_pred_pt) %>%
  reduce(full_join) %>%
  mutate(signficant = case_when(
    p.value < 0.05 ~ "Signifcant",
    p.value >= 0.05 ~ "Non-Signficant"
  )) %>%
  mutate(model = "Temporal")

# Plot --------------------------------------------------------------------

pred_models <- list(pred_pca, pred_peak, pred_mod, pred_sp, pred_pt) %>%
  reduce(full_join) %>%
  mutate(contrast = case_when(
    term == "shape_leng" ~ "PA Size",
    term == "outside_pop" ~ "Outside Population",
    TRUE ~ contrast
  )) %>%
  mutate(contrast = as.factor(contrast)) %>%
  mutate(contrast = fct_relevel(contrast, c(
    "Open Access - No Public Access",
    "Restricted Access - No Public Access",
    "Unknown Access - No Public Access",
    "County - City", "Federal - City",
    "Home Owners Association - City",
    "Joint - City", "Non Profit - City",
    "Private - City", "Special District - City",
    "State - City", "Tribal - City",
    "GAP 2 - GAP 1", "GAP 3 - GAP 1", "GAP 4 - GAP 1",
    "PA Size", "Outside Population"
  ))) %>%
  mutate(model = as.factor(model)) %>%
  mutate(model = fct_relevel(model, c("PCA", "Modification", "Spatial", "Temporal", "Peak")))

myLoc1 <-
  (which(levels(pred_models$contrast) == "Unknown Access - No Public Access") +
    which(levels(pred_models$contrast) == "County - City")) /
    2

myLoc2 <-
  (which(levels(pred_models$contrast) == "Tribal - City") +
    which(levels(pred_models$contrast) == "GAP 2 - GAP 1")) /
    2

myLoc3 <-
  (which(levels(pred_models$contrast) == "GAP 4 - GAP 1") +
    which(levels(pred_models$contrast) == "PA Size")) /
    2

p <- ggplot(pred_models) +
  geom_pointrange(aes(contrast, y = estimate, ymin = conf.low, ymax = conf.high, shape = signficant)) +
  geom_pointrange(aes(contrast, y = estimate, ymin = conf.low, ymax = conf.high, shape = signficant, color = change)) +
  geom_vline(aes(xintercept = myLoc1),  color = "red", alpha = 0.3) +
  geom_vline(aes(xintercept = myLoc2),  color = "red", alpha = 0.3) +
  geom_vline(aes(xintercept = myLoc3), color = "red", alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = "dashed",  alpha = 0.3) +
  facet_wrap(~model, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  scale_shape_manual(values = c(2, 17)) +
  scale_color_viridis_d(end = 0.9, na.value = "black", na.translate = FALSE) +
  guides(color = guide_legend(nrow = 2)) +
  guides(shape = guide_legend(nrow = 2)) +
  labs(
    x = "",
    y = "Slope Estimate",
    shape = "",
    color = ""
  )
p

ggsave(p, file = paste0("regression.pdf"), path = here::here("analysis", "figs"), height = 12, width = 10)
