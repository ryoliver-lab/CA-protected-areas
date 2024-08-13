library(tidyverse)
library(nnet)
library(gtsummary)
library(broom)
# To plot predicted values
library(marginaleffects)
# For the Wald Test
library(car)
library(mgcv)
library(patchwork)
library(sf)

# Data Input --------------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

sg <- read_csv(here::here("data", "processed_data", "cpad_sg.csv"))
strava <- st_read(here::here("data", "processed_data", "cpad_strava.shp"))
ghm <- st_read(here::here("data", "processed_data", "cpad_ghm.shp"))
hfp <- st_read(here::here("data", "processed_data", "cpad_hfp.shp"))

human <- list(sg, strava, ghm, hfp) %>%
  reduce(full_join) %>%
  st_as_sf()

rm(sg, strava, ghm, hfp)

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
    HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv
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
  ))

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  filter(!is.na(hex_int)) %>%
  filter(suid_nma %in% human$suid_nma)

temporal_data <- read_csv(here::here("data", "processed_data", "temporal_analysis.csv")) %>%
  select(suid_nma, date, sg, park_name) %>%
  full_join(classes) %>%
  filter(!is.na(sg)) %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(suid_nma %in% human$suid_nma)

# Functions ---------------------------------------------------------------

normalize <- function(x, na.rm = TRUE) {
  return((x - min(x)) / (max(x) - min(x)))
}

single_pt_haversine <- function(lat, lng) {
  r <- 6371 # Earth's radius (km). Have r = 3956 if you want miles

  # 'Single-point' Haversine formula
  a <- sin(lat / 2)**2 + cos(lat) * sin(lng / 2)**2
  d <- 2 * r * asin(sqrt(a))

  return(d)
}

entropy <- function(target) {
  freq <- table(target) / length(target)
  # vectorize
  vec <- as.data.frame(freq)[, 2]
  # drop 0 to avoid NaN resulting from log2
  vec <- vec[vec > 0]
  # compute entropy
  -sum(vec * log2(vec))
}

cross.entropy <- function(p, phat) {
  x <- 0
  for (i in 1:length(p)) {
    x <- x + (p[i] * log(phat[i]))
  }
  return(-x)
}

# Human Map ---------------------------------------------------------------


# Spatial Analysis --------------------------------------------------------

gam1 <- gam(hex_int ~ s(gHM) + s(HFP2009) + s(y, x, bs = "gp", m = 1), data = spatial_data, family = nb)

summary(gam1)
AIC(gam1)

ghm_slopes = slopes(gam1, variables = "gHM", by = "gHM")
hfp_slopes = slopes(gam1, variables = "HFP2009", by = "HFP2009")
xy_slopes = slopes(gam1, variables = "longitude", by = "latitude")

p1 <-  +
  theme_bw() +
  labs(
    x = "Global Human Modification",
    y = "Strava Value"
  )

p2 <- +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "Strava Value"
  )

p3 <- plot_slopes(gam1, variables = "longitude", by = "latitude") +
  geom_rug() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Strava Value"
  )

plot <- (p1 + p2) / p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, file = paste0("fig4.pdf"), path = here::here("analysis", "figs"), height = 15, width = 25)

### Dispersion and Predictibility

# Dispersion

disp <- var(spatial_data$hex_int) / mean(spatial_data$hex_int)

# Constancy: 1 – H(x)/log(n), where H(x) is information entropy or  1/sd

const <- 1 - entropy(spatial_data$hex_int) / log(nrow(spatial_data))

# Consistency: [H(x) + H(y) – H(XY)]/log(n) where y is spatial position

spatial_data <- spatial_data %>%
  mutate(haversine = single_pt_haversine(y, x))

contin <- (entropy(spatial_data$hex_int) + entropy(spatial_data$haversine) - cross.entropy(spatial_data$hex_int, spatial_data$haversine)) / log(nrow(spatial_data))

pred <- const + contin

park_entropy <- spatial_data %>%
  mutate(
    gHM_scale = normalize(gHM),
    HFP_scale = normalize(HFP2009)
  ) %>%
  mutate(mod = (gHM_scale + HFP_scale) / 2) %>%
  group_by(suid_nma) %>%
  summarise(
    mod_disp = var(mod) / mean(mod),
    disp = var(hex_int) / mean(hex_int),
    entropy = entropy(hex_int),
    inv_sd = 1 / sd(hex_int),
    const = 1 - entropy(hex_int) / log(n()),
    contin = (entropy(hex_int) + entropy(haversine) - cross.entropy(hex_int, haversine)) / log(n()),
    pred = const + contin,
    const_mod = 1 - entropy(mod) / log(n()),
    contin_mod = (entropy(mod) + entropy(haversine) - cross.entropy(mod, haversine)) / log(n()),
    pred_mod = const_mod + contin_mod
  ) %>%
  left_join(cpad_super) %>%
  st_as_sf()

p1 <- ggplot(park_entropy) +
  geom_sf(aes(fill = mod_disp), color = NA, linewidth = 0.1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Index of Dispersion for Modification")

p2 <- ggplot(park_entropy) +
  geom_sf(aes(fill = pred_mod), color = NA, linewidth = 0.1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Predictibility of Modification")

p3 <- ggplot(park_entropy) +
  geom_sf(aes(fill = disp), color = NA, linewidth = 0.1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Index of Dispersion for Strava")

p4 <- ggplot(park_entropy) +
  geom_sf(aes(fill = pred), color = NA, linewidth = 0.1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Predictibility of Strava")

plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig3.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

write_csv(park_entropy, here::here("data", "processed_data", "spatial_predictability.csv"))

# Temporal Analysis -------------------------------------------------------

## Time series for representative park

temp_enough <- temporal_data %>%
  filter(count == 322)

decom_period <- function(suid) {
  park <- temp_enough %>%
    filter(suid_nma == suid) %>%
    mutate(year = year(date))

  park_ts <- ts(park$sg, freq = 30)

  ### Decomposition

  # decomp <- decompose(park_ts)
  # plot(decomp)

  ### Periodicity

  freq <- TSA::periodogram(park_ts)[["freq"]]
  specs <- TSA::periodogram(park_ts)[["spec"]]

  # TSA::periodogram(park_ts, plot = TRUE) # compute and plot periodograms
  # abline(h = 0)

  df <- data.frame(freq = freq, specs = specs)

  df$p_value <- GeneCycle::fisher.g.test(park_ts)

  df$suid_nma <- suid

  return(df)
}

period <- data.frame()

unique_ids <- unique(temp_enough$suid_nma)

for (i in 1:length(unique_ids)) {
  int_data <- decom_period(unique_ids[i])

  period <- rbind(period, int_data)
}

period_maxs <- period %>%
  filter(p_value < 0.05) %>%
  mutate(period = 1 / freq) %>%
  mutate(mag = floor(log10(specs))) %>%
  group_by(suid_nma) %>%
  mutate(max_mag = max(mag)) %>%
  ungroup() %>%
  filter(mag == max_mag) %>%
  select(-mag, -max_mag) %>%
  left_join(classes)

top_periods <- period_maxs %>%
  group_by(period) %>%
  summarize(n = n()) %>%
  mutate(percent = n / length(unique_ids) * 100) %>%
  slice_max(order_by = percent, n = 5)

# write_csv(period_maxs, here::here("analysis", "periodicity.csv"))

p <- ggplot(period_maxs %>% filter(!is.na(class))) +
  geom_vline(data = top_periods, aes(xintercept = period), color = "red", alpha = 0.3, linetype = "dashed") +
  geom_density(aes(period, color = class)) +
  theme_bw() +
  scale_x_log10() +
  scale_color_viridis_d() +
  labs(
    x = "Period (days)",
    y = "Density",
    color = "Human Use Class"
  )

ggsave(p, file = paste0("fig5.pdf"), path = here::here("analysis", "figs"), height = 10, width = 10)

### Predictability

# Constancy: 1 – H(x)/log(n), where H(x) is information entropy or  1/sd

const_temp <- 1 - entropy(temporal_data$sg) / log(nrow(temporal_data))

# Consistency: [H(x) + H(y) – H(XY)]/log(n) where y is spatial position

temporal_data <- temporal_data %>%
  mutate(date_num = as.numeric(date))

# need to fix
contin_temp <- (entropy(temporal_data$sg) + entropy(temporal_data$date_num) - cross.entropy(temporal_data$sg, temporal_data$date_num)) / log(nrow(temporal_data))

pred_temp <- const_temp + contin_temp

park_entropy_temp <- temporal_data %>%
  group_by(suid_nma) %>%
  summarise(
    entropy_temp = entropy(sg),
    inv_sd_temp = 1 / sd(sg),
    const_temp = 1 - entropy(sg) / log(n()),
    contin_temp = (entropy(sg) + entropy(date_num) - cross.entropy(sg, date_num)) / log(n()),
    pred_temp = const_temp + contin_temp
  )


# Combined Plots ----------------------------------------------------------

predictability = full_join(park_entropy, park_entropy_temp)

ggplot(predictability) +
  geom_point(aes(pred, pred_mod)) +
  theme_bw() +
  labs(
    x = "Predictibility of Strava",
    y = "Predictibility of Modification"
  )

ggplot(predictability) +
  geom_point(aes(pred, pred_temp)) +
  theme_bw() +
  labs(
    x = "Predictibility of Strava",
    y = "Predictibility of Safegraph"
  )

ggplot(predictability) +
  geom_point(aes(disp, mod_disp)) +
  theme_bw() +
  labs(
    x = "Index of Dispersion for Strava",
    y = "Index of Dispersion for Modification"
  )
