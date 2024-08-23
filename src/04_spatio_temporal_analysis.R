library(tidyverse)
library(nnet)
library(gtsummary)
library(broom)
# To plot predicted values
library(marginaleffects)
library(mgcv)
library(patchwork)
library(sf)
library(ggrepel)

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

highlight_parks <- c(14310, 22386)

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  filter(!is.na(hex_int)) %>%
  filter(suid_nma %in% human$suid_nma)

temporal_data <- read_csv(here::here("data", "processed_data", "temporal_analysis.csv")) %>%
  select(suid_nma, date, sg, park_name) %>%
  filter(!is.na(sg)) %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(suid_nma %in% human$suid_nma)%>% 
  mutate(year = year(date))
  

# Functions ---------------------------------------------------------------

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
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

park_models <- spatial_data %>% 
  select(hex_int, suid_nma, gHM, HFP2009, y, x) %>%
  na.omit() %>% 
  group_by(suid_nma) %>%
  mutate(count = n())%>%
  filter(count > 10) 

park_data = human %>% 
  filter(suid_nma %in% park_models$suid_nma)

yosemite = human %>% 
  filter(suid_nma == 14310)

yosemite_spatial = spatial_data %>% 
  filter(suid_nma == 14310) %>% 
  select(suid_nma, x, y) %>% 
  mutate(x = round(x),
         y = round(y)) %>% 
  distinct() %>% 
  full_join(yosemite)

gam1 = gam(hex_int ~ s(gHM, k = 6) + s(HFP2009, k = 6) + s(y, x, bs = "gp", m = 1, k = 6), data = park_models)

summary(gam1)
AIC(gam1)

ghm_pred <- predictions(gam1, variables = "gHM", by = "gHM")
hfp_pred <- predictions(gam1, variables = "HFP2009", by = "HFP2009")
xy_pred <- predictions(gam1, variables = "x", by = c("x", "y"))

# write_csv(ghm_pred, here::here("data", "processed_data", "ghm_pred.csv"))
# write_csv(hfp_pred, here::here("data", "processed_data", "hfp_pred.csv"))
# write_csv(xy_pred, here::here("data", "processed_data", "xy_pred.csv"))

p1 <- plot_predictions(gam1,  condition = "gHM") +
  geom_rug(data = park_data, aes(gHM_mean),  alpha = 1/2, outside = TRUE, sides = "b") +
  scale_y_log10() +
  theme_bw() +
  labs(
    x = "Global Human Modification",
    y = "Strava Value"
  )+
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_blank()) +
  geom_point(data = yosemite, aes(gHM_mean, hx_pp_r)) +
  geom_label_repel(
    data = yosemite, aes(gHM_mean, hx_pp_r, label = park_name),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

p2 <- plot_predictions(gam1, condition = "HFP2009") +
  geom_rug(data = park_data, aes(HFP_mean),  alpha = 1/2, outside = TRUE, sides = "b") +
  scale_y_log10() +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "Strava Value"
  ) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_blank())+
  geom_point(data = yosemite, aes(HFP_mean, hx_pp_r)) +
  geom_label_repel(
    data = yosemite, aes(HFP_mean, hx_pp_r, label = park_name),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

p3 <- plot_predictions(gam1, condition  = c("x", "y")) +
  scale_y_log10() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Strava Value",
    color = "Latitude",
    fill ="Latitude"
  ) +
  scale_color_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE), labels = rev(c("42.00", 39.83, 36.81, 34.91, 32.56))) +
  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE), labels = rev(c("42.00", 39.83, 36.81, 34.91, 32.56))) +
  coord_cartesian(clip = "off")+
  geom_point(data = yosemite_spatial, aes(x, hx_pp_r), color = "#addc30") +
  geom_label_repel(
    data = yosemite_spatial, aes(x, hx_pp_r, label = park_name),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

plot <- (p1 + p2) / p3 + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig2.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

### Dispersion and Predictability

# Dispersion

disp <- var(spatial_data$hex_int) / mean(spatial_data$hex_int)

# Constancy: 1 – H(x)/log(n), where H(x) is information entropy or  1/sd

const <- 1 - entropy(spatial_data$hex_int) / log(nrow(spatial_data))

# Consistency: [H(x) + H(y) – H(XY)]/log(n) where y is spatial position

spatial_data <- spatial_data %>%
  mutate(haversine = single_pt_haversine(y, x))

contin <- (entropy(spatial_data$hex_int) + entropy(spatial_data$haversine) - cross.entropy(spatial_data$hex_int, spatial_data$haversine)) / log(nrow(spatial_data))

pred <- const + contin

mod_entropy <- spatial_data %>%
  mutate(mod = gHM) %>%
  filter(!is.na(mod)) %>%
  group_by(suid_nma) %>%
  summarise(
    mod_disp = var(mod) / mean(mod),
    const_mod = 1 - entropy(mod) / log(n()),
    contin_mod = (entropy(mod) + entropy(haversine) - cross.entropy(mod, haversine)) / log(n()),
    pred_mod = const_mod + contin_mod
  ) %>%
  left_join(cpad_super) %>%
  st_as_sf()

spatial_entropy <- spatial_data %>%
  group_by(suid_nma) %>%
  summarize(
    disp = var(hex_int) / mean(hex_int),
    entropy = entropy(hex_int),
    inv_sd = 1 / sd(hex_int),
    const = 1 - entropy(hex_int) / log(n()),
    contin = (entropy(hex_int) + entropy(haversine) - cross.entropy(hex_int, haversine)) / log(n()),
    pred = const + contin
  ) %>%
  left_join(cpad_super) %>%
  select(-geometry)

park_entropy <- full_join(mod_entropy, spatial_entropy)

parks_highlight <- park_entropy %>% 
  filter(suid_nma %in% highlight_parks)

p1 <- ggplot() +
  geom_sf(data =  park_entropy, aes(fill = mod_disp), color = NA, linewidth = 0.1) +
  geom_sf(data =  parks_highlight, color = "red", fill = NA, linewidth = 1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Index of Dispersion for Modification")

p2 <- ggplot() +
  geom_sf(data =  park_entropy,aes(fill = pred_mod), color = NA, linewidth = 0.1) +
  geom_sf(data =  parks_highlight, color = "red", fill = NA, linewidth = 1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Predictibility of Modification")

p3 <- ggplot() +
  geom_sf(data =  park_entropy,aes(fill = disp), color = NA, linewidth = 0.1) +
  geom_sf(data =  parks_highlight, color = "red", fill = NA, linewidth = 1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Index of Dispersion for Strava")

p4 <- ggplot() +
  geom_sf(data =  park_entropy,aes(fill = pred), color = NA, linewidth = 0.1) +
  geom_sf(data =  parks_highlight, color = "red", fill = NA, linewidth = 1) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "Predictibility of Strava")

plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig3.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

write_csv(park_entropy %>% st_drop_geometry() %>% select(-mod_disp), here::here("data", "processed_data", "spatial_predictability.csv"))

# Temporal Analysis -------------------------------------------------------

## Time series for representative park

temp_enough <- temporal_data %>%
  filter(count == 837)

temp_2019 <- temporal_data %>%
  filter(year == 2019) %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>% 
  filter(count == 365)

temp_2020 <- temporal_data %>%
  filter(year == 2020) %>%
  group_by(suid_nma) %>%
  mutate(count = n())  %>% 
  filter(count == 366)

decom_period <- function(suid, data_set) {
  park <- data_set %>%
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
  int_data <- decom_period(unique_ids[i], temp_enough)

  period <- rbind(period, int_data)
}

period_maxs <- period %>%
  filter(p_value < 0.05) %>%
  mutate(period = 1 / freq) %>%
  filter(period < 837) %>%
  mutate(mag = floor(log10(specs))) %>%
  group_by(suid_nma) %>%
  mutate(max_mag = max(mag)) %>%
  ungroup() %>%
  filter(mag == max_mag) %>%
  select(-mag, -max_mag) %>% 
  mutate(year = NA)

top_periods1 <- period_maxs %>%
  group_by(period) %>%
  mutate(n = n()) %>%
  mutate(percent = n / length(unique_ids) * 100) %>%
  group_by(suid_nma) %>%
  slice_max(order_by = specs, n = 3) %>%
  mutate(rank = rank(specs))

period <- data.frame()

unique_ids <- unique(temp_2019$suid_nma)

for (i in 1:length(unique_ids)) {
  int_data <- decom_period(unique_ids[i], temp_2019)
  
  period <- rbind(period, int_data)
}

period_maxs_2019 <- period %>%
  filter(p_value < 0.05) %>%
  mutate(period = 1 / freq) %>%
  filter(period < 365) %>%
  mutate(mag = floor(log10(specs))) %>%
  group_by(suid_nma) %>%
  mutate(max_mag = max(mag)) %>%
  ungroup() %>%
  filter(mag == max_mag) %>%
  select(-mag, -max_mag) %>% 
  mutate(year = 2019)

top_periods1_2019 <- period_maxs_2019 %>%
  group_by(period) %>%
  mutate(n = n()) %>%
  mutate(percent = n / length(unique_ids) * 100) %>%
  group_by(suid_nma) %>%
  slice_max(order_by = specs, n = 3) %>%
  mutate(rank = rank(specs))

period <- data.frame()

unique_ids <- unique(temp_2020$suid_nma)

for (i in 1:length(unique_ids)) {
  int_data <- decom_period(unique_ids[i], temp_2020)
  
  period <- rbind(period, int_data)
}

period_maxs_2020 <- period %>%
  filter(p_value < 0.05) %>%
  mutate(period = 1 / freq) %>%
  filter(period < 366) %>%
  mutate(mag = floor(log10(specs))) %>%
  group_by(suid_nma) %>%
  mutate(max_mag = max(mag)) %>%
  ungroup() %>%
  filter(mag == max_mag) %>%
  select(-mag, -max_mag) %>% 
  mutate(year = 2020)

top_periods1_2020 <- period_maxs_2020 %>%
  group_by(period) %>%
  mutate(n = n()) %>%
  mutate(percent = n / length(unique_ids) * 100) %>%
  group_by(suid_nma) %>%
  slice_max(order_by = specs, n = 3) %>%
  mutate(rank = rank(specs))

period_max_all = list(period_maxs, period_maxs_2019, period_maxs_2020) %>% 
  reduce(full_join)

top_periods_all = list(top_periods1, top_periods1_2019, top_periods1_2020) %>% 
  reduce(full_join)

temp_highlight <- top_periods1 %>%
  filter(suid_nma %in% c(highlight_parks)) %>%
  left_join(cpad_super) %>%
  mutate(label = paste(park_name, "Top Peak")) %>%
  filter(rank == 1)

temp_highlight_2019 <- top_periods1_2019  %>%
  filter(suid_nma %in% c(highlight_parks)) %>%
  left_join(cpad_super) %>%
  mutate(label = paste(park_name, "Top Peak")) %>%
  filter(rank == 1)

temp_highlight_2020  <- top_periods1_2020  %>%
  filter(suid_nma %in% c(highlight_parks)) %>%
  left_join(cpad_super) %>%
  mutate(label = paste(park_name, "Top Peak")) %>%
  filter(rank == 1)

write_csv(period_max_all, here::here("analysis", "periodicity.csv"))

write_csv(top_periods_all, here::here("analysis", "periodicity_max.csv"))

colors <- c("1" = "black", "2" = "blue", "3" = "darkgreen")

p1 <- ggplot() +
  geom_vline(aes(xintercept = c(30, 90, 180, 365, 365 + 365)), color = "red", alpha = 0.3, linetype = "dashed") +
  geom_vline(data = temp_highlight, aes(xintercept = period), color = "black", linetype = "dashed") +
  geom_density(data = top_periods1 %>% filter(rank == 1), aes(period, color = "1"), linewidth = 1) +
  geom_density(data = top_periods1 %>% filter(rank == 2), aes(period, color = "2"), linetype = "dashed", linewidth = 1) +
  geom_density(data = top_periods1 %>% filter(rank == 3), aes(period, color = "3"), linetype = "dotdash", linewidth = 1) +
  theme_bw() +
  # scale_x_log10() +
  labs(
    x = "Period (days)",
    y = "Density",
    color = "Peak Rank"
  ) +
  scale_color_manual(values = colors) +
  geom_label_repel(
    data = temp_highlight, aes(x = period, y = c(0.01, 0.005), label = label),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

p2 <- ggplot() +
  geom_vline(aes(xintercept = c(30, 90, 180, 365)), color = "red", alpha = 0.3, linetype = "dashed") +
  geom_vline(data = temp_highlight_2019, aes(xintercept = period), color = "black", linetype = "dashed") +
  geom_density(data = top_periods1_2019 %>% filter(rank == 1), aes(period, color = "1"), linewidth = 1) +
  geom_density(data = top_periods1_2019 %>% filter(rank == 2), aes(period, color = "2"), linetype = "dashed", linewidth = 1) +
  geom_density(data = top_periods1_2019 %>% filter(rank == 3), aes(period, color = "3"), linetype = "dotdash", linewidth = 1) +
  theme_bw() +
  # scale_x_log10() +
  labs(
    x = "Period (days)",
    y = "Density",
    color = "Peak Rank"
  ) +
  scale_color_manual(values = colors) +
  geom_label_repel(
    data = temp_highlight_2019, aes(x = period, y = c(0.01, 0.005), label = label),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

p3 <- ggplot() +
  geom_vline(aes(xintercept = c(30, 90, 180, 365)), color = "red", alpha = 0.3, linetype = "dashed") +
  geom_vline(data = temp_highlight_2020, aes(xintercept = period), color = "black", linetype = "dashed") +
  geom_density(data = top_periods1_2020 %>% filter(rank == 1), aes(period, color = "1"), linewidth = 1) +
  geom_density(data = top_periods1_2020 %>% filter(rank == 2), aes(period, color = "2"), linetype = "dashed", linewidth = 1) +
  geom_density(data = top_periods1_2020 %>% filter(rank == 3), aes(period, color = "3"), linetype = "dotdash", linewidth = 1) +
  theme_bw() +
  # scale_x_log10() +
  labs(
    x = "Period (days)",
    y = "Density",
    color = "Peak Rank"
  ) +
  scale_color_manual(values = colors) +
  geom_label_repel(
    data = temp_highlight_2020, aes(x = period, y = c(0.015, 0.01), label = label),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

ggsave(p1 / p2 / p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect"), file = paste0("fig5.pdf"), path = here::here("analysis", "figs"), height = 18, width = 15)

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

predictability <- full_join(park_entropy, park_entropy_temp)

pred_highlight = predictability %>% 
  filter(suid_nma %in% c(highlight_parks))

write_csv(predictability %>% st_drop_geometry() %>% select(-mod_disp), here::here("data", "processed_data", "predictability.csv"))

p1 <- ggplot() +
  geom_point(data = predictability, aes(pred, pred_mod)) +
  geom_point(data = pred_highlight, aes(pred, pred_mod), color = "red", size = 2) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  labs(
    x = "Predictibility of Strava",
    y = "Predictibility of Modification"
  ) +
  geom_label_repel(
    data = pred_highlight, aes(pred, pred_mod, label = park_name),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black",vjust = "outward", hjust = "outward"
  )

p2 <- ggplot() +
  geom_point(data = predictability, aes(pred, pred_temp)) +
  geom_point(data = pred_highlight, aes(pred, pred_temp), color = "red", size = 2) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Predictibility of Strava",
    y = "Predictibility of Safegraph"
  ) +
  geom_label_repel(
    data = pred_highlight, aes(pred, pred_temp, label = park_name),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black",vjust = "outward", hjust = "outward"
  )

p3 <- ggplot() +
  geom_point(data = predictability, aes(disp, mod_disp)) +
  geom_point(data = pred_highlight, aes(disp, mod_disp), color = "red", size = 2) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Index of Dispersion for Strava",
    y = "Index of Dispersion for Modification"
  ) +
  geom_label_repel(
    data = pred_highlight, aes(disp, mod_disp, label = park_name),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

plot <- p1 + p2 + p3 + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig4.pdf"), path = here::here("analysis", "figs"), height = 6, width = 15)
