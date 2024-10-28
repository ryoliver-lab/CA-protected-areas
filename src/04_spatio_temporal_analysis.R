library(tidyverse)
library(nnet)
library(gtsummary)
library(broom)
# To plot predicted values
library(marginaleffects)
library(emmeans)
library(mgcv)
library(patchwork)
library(sf)
library(ggrepel)
library(ggridges)
library(ggExtra)
library(gghalves)

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
    sg, mean_sg, hx_pp_r, mx_hx__, mn_hx__, suid_nma,
    access_typ, park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    gHM_min, gHM_mean, gHM_max, gHM_median, gHM_sd, gHM_range, gHM_cv, HFP_min,
    HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv, year
  ) %>%
  mutate(
    hex_range = mx_hx__ - mn_hx__,
    HFP_mean = case_when(
      HFP_mean < 0 ~ 0,
      TRUE ~ HFP_mean
    ),
    HFP_median = case_when(
      HFP_median < 0 ~ 0,
      TRUE ~ HFP_median
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
  filter(!is.na(year)) %>%
  ungroup() %>%
  mutate(hfp_quartile = case_when(
    HFP_median < quantile(HFP_median, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
    HFP_median >= quantile(HFP_median, probs = 0.25, na.rm = TRUE) & HFP_median < quantile(HFP_median, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
    HFP_median <= quantile(HFP_median, probs = 0.75, na.rm = TRUE) & HFP_median >= quantile(HFP_median, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
    HFP_median > quantile(HFP_median, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  ))

hfp <- st_read(here::here("data", "processed_data", "in_out_hfp.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  st_drop_geometry() %>%
  full_join(cpad_super) %>%
  st_drop_geometry() %>%
  select(-geometry) %>%
  full_join(human %>% select(suid_nma, gap_status))

highlight_parks <- c(14310, 22386, 33731, 30603)

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  filter(!is.na(hex_int)) %>%
  filter(suid_nma %in% human$suid_nma) %>%
  full_join(human %>% select(suid_nma, gap_status))

temporal_data <- read_csv(here::here("data", "processed_data", "temporal_analysis.csv")) %>%
  select(suid_nma, date, sg, park_name) %>%
  filter(!is.na(sg)) %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(suid_nma %in% human$suid_nma) %>%
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

# Checks ---------------------------------------------------------------

sg_have <- human %>%
  filter(!is.na(sg)) %>%
  mutate(sg_present = "Yes")

sg_havenot <- human %>%
  filter(is.na(sg)) %>%
  mutate(sg_present = "No")

p1 <- ggplot() +
  geom_sf(data = sg_have, fill = "blue", color = "transparent") +
  geom_sf(data = sg_havenot, fill = "red", color = "red") +
  theme_bw()
p1

ggsave(p1, file = paste0("missing_parks.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

missing_sum <- rbind(sg_have, sg_havenot) %>%
  st_drop_geometry() %>%
  group_by(sg_present) %>%
  summarize(
    ghm_mean = mean(gHM_mean, na.rm = TRUE),
    ghm_sd = sd(gHM_mean, na.rm = TRUE),
    ghm_max = max(gHM_mean, na.rm = TRUE),
    ghm_min = min(gHM_mean, na.rm = TRUE),
    hfp_mean = mean(HFP_mean, na.rm = TRUE),
    hfp_sd = sd(HFP_mean, na.rm = TRUE),
    hfp_max = max(HFP_mean, na.rm = TRUE),
    hfp_min = min(HFP_mean, na.rm = TRUE),
    strava_mean = mean(hx_pp_r, na.rm = TRUE),
    strava_sd = sd(hx_pp_r, na.rm = TRUE),
    strava_max = max(hx_pp_r, na.rm = TRUE),
    strava_min = min(hx_pp_r, na.rm = TRUE)
  )

cor_check <- human %>%
  st_drop_geometry() %>%
  select(outside_pop, HFP_mean) %>%
  drop_na()

cor(cor_check$outside_pop, cor_check$HFP_mean)


# Scatter Plot ------------------------------------------------------------

human_highlight <- human %>%
  filter(suid_nma %in% highlight_parks)

human_sum <- human %>%
  st_drop_geometry() %>%
  select(-year, -sg) %>%
  distinct()

human_highlight_sum <- human_sum %>%
  filter(suid_nma %in% highlight_parks)

p <- ggplot(human_sum) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(HFP_median, mean_sg, color = gap_status), alpha = 0.7) +
  geom_point(data = human_highlight_sum, aes(HFP_median, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Relative Tourism",
    color = "Management"
  ) +
  scale_y_log10() +
  geom_label_repel(
    data = human_highlight_sum, aes(HFP_median, mean_sg, label = park_name),
    size = 4,
    alpha = 0.8,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  ) +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom")

p1 <- ggMarginal(p, type = "density", fill = "grey")

ggsave(p1, file = paste0("fig1.pdf"), path = here::here("analysis", "figs"), height = 8, width = 10)

hfp <- hfp %>%
  filter(!is.na(HFP2009)) %>%
  rowwise() %>%
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4",
    loc == "Outside Protected Areas" ~ "NA"
  ))

p2 <- ggplot(hfp %>% filter(!is.na(HFP2009))) +
  geom_density_ridges(aes(x = HFP2009, y = loc, fill = gap_status), alpha = 0.5, scale = 3) +
  geom_vline(data = human_highlight_sum, aes(xintercept = HFP_median), color = "red", linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "",
    fill = "GAP Status"
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  geom_label_repel(
    data = human_highlight_sum, aes(x = HFP_median, y = 4, label = park_name),
    size = 4,
    alpha = 0.65,
    min.segment.length = 1,
    arrow = arrow(length = unit(0.1, "inches")),
    box.padding = 1,
    color = "black"
  )

ggsave(p2, file = paste0("fig2.pdf"), path = here::here("analysis", "figs"), height = 8, width = 10)

# Spatial Analysis --------------------------------------------------------

park_models <- spatial_data %>%
  select(hex_int, suid_nma, gHM, HFP2009, y, x, gap_status) %>%
  na.omit() %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(count > 10)

park_data = human %>% 
  filter(suid_nma %in% park_models$suid_nma)

gam1 <- gam(hex_int ~ s(HFP2009, k = 6) + s(y, x, bs = "gp", m = 1, k = 6) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models)
gam2 <- gam(hex_int ~ s(HFP2009, k = 6) + s(y, x, bs = "gp", m = 1, k = 6) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(gap_status == "GAP 1"))
gam3 <- gam(hex_int ~ s(HFP2009, k = 6) + s(y, x, bs = "gp", m = 1, k = 6) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(gap_status == "GAP 2"))
gam4 <- gam(hex_int ~ s(HFP2009, k = 6) + s(y, x, bs = "gp", m = 1, k = 6) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(gap_status == "GAP 3"))
gam5 <- gam(hex_int ~ s(HFP2009, k = 6) + s(y, x, bs = "gp", m = 1, k = 6) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(gap_status == "GAP 4"))

summary(gam1)
summary(gam2)
summary(gam3)
summary(gam4)
summary(gam5)

p1 <- plot_predictions(gam1, condition = "HFP2009") +
  geom_rug(data = park_data, aes(HFP_median), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "Strava Value"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

p2 <- plot_predictions(gam2, condition = "HFP2009") +
  geom_rug(data = park_data %>% filter(gap_status == "GAP 1"), aes(HFP_median), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "Strava Value",
    title = "GAP 1"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p3 <- plot_predictions(gam3, condition = "HFP2009") +
  geom_rug(data = park_data %>% filter(gap_status == "GAP 2"), aes(HFP_median), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "Strava Value",
    title = "GAP 2"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p4 <- plot_predictions(gam4, condition = "HFP2009") +
  geom_rug(data = park_data %>% filter(gap_status == "GAP 3"), aes(HFP_median), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "Strava Value",
    title = "GAP 3"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p5 <- plot_predictions(gam5, condition = "HFP2009") +
  geom_rug(data = park_data %>% filter(gap_status == "GAP 4"), aes(HFP_median), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw() +
  labs(
    x = "Human Footprint Index",
    y = "Strava Value",
    title = "GAP 4"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

plot <- (p1) / ((p2 + p3) / (p4 + p5)) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig3.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

# Temporal Analysis -------------------------------------------------------

temporal_data <- temporal_data %>%
  mutate(date_j = yday(date)) %>%
  mutate(date_j = case_when(
    year == 2020 ~ date_j + 365,
    year == 2021 ~ date_j + 731,
    year == 2019 ~ date_j
  ))

highlight_temp <- temporal_data %>%
  filter(suid_nma %in% highlight_parks)

temporal_bins <- temporal_data %>%
  group_by(suid_nma) %>%
  mutate(
    max = max(sg, na.rm = TRUE),
    min = min(sg, na.rm = TRUE),
    quartile_1 = quantile(sg, probs = 0.25),
    quartile_2 = quantile(sg, probs = 0.5),
    quartile_3 = quantile(sg, probs = 0.75)
  ) %>%
  ungroup() %>%
  mutate(bin = case_when(
    sg < quartile_1 ~ "1st Quartile",
    sg > quartile_3 ~ "4th Quartile",
    sg >= quartile_1 & sg < quartile_2 ~ "2nd Quartile",
    sg >= quartile_2 & sg <= quartile_3 ~ "3rd Quartile"
  ))

temp_date <- temporal_bins %>%
  group_by(date_j, bin) %>%
  mutate(date_count = n())

p1 <- ggplot(temp_date) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c() +
  theme_bw() +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count"
  ) +
  scale_x_date()

p2 <- ggplot(temp_date) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  geom_line(data = highlight_temp, aes(date, sg, color = as.factor(park_name)), linewidth = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    x = "Date",
    y = "Relative Tourism",
    color = ""
  ) +
  scale_x_date()

plot <- p1 / p2 + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig4.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)


# Change in SG ------------------------------------------------------------

human_w_hfp <- human %>%
  filter(!is.na(hfp_quartile))

p1 <- ggplot(human_w_hfp %>% filter(year != 2021)) +
  geom_line(aes(as.factor(year), sg, group = suid_nma), alpha = 0.8) +
  geom_line(data = human_highlight %>% filter(year != 2021), aes(as.factor(year), sg, group = suid_nma), color = "red") +
  geom_half_violin(data = human_w_hfp %>% filter(year == 2019), aes(as.factor(year), sg), fill = "grey") +
  geom_half_violin(data = human_w_hfp %>% filter(year == 2020), aes(as.factor(year), sg), side = "r", fill = "grey") +
  facet_wrap(~hfp_quartile) +
  theme_bw() +
  scale_y_log10() +
  labs(
    x = "Year",
    y = "Relative Tourism"
  ) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  )
p1

ggsave(p1, file = paste0("fig5.pdf"), path = here::here("analysis", "figs"), height = 8, width = 10)

# OLD ---------------------------------------------------------------------

# p1 = ggplot(data = temporal_data) +
#   geom_vline(aes(xintercept = 21), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 49), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 111), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 147), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 185), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 245), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 332), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 359), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 365), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 385), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 413), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 468), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 511), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 551), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 616), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 696), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 725), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 731), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 749), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 777), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 825), color = "red", alpha = 0.2) +
#   geom_vline(aes(xintercept = 440), color = "blue", alpha = 0.2) + #covid lockdown
#   geom_vline(aes(xintercept = 480), color = "blue", alpha = 0.2) + #earliest reopen
#   geom_line(data = temporal_data, aes(date_j, sg, group = as.factor(park_name)), alpha = 0.5, color = "grey", linewidth = 0.1) +
#   geom_line(data = highlight_temp, aes(date_j, sg, color = as.factor(park_name))) +
#   theme_bw() +
#   labs(x = "Julian Date",
#        y = "Relative Tourism",
#        color = "Park") +
#   theme_bw() +
#   scale_color_viridis_d(end = 0.9) +
#   scale_y_log10() +
#   facet_wrap(~quartile, ncol = 1, scales = "free_y") +
#   theme(strip.background = element_rect(fill = "white"),
#         strip.text = element_text(face = "bold"))
# p1

# ### Dispersion and Predictability
#
# # Dispersion
#
# disp <- var(spatial_data$hex_int) / mean(spatial_data$hex_int)
#
# # Constancy: 1 – H(x)/log(n), where H(x) is information entropy or  1/sd
#
# const <- 1 - entropy(spatial_data$hex_int) / log(nrow(spatial_data))
#
# # Consistency: [H(x) + H(y) – H(XY)]/log(n) where y is spatial position
#
# spatial_data <- spatial_data %>%
#   mutate(haversine = single_pt_haversine(y, x))
#
# contin <- (entropy(spatial_data$hex_int) + entropy(spatial_data$haversine) - cross.entropy(spatial_data$hex_int, spatial_data$haversine)) / log(nrow(spatial_data))
#
# pred <- const + contin
#
# mod_entropy <- spatial_data %>%
#   mutate(mod = gHM) %>%
#   filter(!is.na(mod)) %>%
#   group_by(suid_nma) %>%
#   summarise(
#     mod_disp = var(mod) / mean(mod),
#     const_mod = 1 - entropy(mod) / log(n()),
#     contin_mod = (entropy(mod) + entropy(haversine) - cross.entropy(mod, haversine)) / log(n()),
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(const_mod)) %>%
#   filter(!is.na(contin_mod)) %>%
#   mutate(
#     const_mod_scale = normalize(const_mod),
#     contin_mod_scale = normalize(contin_mod)
#   ) %>%
#   mutate(
#     pred_mod = const_mod_scale + contin_mod_scale
#   ) %>%
#   left_join(cpad_super) %>%
#   st_as_sf()
#
# spatial_entropy <- spatial_data %>%
#   group_by(suid_nma) %>%
#   summarize(
#     disp = var(hex_int) / mean(hex_int),
#     entropy = entropy(hex_int),
#     inv_sd = 1 / sd(hex_int),
#     const = 1 - entropy(hex_int) / log(n()),
#     contin = (entropy(hex_int) + entropy(haversine) - cross.entropy(hex_int, haversine)) / log(n())
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(const)) %>%
#   filter(!is.na(contin)) %>%
#   mutate(
#     const_scale = scale(const),
#     contin_scale = scale(contin)
#   ) %>%
#   mutate(
#     const_scale = const_scale[, 1],
#     contin_scale = contin_scale[, 1]
#   ) %>%
#   mutate(
#     pred = const_scale + contin_scale
#   ) %>%
#   left_join(cpad_super) %>%
#   select(-geometry)
#
# park_entropy <- full_join(mod_entropy, spatial_entropy) %>%
#   st_drop_geometry()
#
# ggplot() +
#   geom_point(data = park_entropy, aes(disp, mod_disp)) +
#   theme_bw() +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(
#     x = "Index of Dispersion for Strava",
#     y = "Index of Dispersion for Modification"
#   )
#
# strava_disp = park_entropy %>%
#   left_join(human) %>%
#   st_as_sf() %>%
#   select(suid_nma, park_name, disp, gHM_mean, HFP_mean, hx_pp_r) %>%
#   mutate(bin = case_when(
#     disp <= 0.00002 ~ "Low",
#     disp >= 1 ~ "High",
#     TRUE ~ "Med"
#   ))
#
# ggplot() +
#   geom_sf(data = strava_disp, aes(fill = bin), color = "transparent") +
#   scale_fill_viridis_d() +
#   theme_bw()
#
# strava_disp_sum = strava_disp %>%
#   st_drop_geometry() %>%
#   group_by(bin) %>%
#   summarize(
#     ghm_mean = mean(gHM_mean, na.rm = TRUE),
#     ghm_sd = sd(gHM_mean, na.rm = TRUE),
#     ghm_max = max(gHM_mean, na.rm = TRUE),
#     ghm_min = min(gHM_mean, na.rm = TRUE),
#     hfp_mean = mean(HFP_mean, na.rm = TRUE),
#     hfp_sd = sd(HFP_mean, na.rm = TRUE),
#     hfp_max = max(HFP_mean, na.rm = TRUE),
#     hfp_min = min(HFP_mean, na.rm = TRUE),
#     strava_mean = mean(hx_pp_r, na.rm = TRUE),
#     strava_sd = sd(hx_pp_r, na.rm = TRUE),
#     strava_max = max(hx_pp_r, na.rm = TRUE),
#     strava_min = min(hx_pp_r, na.rm = TRUE)
#   )
#
# parks_highlight <- park_entropy %>%
#   filter(suid_nma %in% highlight_parks)
#
# p1 <- ggplot() +
#   geom_sf(data = park_entropy, aes(fill = mod_disp), color = NA, linewidth = 0.1) +
#   geom_sf(data = parks_highlight, color = "red", fill = NA, linewidth = 1) +
#   theme_bw() +
#   scale_fill_viridis_c() +
#   labs(fill = "Index of Dispersion for Modification")
#
# p2 <- ggplot() +
#   geom_sf(data = park_entropy, aes(fill = pred_mod), color = NA, linewidth = 0.1) +
#   geom_sf(data = parks_highlight, color = "red", fill = NA, linewidth = 1) +
#   theme_bw() +
#   scale_fill_viridis_c() +
#   labs(fill = "Predictibility of Modification")
#
# p3 <- ggplot() +
#   geom_sf(data = park_entropy, aes(fill = disp), color = NA, linewidth = 0.1) +
#   geom_sf(data = parks_highlight, color = "red", fill = NA, linewidth = 1) +
#   theme_bw() +
#   scale_fill_viridis_c() +
#   labs(fill = "Index of Dispersion for Strava")
#
# p4 <- ggplot() +
#   geom_sf(data = park_entropy, aes(fill = pred), color = NA, linewidth = 0.1) +
#   geom_sf(data = parks_highlight, color = "red", fill = NA, linewidth = 1) +
#   theme_bw() +
#   scale_fill_viridis_c() +
#   labs(fill = "Predictibility of Strava")
#
# plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")
#
# ggsave(plot, file = paste0("fig3.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)
#
# write_csv(park_entropy, here::here("data", "processed_data", "spatial_predictability.csv"))

# ## Time series for representative park
#
# temp_2019 <- temporal_data %>%
#   filter(year == 2019) %>%
#   group_by(suid_nma) %>%
#   mutate(count = n()) %>%
#   filter(count == 365)
#
# temp_2020 <- temporal_data %>%
#   filter(year == 2020) %>%
#   group_by(suid_nma) %>%
#   mutate(count = n()) %>%
#   filter(count == 366)
#
# decom_period <- function(suid, data_set) {
#   park <- data_set %>%
#     filter(suid_nma == suid) %>%
#     mutate(year = year(date))
#
#   park_ts <- ts(park$sg, freq = 30)
#
#   ### Decomposition
#
#   # decomp <- decompose(park_ts)
#   # plot(decomp)
#
#   ### Periodicity
#
#   freq <- TSA::periodogram(park_ts)[["freq"]]
#   specs <- TSA::periodogram(park_ts)[["spec"]]
#
#   # TSA::periodogram(park_ts, plot = TRUE) # compute and plot periodograms
#   # abline(h = 0)
#
#   df <- data.frame(freq = freq, specs = specs)
#
#   df$p_value <- GeneCycle::fisher.g.test(park_ts)
#
#   df$suid_nma <- suid
#
#   return(df)
# }
#
# period <- data.frame()
#
# unique_ids <- unique(temp_2019$suid_nma)
#
# for (i in 1:length(unique_ids)) {
#   int_data <- decom_period(unique_ids[i], temp_2019)
#
#   period <- rbind(period, int_data)
# }
#
# period_maxs_2019 <- period %>%
#   filter(p_value < 0.05) %>%
#   mutate(period = 1 / freq) %>%
#   filter(period < 365) %>%
#   mutate(mag = floor(log10(specs))) %>%
#   group_by(suid_nma) %>%
#   mutate(max_mag = max(mag)) %>%
#   ungroup() %>%
#   filter(mag == max_mag) %>%
#   select(-mag, -max_mag) %>%
#   mutate(year = 2019)
#
# top_periods1_2019 <- period_maxs_2019 %>%
#   group_by(period) %>%
#   mutate(n = n()) %>%
#   mutate(percent = n / length(unique_ids) * 100) %>%
#   group_by(suid_nma) %>%
#   slice_max(order_by = specs, n = 3) %>%
#   mutate(rank = rank(specs))
#
# period <- data.frame()
#
# unique_ids <- unique(temp_2020$suid_nma)
#
# for (i in 1:length(unique_ids)) {
#   int_data <- decom_period(unique_ids[i], temp_2020)
#
#   period <- rbind(period, int_data)
# }
#
# period_maxs_2020 <- period %>%
#   filter(p_value < 0.05) %>%
#   mutate(period = 1 / freq) %>%
#   filter(period < 366) %>%
#   mutate(mag = floor(log10(specs))) %>%
#   group_by(suid_nma) %>%
#   mutate(max_mag = max(mag)) %>%
#   ungroup() %>%
#   filter(mag == max_mag) %>%
#   select(-mag, -max_mag) %>%
#   mutate(year = 2020)
#
# top_periods1_2020 <- period_maxs_2020 %>%
#   group_by(period) %>%
#   mutate(n = n()) %>%
#   mutate(percent = n / length(unique_ids) * 100) %>%
#   group_by(suid_nma) %>%
#   slice_max(order_by = specs, n = 3) %>%
#   mutate(rank = rank(specs))
#
# period_max_all <- list(period_maxs_2019, period_maxs_2020) %>%
#   reduce(full_join)
#
# top_periods_all <- list(top_periods1_2019, top_periods1_2020) %>%
#   reduce(full_join)
#
# temp_highlight_2019 <- top_periods1_2019 %>%
#   filter(suid_nma %in% c(highlight_parks)) %>%
#   left_join(cpad_super) %>%
#   mutate(label = paste(park_name, "Top Peak")) %>%
#   filter(rank == 1)
#
# temp_highlight_2020 <- top_periods1_2020 %>%
#   filter(suid_nma %in% c(highlight_parks)) %>%
#   left_join(cpad_super) %>%
#   mutate(label = paste(park_name, "Top Peak")) %>%
#   filter(rank == 1)
#
# write_csv(period_max_all, here::here("analysis", "periodicity.csv"))
#
# write_csv(top_periods_all, here::here("analysis", "periodicity_max.csv"))
#
# colors <- c("1" = "black", "2" = "blue", "3" = "darkgreen")
#
# p2 <- ggplot() +
#   geom_vline(aes(xintercept = c(30, 90, 180, 365)), color = "red", alpha = 0.3, linetype = "dashed") +
#   geom_vline(data = temp_highlight_2019, aes(xintercept = period), color = "black", linetype = "dashed") +
#   geom_density(data = top_periods1_2019 %>% filter(rank == 1), aes(period, color = "1"), linewidth = 1) +
#   geom_density(data = top_periods1_2019 %>% filter(rank == 2), aes(period, color = "2"), linetype = "dashed", linewidth = 1) +
#   geom_density(data = top_periods1_2019 %>% filter(rank == 3), aes(period, color = "3"), linetype = "dotdash", linewidth = 1) +
#   theme_bw() +
#   # scale_x_log10() +
#   labs(
#     x = "Period (days)",
#     y = "Density",
#     color = "Peak Rank",
#     title = "2019"
#   ) +
#   scale_color_manual(values = colors) +
#   geom_label_repel(
#     data = temp_highlight_2019, aes(x = period, y = c(0.01, 0.005), label = label),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
# p3 <- ggplot() +
#   geom_vline(aes(xintercept = c(30, 90, 180, 365)), color = "red", alpha = 0.3, linetype = "dashed") +
#   geom_vline(data = temp_highlight_2020, aes(xintercept = period), color = "black", linetype = "dashed") +
#   geom_density(data = top_periods1_2020 %>% filter(rank == 1), aes(period, color = "1"), linewidth = 1) +
#   geom_density(data = top_periods1_2020 %>% filter(rank == 2), aes(period, color = "2"), linetype = "dashed", linewidth = 1) +
#   geom_density(data = top_periods1_2020 %>% filter(rank == 3), aes(period, color = "3"), linetype = "dotdash", linewidth = 1) +
#   theme_bw() +
#   # scale_x_log10() +
#   labs(
#     x = "Period (days)",
#     y = "Density",
#     color = "Peak Rank",
#     title = "2020"
#   ) +
#   scale_color_manual(values = colors) +
#   geom_label_repel(
#     data = temp_highlight_2020, aes(x = period, y = c(0.015, 0.01), label = label),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
# ggsave(p2 / p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect"), file = paste0("fig5.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)
#
# top_periods_rank_2019 <- top_periods_all %>%
#   filter(year == 2019) %>%
#   select(suid_nma, period, rank) %>%
#   pivot_wider(names_from = rank, values_from = period)
#
# names(top_periods_rank_2019) <- c("suid_nma", "rank1", "rank2", "rank3")
#
# top_highlight_2019 <- top_periods_rank_2019 %>%
#   filter(suid_nma %in% highlight_parks) %>%
#   left_join(human)
#
# top_periods_rank_2020 <- top_periods_all %>%
#   filter(year == 2020) %>%
#   select(suid_nma, period, rank) %>%
#   pivot_wider(names_from = rank, values_from = period)
#
# names(top_periods_rank_2020) <- c("suid_nma", "rank1", "rank2", "rank3")
#
# top_highlight_2020 <- top_periods_rank_2020 %>%
#   filter(suid_nma %in% highlight_parks) %>%
#   left_join(human)
#
# p <- ggplot() +
#   geom_point(data = top_periods_rank_2019, aes(rank1, rank2)) +
#   geom_point(data = top_highlight_2019, aes(rank1, rank2), color = "red") +
#   theme_bw() +
#   labs(
#     x = "Highest Peak",
#     y = "Next Highest Peak"
#   ) +
#   geom_label_repel(
#     data = top_highlight_2019, aes(rank1, rank2, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
# p2 <- ggplot() +
#   geom_point(data = top_periods_rank_2020, aes(rank1, rank2)) +
#   geom_point(data = top_highlight_2020, aes(rank1, rank2), color = "red") +
#   theme_bw() +
#   labs(
#     x = "Highest Peak",
#     y = "Next Highest Peak"
#   ) +
#   geom_label_repel(
#     data = top_highlight_2020, aes(rank1, rank2, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
# ggsave(p / p2, file = paste0("fig5.2.pdf"), path = here::here("analysis", "figs"), height = 15, width = 10)
#
# ### Predictability
#
# # Constancy: 1 – H(x)/log(n), where H(x) is information entropy or  1/sd
#
# const_temp <- 1 - entropy(temporal_data$sg) / log(nrow(temporal_data))
#
# # Consistency: [H(x) + H(y) – H(XY)]/log(n) where y is spatial position
#
# temporal_data <- temporal_data %>%
#   mutate(date_num = as.numeric(date))
#
# # need to fix
# contin_temp <- (entropy(temporal_data$sg) + entropy(temporal_data$date_num) - cross.entropy(temporal_data$sg, temporal_data$date_num)) / log(nrow(temporal_data))
#
# pred_temp <- const_temp + contin_temp
#
# park_entropy_temp <- temporal_data %>%
#   group_by(suid_nma, year) %>%
#   summarise(
#     entropy_temp = entropy(sg),
#     inv_sd_temp = 1 / sd(sg),
#     const_temp = 1 - entropy(sg) / log(n()),
#     contin_temp = (entropy(sg) + entropy(date_num) - cross.entropy(sg, date_num)) / log(n())
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(const_temp)) %>%
#   filter(!is.na(contin_temp)) %>%
#   mutate(
#     const_temp_scale = scale(const_temp),
#     contin_temp_scale = scale(contin_temp)
#   ) %>%
#   mutate(
#     const_temp_scale = const_temp_scale[, 1],
#     contin_temp_scale = contin_temp_scale[, 1]
#   ) %>%
#   mutate(
#     pred_temp = const_temp_scale + contin_temp_scale
#   )

# predictability <- full_join(park_entropy, park_entropy_temp)
#
# pred_highlight <- predictability %>%
#   filter(suid_nma %in% c(highlight_parks))
#
# write_csv(predictability %>% st_drop_geometry(), here::here("data", "processed_data", "predictability.csv"))
#
# p1 <- ggplot() +
#   geom_point(data = predictability %>% filter(year == 2019), aes(pred, pred_mod)) +
#   geom_point(data = pred_highlight %>% filter(year == 2019), aes(pred, pred_mod), color = "red", size = 2) +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   theme_bw() +
#   labs(
#     x = "Predictibility of Strava",
#     y = "Predictibility of Modification"
#   ) +
#   geom_label_repel(
#     data = pred_highlight %>% filter(year == 2019), aes(pred, pred_mod, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black", vjust = "outward", hjust = "outward"
#   )
#
# p2 <- ggplot() +
#   geom_point(data = predictability %>% filter(year == 2019), aes(pred, pred_temp)) +
#   geom_point(data = pred_highlight %>% filter(year == 2019), aes(pred, pred_temp), color = "red", size = 2) +
#   theme_bw() +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   labs(
#     x = "Predictibility of Strava",
#     y = "Predictibility of Safegraph"
#   ) +
#   geom_label_repel(
#     data = pred_highlight %>% filter(year == 2019), aes(pred, pred_temp, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black", vjust = "outward", hjust = "outward"
#   )
#
# p3 <- ggplot() +
#   geom_point(data = predictability %>% filter(year == 2020), aes(pred, pred_temp)) +
#   geom_point(data = pred_highlight %>% filter(year == 2020), aes(pred, pred_temp), color = "red", size = 2) +
#   theme_bw() +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   labs(
#     x = "Predictibility of Strava",
#     y = "Predictibility of Safegraph"
#   ) +
#   geom_label_repel(
#     data = pred_highlight %>% filter(year == 2020), aes(pred, pred_temp, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black", vjust = "outward", hjust = "outward"
#   )
#
# p4 <- ggplot() +
#   geom_point(data = predictability %>% filter(year == 2019), aes(disp, mod_disp)) +
#   geom_point(data = pred_highlight %>% filter(year == 2019), aes(disp, mod_disp), color = "red", size = 2) +
#   theme_bw() +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   labs(
#     x = "Index of Dispersion for Strava",
#     y = "Index of Dispersion for Modification"
#   ) +
#   geom_label_repel(
#     data = pred_highlight %>% filter(year == 2019), aes(disp, mod_disp, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
# plot <- (p1 + p4) / (p2 + p3) + plot_annotation(tag_levels = "A")
#
# ggsave(plot, file = paste0("fig4.pdf"), path = here::here("analysis", "figs"), height = 6, width = 15)
#
# p <- ggplot(predictability %>% filter(year %in% c(2019, 2020))) +
#   geom_point(aes(as.factor(year), pred_temp)) +
#   geom_line(aes(as.factor(year), pred_temp, group = suid_nma)) +
#   geom_point(data = pred_highlight %>% filter(year %in% c(2019, 2020)), aes(as.factor(year), pred_temp), color = "red") +
#   geom_line(data = pred_highlight %>% filter(year %in% c(2019, 2020)), aes(as.factor(year), pred_temp, group = suid_nma), color = "red") +
#   theme_bw() +
#   labs(
#     x = "Year",
#     y = "Temporal Predictability"
#   ) +
#   geom_label_repel(
#     data = pred_highlight %>% filter(year == 2019), aes(as.factor(year), pred_temp, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
# ggsave(p, file = paste0("temp_change.pdf"), path = here::here("analysis", "figs"), height = 8, width = 6)

# temporal_bins2 <- temporal_data %>%
#   mutate(
#     max = max(sg, na.rm = TRUE),
#     min = min(sg, na.rm = TRUE),
#     quartile_1 = quantile(sg, probs = 0.25),
#     quartile_2 = quantile(sg, probs = 0.5),
#     quartile_3 = quantile(sg, probs = 0.75)
#   ) %>%
#   ungroup() %>%
#   mutate(bin = case_when(
#     sg < quartile_1 ~ 1,
#     sg > quartile_3 ~ 4,
#     sg >= quartile_1 & sg < quartile_2 ~ 2,
#     sg >= quartile_2 & sg <= quartile_3 ~ 3
#   ))
#
# temp_date2 <- temporal_bins2 %>%
#   group_by(date_j, bin) %>%
#   mutate(date_count = n())
#
# p2 <- ggplot(temp_date2) +
#   geom_tile(aes(date_j, bin, color = date_count, fill = date_count)) +
#   geom_vline(aes(xintercept = 21), color = "red") +
#   geom_vline(aes(xintercept = 49), color = "red") +
#   geom_vline(aes(xintercept = 111), color = "red") +
#   geom_vline(aes(xintercept = 147), color = "red") +
#   geom_vline(aes(xintercept = 185), color = "red") +
#   geom_vline(aes(xintercept = 245), color = "red") +
#   geom_vline(aes(xintercept = 332), color = "red") +
#   geom_vline(aes(xintercept = 359), color = "red") +
#   geom_vline(aes(xintercept = 365), color = "red") +
#   geom_vline(aes(xintercept = 385), color = "red") +
#   geom_vline(aes(xintercept = 413), color = "red") +
#   geom_vline(aes(xintercept = 468), color = "red") +
#   geom_vline(aes(xintercept = 511), color = "red") +
#   geom_vline(aes(xintercept = 551), color = "red") +
#   geom_vline(aes(xintercept = 616), color = "red") +
#   geom_vline(aes(xintercept = 696), color = "red") +
#   geom_vline(aes(xintercept = 725), color = "red") +
#   geom_vline(aes(xintercept = 731), color = "red") +
#   geom_vline(aes(xintercept = 749), color = "red") +
#   geom_vline(aes(xintercept = 777), color = "red") +
#   geom_vline(aes(xintercept = 825), color = "red") +
#   geom_vline(aes(xintercept = 440), color = "blue") + #covid lockdown
#   geom_vline(aes(xintercept = 480), color = "blue") + #earliest reopen
#   geom_line(data = highlight_temp, aes(date_j, sg, group = as.factor(park_name))) +
#   scale_color_viridis_c()+
#   scale_fill_viridis_c() +
#   theme_bw() +
#   labs(x = "Julian Date",
#        y = "Safegraph",
#        color = "Count",
#        fill = "Count") +
#   scale_x_reverse()
#
# ggsave(p2, file = paste0("fig3.2.pdf"), path = here::here("analysis", "figs"), height = 8, width = 15)

# hfp_pred <- predictions(gam_sub1, variables = "HFP2009", by = "HFP2009")
# xy_pred <- predictions(gam1, variables = "x", by = c("x", "y"))

# write_csv(hfp_pred, here::here("data", "processed_data", "hfp_pred.csv"))
# write_csv(xy_pred, here::here("data", "processed_data", "xy_pred.csv"))

# p1 <- plot_predictions(gam_sub, condition = "HFP2009") +
#   geom_rug(data = park_data, aes(gHM_mean), alpha = 1 / 2, outside = TRUE, sides = "b") +
#   scale_y_log10() +
#   theme_bw() +
#   labs(
#     x = "Global Human Modification",
#     y = "Strava Value"
#   ) +
#   coord_cartesian(clip = "off") +
#   theme(
#     axis.text.x = element_text(margin = margin(t = 10)),
#     axis.ticks.x = element_blank()
#   ) +
#   geom_point(data = yosemite, aes(gHM_mean, hx_pp_r)) +
#   geom_label_repel(
#     data = yosemite, aes(gHM_mean, hx_pp_r, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
# p2 <- plot_predictions(gam1, condition = "HFP2009") +
#   geom_rug(data = park_data, aes(HFP_median), alpha = 1 / 2, outside = TRUE, sides = "b") +
#   scale_y_log10() +
#   theme_bw() +
#   labs(
#     x = "Human Footprint Index",
#     y = "Strava Value"
#   ) +
#   coord_cartesian(clip = "off") +
#   theme(
#     axis.text.x = element_text(margin = margin(t = 10)),
#     axis.ticks.x = element_blank()
#   ) +
#   geom_point(data = yosemite, aes(HFP_median, hx_pp_r)) +
#   geom_label_repel(
#     data = yosemite, aes(HFP_median, hx_pp_r, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
#
#
# p3 <- plot_predictions(gam1, condition = c("x", "y")) +
#   scale_y_log10() +
#   theme_bw() +
#   labs(
#     x = "Longitude",
#     y = "Strava Value",
#     color = "Latitude",
#     fill = "Latitude"
#   ) +
#   scale_color_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE), labels = rev(c("42.00", 39.83, 36.81, 34.91, 32.56))) +
#   scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE), labels = rev(c("42.00", 39.83, 36.81, 34.91, 32.56))) +
#   coord_cartesian(clip = "off") +
#   geom_point(data = yosemite_spatial, aes(x, hx_pp_r), color = "#addc30") +
#   geom_label_repel(
#     data = yosemite_spatial, aes(x, hx_pp_r, label = park_name),
#     size = 4,
#     alpha = 0.65,
#     min.segment.length = 1,
#     arrow = arrow(length = unit(0.1, "inches")),
#     box.padding = 1,
#     color = "black"
#   )
