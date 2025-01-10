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

sf_use_s2(FALSE)

# Data Input --------------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) 

ca = st_read(here::here("data", "raw_data", "CA_State_TIGER2016.shp")) %>%
  sf::st_transform("EPSG:4326") %>% 
  mutate(area = st_area(geometry)) %>% 
  select(area) %>% 
  st_drop_geometry()

sg <- read_csv(here::here("data", "processed_data", "cpad_sg.csv"))
strava <- st_read(here::here("data", "processed_data", "cpad_strava.shp"))
ghm <- st_read(here::here("data", "processed_data", "cpad_ghm.shp"))
hfp <- st_read(here::here("data", "processed_data", "cpad_hfp.shp"))
bio <- st_read(here::here("data", "processed_data", "cpad_bio.shp"))

human <- list(sg, strava, ghm, hfp, bio) %>%
  reduce(full_join) %>%
  st_as_sf()

rm(sg, strava, ghm, hfp, bio)

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
    HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv, year, bio_men, bio_mdn,rare_men, rar_mdn
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
  )) %>%
  mutate(bio_quartile = case_when(
    bio_mdn < quantile(bio_mdn, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
    bio_mdn >= quantile(bio_mdn, probs = 0.25, na.rm = TRUE) & bio_mdn < quantile(bio_mdn, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
    bio_mdn <= quantile(bio_mdn, probs = 0.75, na.rm = TRUE) & bio_mdn >= quantile(bio_mdn, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
    bio_mdn > quantile(bio_mdn, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  ))%>%
  mutate(sg_quartile = case_when(
    mean_sg < quantile(mean_sg, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
    mean_sg >= quantile(mean_sg, probs = 0.25, na.rm = TRUE) & mean_sg < quantile(mean_sg, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
    mean_sg <= quantile(mean_sg, probs = 0.75, na.rm = TRUE) & mean_sg >= quantile(mean_sg, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
    mean_sg > quantile(mean_sg, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  ))

cpad_no_geom = cpad_super %>%
  st_drop_geometry()

highlight_parks <- c(14310, 22386, 33731, 30603)

human_highlight <- human %>%
  filter(suid_nma %in% highlight_parks)

human_sum <- human %>%
  st_drop_geometry() %>%
  select(-year, -sg) %>%
  distinct()

human_highlight_sum <- human_sum %>%
  filter(suid_nma %in% highlight_parks)

NPS = human_sum %>% 
  filter(mng_agncy == "United States National Park Service")
  
TNC = human_sum %>% 
  filter(mng_agncy == "The Nature Conservancy")
  
UC = human_sum %>% 
  filter(mng_agncy == "University of California")
  
CDFW = human_sum %>% 
  filter(mng_agncy == "California Department of Fish and Wildlife")

p1 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(HFP_median, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = NPS, aes(HFP_median, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Tourism",
    color = "Management",
    title = "A"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))


p2 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(bio_men, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = NPS, aes(bio_men, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Mean Terrestrial Species Count",
    y = "Tourism",
    color = "Management",
    title = "B"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p3 <- ggplot(human_sum) +
  geom_point(aes(HFP_median, bio_men), alpha = 0.7, color = "grey") +
  geom_point(data = NPS, aes(HFP_median, bio_men), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Mean Terrestrial Species Count",
    color = "Management",
    title = "C"
  ) +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p = p1 / p2 / p3

ggsave(p, file = paste0("fig2_NPS.pdf"), path = here::here("analysis", "figs"), height = 8, width = 10)

p1 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(HFP_median, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = TNC, aes(HFP_median, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Tourism",
    color = "Management",
    title = "A"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))


p2 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(bio_men, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = TNC, aes(bio_men, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Mean Terrestrial Species Count",
    y = "Tourism",
    color = "Management",
    title = "B"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p3 <- ggplot(human_sum) +
  geom_point(aes(HFP_median, bio_men), alpha = 0.7, color = "grey") +
  geom_point(data = TNC, aes(HFP_median, bio_men), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Mean Terrestrial Species Count",
    color = "Management",
    title = "C"
  ) +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p = p1 / p2 / p3

ggsave(p, file = paste0("fig2_TNC.pdf"), path = here::here("analysis", "figs"), height = 8, width = 10)

p1 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(HFP_median, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = UC, aes(HFP_median, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Tourism",
    color = "Management",
    title = "A"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))


p2 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(bio_men, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = UC, aes(bio_men, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Mean Terrestrial Species Count",
    y = "Tourism",
    color = "Management",
    title = "B"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p3 <- ggplot(human_sum) +
  geom_point(aes(HFP_median, bio_men), alpha = 0.7, color = "grey") +
  geom_point(data = UC, aes(HFP_median, bio_men), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Mean Terrestrial Species Count",
    color = "Management",
    title = "C"
  ) +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p = p1 / p2 / p3

ggsave(p, file = paste0("fig2_UC.pdf"), path = here::here("analysis", "figs"), height = 8, width = 10)

p1 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(HFP_median, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = CDFW, aes(HFP_median, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Tourism",
    color = "Management",
    title = "A"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))


p2 <- ggplot(human_sum) +
  # geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5) +
  geom_point(aes(bio_men, mean_sg), alpha = 0.7, color = "grey") +
  geom_point(data = CDFW, aes(bio_men, mean_sg), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Mean Terrestrial Species Count",
    y = "Tourism",
    color = "Management",
    title = "B"
  ) +
  scale_y_log10() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p3 <- ggplot(human_sum) +
  geom_point(aes(HFP_median, bio_men), alpha = 0.7, color = "grey") +
  geom_point(data = CDFW, aes(HFP_median, bio_men), color = "red", size = 2) +
  theme_bw() +
  labs(
    x = "Median Human Footprint Index",
    y = "Mean Terrestrial Species Count",
    color = "Management",
    title = "C"
  ) +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "bottom",
        plot.title = element_text(size= 20, vjust = -0.5))

p = p1 / p2 / p3

ggsave(p, file = paste0("fig2_CDFW.pdf"), path = here::here("analysis", "figs"), height = 8, width = 10)
