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
  mutate(sg_2019_range = max_sg_2019 - min_sg_2019,
         sg_2020_range = max_sg_2020 - min_sg_2020,
         hex_range = mx_hx__ - mn_hx__)

# PCA ---------------------------------------------------------------------

full_pca_data <- full_cpad %>%
  st_drop_geometry() %>%
  select(
    max_sg_2019, min_sg_2019, sg_2019, max_sg_2020, min_sg_2020, sg_2020, hx_pp_r, mx_hx__, mn_hx__,
    gHM_min, gHM_mean, gHM_max, gHM_median, gHM_sd, gHM_range, gHM_cv, HFP_min,
    HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv, suid_nma,
    mx_tp_m, mx_tp_s, mx_tp_c, mn_tp_m, mn_tp_s, mn_tp_c, prec_min, prec_men, prec_mx,
    prc_mdn, prec_sd, prc_rng, prec_cv, NDVI_min, NDVI_men, NDVI_mx, NDVI_md, NDVI_sd,
    NDVI_rn, NDVI_cv, cc_min, cc_mean, cc_max, cc_median, cc_sd, cc_range, cc_cv, access_typ
  ) %>%
  drop_na()

pca_data <- full_pca_data %>%
  select(-suid_nma, -access_typ)

m_com <- as.matrix(pca_data)

set.seed(42)
nmds <- metaMDS(m_com, distance = "bray")
nmds

# extract NMDS scores (x and y coordinates)
filled_scores4 <- as.data.frame(scores(nmds))

# add columns to data frame
filled_scores4$id <- full_pca_data$suid_nma
filled_scores4$access <- full_pca_data$access_typ

filled_scores4 <- filled_scores4 %>%
  mutate(id = as.factor(id), access = as.factor(access))

species_scores4 <- as.data.frame(nmds[["species"]])

rownames(species_scores4) <- c()

species_scores4 <- species_scores4 %>%
  rownames_to_column(var = "var")

centroid4 <- filled_scores4 %>%
  group_by(access) %>%
  summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

nmds4 <- ggplot(filled_scores4, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  # stat_ellipse(geom = "polygon", aes(color = access, fill = access), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color = access), alpha = 0.3) +
  theme_bw(base_size = 16) +
  geom_point(data = centroid4, aes(color = access), size = 4, alpha = 0.9) +
  labs(x = "nMDS1", y = "nMDS2", color = "Access Level") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  # geom_segment(data = species_scores4, aes(x = 0, xend = MDS1, y = 0, yend = MDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  # ggrepel::geom_label_repel(data = species_scores4, aes(x = MDS1, y = MDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# annotate("text", x = 0.17, y = 0.25, label = "Stress: 0.155")

nmds4

# Clustering --------------------------------------------------------------

# clusters <- hclust(dist(iris[, 3:4]))
# plot(clusters)

# Human PCA ---------------------------------------------------------------

full_pca_data <- full_cpad %>%
  st_drop_geometry() %>%
  select(
    sg_2019, sg_2020, hx_pp_r,
    gHM_mean, gHM_cv,
    HFP_mean, HFP_cv, suid_nma,
    access_typ, sg_2019_range, 
    sg_2020_range, hex_range
  ) %>%
  drop_na()

pca_data <- full_pca_data %>%
  select(-suid_nma, -access_typ)

m_com <- as.matrix(pca_data)

set.seed(42)
nmds_h <- metaMDS(m_com, distance = "bray")
nmds_h

# extract NMDS scores (x and y coordinates)
filled_scores5 <- as.data.frame(scores(nmds_h)$sites)

# add columns to data frame
filled_scores5$id <- full_pca_data$suid_nma
filled_scores5$access <- full_pca_data$access_typ

filled_scores5 <- filled_scores5 %>%
  mutate(id = as.factor(id), access = as.factor(access))

species_scores5 <- as.data.frame(scores(nmds_h)$species)

rownames(species_scores5) <- c("Safegraph 2019", "Safegraph 2020", "Strava Value",
                               "Global Human Modification", "GHM CV", "Human Footprint Index", "HFI CV")

species_scores5 <- species_scores5 %>%
  rownames_to_column(var = "var")

centroid5 <- filled_scores5 %>%
  group_by(access) %>%
  summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

nmds5 <- ggplot(filled_scores5, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  stat_ellipse(geom = "polygon", aes(color = access, fill = access), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color = access), alpha = 0.3) +
  theme_bw(base_size = 16) +
  geom_point(data = centroid5, aes(color = access), size = 4, alpha = 0.9) +
  labs(x = "nMDS1", y = "nMDS2", color = "Access Level") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  geom_segment(data = species_scores5, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = species_scores5, aes(x = NMDS1, y = NMDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25, max.overlaps = 15) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  # annotate("text", x = 0.17, y = 0.25, label = "Stress: 0.155")

nmds5

ggsave(nmds5, file = paste0("human_PCA.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# Environment PCA ---------------------------------------------------------

full_pca_data <- full_cpad %>%
  st_drop_geometry() %>%
  select(
    suid_nma,
    mx_tp_m, mx_tp_c, mn_tp_m, mn_tp_c, prec_men,
    prec_cv, NDVI_men, NDVI_cv, cc_mean, cc_cv, access_typ, min_mean_temp,
    min_cv_temp
  ) %>%
  drop_na()  %>% 
  mutate(mn_tp_c = case_when(
    mn_tp_c < 0 ~ -(mn_tp_c),
    TRUE ~ mn_tp_c
  )) %>% 
  mutate(mn_tp_m = mn_tp_m + 14) %>% 
  mutate(NDVI_cv = case_when(
    NDVI_cv < 0 ~ -(NDVI_cv),
    TRUE ~ NDVI_cv
  )) %>% 
  mutate(NDVI_men = NDVI_men + 1)

pca_data <- full_pca_data %>%
  select(-suid_nma, -access_typ)

m_com <- as.matrix(pca_data)

set.seed(42)
nmds_e <- metaMDS(m_com, distance = "bray")
nmds_e

# extract NMDS scores (x and y coordinates)
filled_scores6 <- as.data.frame(scores(nmds_e)$sites)

# add columns to data frame
filled_scores6$id <- full_pca_data$suid_nma
filled_scores6$access <- full_pca_data$access_typ

filled_scores6 <- filled_scores6 %>%
  mutate(id = as.factor(id), access = as.factor(access))

species_scores6 <- as.data.frame(scores(nmds_e)$species)

# rownames(species_scores4) <- c()

species_scores6 <- species_scores6 %>%
  rownames_to_column(var = "var")

centroid6 <- filled_scores6 %>%
  group_by(access) %>%
  summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

nmds6 <- ggplot(filled_scores6, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  stat_ellipse(geom = "polygon", aes(color = access, fill = access), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color = access), alpha = 0.3) +
  theme_bw(base_size = 16) +
  geom_point(data = centroid6, aes(color = access), size = 4, alpha = 0.9) +
  labs(x = "nMDS1", y = "nMDS2", color = "Access Level") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  geom_segment(data = species_scores6, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = species_scores6, aes(x = NMDS1, y = NMDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25, max.overlaps = 15) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# annotate("text", x = 0.17, y = 0.25, label = "Stress: 0.155")

nmds6

# PCA subset --------------------------------------------------------------

full_pca_data <- full_cpad %>%
  st_drop_geometry() %>%
  select(
    sg_2019, sg_2020, hx_pp_r,
    gHM_mean, gHM_cv,
    HFP_mean, HFP_cv, suid_nma,
    mx_tp_m, mx_tp_c, mn_tp_m, mn_tp_c, prec_men,
    prec_cv, NDVI_men, NDVI_cv, cc_mean, cc_cv, access_typ,
    gap1_acres, gap2_acres, gap3_acres, gap4_acres
  ) %>%
  drop_na()  %>% 
  mutate(mn_tp_c = case_when(
    mn_tp_c < 0 ~ -(mn_tp_c),
    TRUE ~ mn_tp_c
  )) %>% 
  mutate(mn_tp_m = mn_tp_m + 14) %>% 
  mutate(NDVI_cv = case_when(
    NDVI_cv < 0 ~ -(NDVI_cv),
    TRUE ~ NDVI_cv
  )) %>% 
  mutate(NDVI_men = NDVI_men + 1)

pca_data <- full_pca_data %>%
  select(-suid_nma, -access_typ)

m_com <- as.matrix(pca_data)

set.seed(42)
nmds <- metaMDS(m_com, distance = "bray")
nmds

# extract NMDS scores (x and y coordinates)
filled_scores4 <- as.data.frame(scores(nmds)$sites)

# add columns to data frame
filled_scores4$id <- full_pca_data$suid_nma
filled_scores4$access <- full_pca_data$access_typ

filled_scores4 <- filled_scores4 %>%
  mutate(id = as.factor(id), access = as.factor(access))

species_scores4 <- as.data.frame(nmds[["species"]])

rownames(species_scores4) <- c()

species_scores4 <- species_scores4 %>%
  rownames_to_column(var = "var")

centroid4 <- filled_scores4 %>%
  group_by(access) %>%
  summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

nmds4 <- ggplot(filled_scores4, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  # stat_ellipse(geom = "polygon", aes(color = access, fill = access), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color = access), alpha = 0.3) +
  theme_bw(base_size = 16) +
  geom_point(data = centroid4, aes(color = access), size = 4, alpha = 0.9) +
  labs(x = "nMDS1", y = "nMDS2", color = "Access Level") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  # geom_segment(data = species_scores4, aes(x = 0, xend = MDS1, y = 0, yend = MDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  # ggrepel::geom_label_repel(data = species_scores4, aes(x = MDS1, y = MDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# annotate("text", x = 0.17, y = 0.25, label = "Stress: 0.155")

nmds4
