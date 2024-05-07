library(tidyverse)
library(sf)
library(vegan)
library(FactoMineR)
library(factoextra)

set.seed(42)

# Load Data ---------------------------------------------------------------

sg <- read_csv(here::here("data", "processed_data", "cpad_sg.csv"))
strava <- st_read(here::here("data", "processed_data", "cpad_strava.shp"))
ghm <- st_read(here::here("data", "processed_data", "cpad_ghm.shp"))
hfp <- st_read(here::here("data", "processed_data", "cpad_hfp.shp"))

human <- list(sg, strava, ghm, hfp) %>%
  reduce(full_join) %>%
  st_as_sf() %>%
  filter(shape_area >= 10000) %>% 
#check to cut things like cemetaries
  select(
    max_sg, min_sg, sg, hx_pp_r, mx_hx__, mn_hx__, suid_nma,
    access_typ, park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    gHM_min, gHM_mean, gHM_max, gHM_median, gHM_sd, gHM_range, gHM_cv, HFP_min,
    HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv
  ) %>% 
  mutate(sg_range = max_sg - min_sg,
         hex_range = mx_hx__ - mn_hx__)

rm(sg, strava, ghm, hfp)

full_famd_data <- human %>%
  st_drop_geometry() %>%
  select(
    sg, hx_pp_r,
    gHM_mean, HFP_mean, suid_nma,
    access_typ, gap1_acres, 
    gap2_acres, gap3_acres, gap4_acres, mng_ag_lev
  ) %>%
  drop_na()

famd_data = full_famd_data %>% 
  select(-suid_nma)

full_pca_data <- human %>%
  st_drop_geometry() %>%
  select(
    sg, hx_pp_r,
    gHM_mean, HFP_mean, suid_nma,
    access_typ
  ) %>%
  drop_na()

pca_data <- full_pca_data %>%
  select(-suid_nma, -access_typ)

# nMDS ---------------------------------------------------------------------

m_com <- as.matrix(pca_data)
nmds_h <- metaMDS(m_com, distance = "bray")
nmds_h

# extract NMDS scores (x and y coordinates)
filled_scores5 <- as.data.frame(scores(nmds_h)$sites)

# add columns to data frame
filled_scores5$suid_nma <- full_pca_data$suid_nma
filled_scores5$access_typ <- full_pca_data$access_typ
filled_scores5$kmeans_class <- as.factor(kmeans$cluster)

filled_scores5 <- filled_scores5 %>%
  mutate(id = as.factor(suid_nma), access = as.factor(access_typ))

species_scores5 <- as.data.frame(scores(nmds_h)$species)

rownames(species_scores5) <- c("Safegraph", "Strava Value",
                               "Global Human Modification", "GHM CV", "Human Footprint Index", "HFI CV",
                               "SG 2019 Range", "SG 2020 Range", "Strava Range")

species_scores5 <- species_scores5 %>%
  rownames_to_column(var = "var")

centroid5 <- filled_scores5 %>%
  group_by(kmeans_class) %>%
  summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

nmds5 <- ggplot(filled_scores5, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  stat_ellipse(geom = "polygon", aes(color = kmeans_class, fill = kmeans_class), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color = kmeans_class), alpha = 0.3) +
  theme_bw(base_size = 16) +
  geom_point(data = centroid5, aes(color = kmeans_class), size = 4, alpha = 0.9) +
  labs(x = "nMDS1", y = "nMDS2", color = "Access Level") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  geom_segment(data = species_scores5, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = species_scores5, aes(x = NMDS1, y = NMDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25, max.overlaps = 15) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlim(c(-1.7, NA)) +
  ylim(c(-1.2, NA)) 
  # annotate("text", x = 0.17, y = 0.25, label = "Stress: 0.155")

nmds5

# ggsave(nmds5, file = paste0("human_PCA.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

nmds_cpad = full_cpad %>% 
  full_join(filled_scores5) %>% 
  select(NMDS1, NMDS2, suid_nma, access_typ, park_name, mng_ag_typ, mng_agncy) %>% 
  filter(!is.na(NMDS1)) %>% 
  mutate(class = case_when(
    NMDS1 <= 0 & NMDS2 <= 0 ~ "low mod / med mobility",
    NMDS1 > 0 & NMDS2 > 0 ~ "high mod / med mobility",
    NMDS1 <= 0 & NMDS2 > 0 ~ "low mod / high mobility",
    NMDS1 > 0 & NMDS2 <= 0 ~ "high mod / low mobility"
  ))

nmds_cpad$kmeans_class = kmeans$cluster

nmds = nmds_cpad %>% 
  st_drop_geometry()

write_csv(nmds, here::here("analysis", "nmds.csv"))

# count = nmds_cpad %>% 
#   st_drop_geometry() %>% 
#   group_by(class) %>% 
#   summarize(n = n())

# FAMD --------------------------------------------------------------------

famd <- FAMD(famd_data, graph = FALSE)

eigen <- famd$eig
eigen

famd_var = as.data.frame(famd$var) %>% 
  mutate(var = rownames(famd_var))

famd_fig <- fviz_famd_ind(famd, label = "none", habillage = 6, palette = hcl.colors(4, "viridis"), 
                          addEllipses = TRUE, ellipse.level = 0.95, invisible = "quali.var", alpha.ind = 0.4, 
                          ellipse.alpha = 0.2) +
  theme_bw() +
  labs(x = "FAMD1 (10.5%)", y = "FAMD2 (9%)", color = "Access Type", 
       fill = "Access Type", title = NULL) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left") + 
  geom_segment(data = famd_var, aes(x = 0, xend = coord.Dim.1*7, y = 0, yend = coord.Dim.2*7), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd = 0.5) +
  ggrepel::geom_label_repel(data = famd_var, aes(x = coord.Dim.1*7, y = coord.Dim.2*7, label = var), 
                            cex = 3, direction = "both", segment.size = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylim(c(-5, 10))

famd_fig

# ggsave(famd_fig, file = paste0("FAMD.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)