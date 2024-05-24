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
  st_as_sf() 

rm(sg, strava, ghm, hfp)

#take out hoas

human = human %>%
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
  )

full_famd_data <- human %>%
  st_drop_geometry() %>%
  select(
    sg, hx_pp_r,
    gHM_mean, HFP_mean, suid_nma,
    access_typ, gap1_acres, sg_range, hex_range,
    gap2_acres, gap3_acres, gap4_acres, mng_ag_typ
  ) %>%
  drop_na() %>% 
  filter(!is.infinite(sg))

famd_data <- full_famd_data %>%
  select(-suid_nma)

full_pca_data <- human %>%
  st_drop_geometry() %>%
  select(
    sg, hx_pp_r,
    gHM_mean, HFP_mean, suid_nma,
    access_typ, sg_range, hex_range,
  ) %>%
  drop_na()%>% 
  filter(!is.infinite(sg))

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

filled_scores5 <- filled_scores5 %>%
  mutate(id = as.factor(suid_nma), access = as.factor(access_typ)) %>%
  mutate(class = case_when(
    NMDS1 <= 0 & NMDS2 <= 0 ~ "high mod / med mobility",
    NMDS1 > 0 & NMDS2 > 0 ~ "low mod / high safegraph",
    NMDS1 <= 0 & NMDS2 > 0 ~ "high mod / med mobility",
    NMDS1 > 0 & NMDS2 <= 0 ~ "low mod / high strava"
  ))

species_scores5 <- as.data.frame(scores(nmds_h)$species)

rownames(species_scores5) <- c(
  "Safegraph", "Strava Value",
  "Global Human Modification", "Human Footprint Index",
  "Safegraph Range", "Strava Range"
)

species_scores5 <- species_scores5 %>%
  rownames_to_column(var = "var")

centroid5 <- filled_scores5 %>%
  group_by(class) %>%
  summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

nmds5 <- ggplot(filled_scores5, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  stat_ellipse(geom = "polygon", aes(color = class, fill = class), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color = class), alpha = 0.3) +
  theme_bw(base_size = 16) +
  geom_point(data = centroid5, aes(color = class), size = 4, alpha = 0.9) +
  labs(x = "nMDS1", y = "nMDS2", color = "Human Use") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  geom_segment(data = species_scores5, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = species_scores5, aes(x = NMDS1, y = NMDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25, max.overlaps = 15) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# annotate("text", x = 0.17, y = 0.25, label = "Stress: 0.155")

nmds5

ggsave(nmds5, file = paste0("human_PCA.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

nmds_cpad <- human %>%
  full_join(filled_scores5)

nmds <- nmds_cpad %>%
  st_drop_geometry()

write_csv(nmds, here::here("analysis", "nmds.csv"))

count = nmds_cpad %>%
  st_drop_geometry() %>%
  group_by(class) %>%
  summarize(n = n())

# FAMD --------------------------------------------------------------------

famd <- FAMD(famd_data, graph = FALSE)

famd_coords <- as.data.frame(famd$ind[["coord"]]) %>%
  select(Dim.1, Dim.2)

famd_coords$class <- filled_scores5$class

famd_centroid <- famd_coords %>%
  group_by(class) %>%
  summarize(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2))

eigen <- famd$eig
# eigen

famd_var <- as.data.frame(famd$var)
famd_var <- famd_var %>%
  mutate(var = c(
    "Safegraph", "Strava Value",
    "Global Human Modification", "Human Footprint Index",
    "Access Type", "GAP 1",
    "Safegraph Range", "Strava Range",
    "GAP 2", "GAP 3", "GAP 4", "Management Type"
  ))

famd_fig <- ggplot(data = famd_coords) +
  geom_point(aes(Dim.1, Dim.2, color = class)) +
  stat_ellipse(aes(Dim.1, Dim.2, color = class, fill = class), geom = "polygon", linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  theme_bw() +
  labs(
    x = "FAMD1 (7.5%)", y = "FAMD2 (4.1%)", color = "Human Use",
    fill = "Human Use", title = NULL
  ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left") +
  geom_point(data = famd_centroid, aes(Dim.1, Dim.2, color = class), size = 4, alpha = 0.9) +
  geom_segment(
    data = famd_var, aes(x = 0, xend = coord.Dim.1 * 7, y = 0, yend = coord.Dim.2 * 7),
    arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd = 0.5
  ) +
  ggrepel::geom_label_repel(
    data = famd_var, aes(x = coord.Dim.1 * 7, y = coord.Dim.2 * 7, label = var),
    cex = 3, direction = "both", segment.size = 0.25
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  xlim(-5, 8) +
  ylim(-5, 8) +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

famd_fig

ggsave(famd_fig, file = paste0("FAMD.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
