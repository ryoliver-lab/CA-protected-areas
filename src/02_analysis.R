library(tidyverse)
library(sf)
library(patchwork)
library(lme4)
library(lmerTest)
library(effectsize)
library(rsvg)
library(png)
library(grid)
library(mgcv)
library(sjstats)

sf_use_s2(FALSE)

library(nnet)
library(gtsummary)
library(broom)
# To plot predicted values
library(marginaleffects)
library(ggrepel)
library(ggridges)
library(ggExtra)
library(gghalves)
library(ggtext)
library(ggimage)
library(geosphere)
library(ggfortify)
library(ggpmisc)
library(philentropy)
library(spdep)
library(base64enc)
library(colorspace)

# Data Input --------------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

human <- st_read(here::here("data", "processed_data", "human.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super)) %>%
  # take out hoas and other protected areas
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
    suid_nma, access_typ, park_name, mng_ag_id, mng_agncy,
    gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    HFP_median, ntv_med, rare_med, gray_scale
  ) %>%
  rowwise() %>%
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4"
  )) %>%
  ungroup()

human_sum <- human %>%
  st_drop_geometry() %>%
  distinct()

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  filter(suid_nma %in% human$suid_nma) %>%
  full_join(human %>% st_drop_geometry() %>% select(suid_nma, gap_status, mng_agncy, shape_area)) %>%
  mutate(
    native = ntv_plnt + ntv_anm,
    rare = rar_plnt + rare_anm
  )

NPS <- spatial_data %>%
  filter(mng_agncy == "United States National Park Service")

TNC <- spatial_data %>%
  filter(mng_agncy == "The Nature Conservancy")

UC <- spatial_data %>%
  filter(mng_agncy == "United States Forest Service")

CDFW <- spatial_data %>%
  filter(mng_agncy == "California Department of Fish and Wildlife")

CDPR <- spatial_data %>%
  filter(mng_agncy == "California Department of Parks and Recreation")

gap1 <- spatial_data %>%
  filter(gap_status == "GAP 1")

gap2 <- spatial_data %>%
  filter(gap_status == "GAP 2")

gap3 <- spatial_data %>%
  filter(gap_status == "GAP 3")

gap4 <- spatial_data %>%
  filter(gap_status == "GAP 4")

colors <- c(
  "United States National Park Service" = "#031D3A",
  "The Nature Conservancy" = "#00ABF5",
  "United States Forest Service" = "#047658",
  "California Department of Fish and Wildlife" = "#C4AF45",
  "California Department of Parks and Recreation" = "#8A716A"
)
colors2 <- c(
  "NPS" = "#031D3A",
  "TNC" = "#00ABF5",
  "USFS" = "#047658",
  "CDFW" = "#C4AF45",
  "CDPR" = "#8A716A"
)

# Manager Summary ---------------------------------------------------------

## NPS

length(unique(NPS$suid_nma)) # 64

NPS_unique <- NPS %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(NPS_unique$shape_area) / 1000000 # 30743.97

# percent in GAP 1/2
sum(filter(NPS_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(NPS_unique$shape_area) * 100
# 99.69799

# median HFP
median(NPS$HFP2009, na.rm = TRUE) # 1.237312

# median strava
median(NPS$gray_scale, na.rm = TRUE) # 17.90872

# median native plant species
median(NPS$ntv_plnt, na.rm = TRUE) #  976

# median native animal species
median(NPS$ntv_anm, na.rm = TRUE) #  114

# median rare plant species
median(NPS$rar_plnt, na.rm = TRUE) #  1

# median rare animal species
median(NPS$rare_anm, na.rm = TRUE) #  1

## TNC

length(unique(TNC$suid_nma)) # 40

TNC_unique <- TNC %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(TNC_unique$shape_area) / 1000000 # 733.2876

# percent in GAP 1/2
sum(filter(TNC_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(TNC_unique$shape_area) * 100
# 92.41046

# median HFP
median(TNC$HFP2009, na.rm = TRUE) # 4.926998

# median strava
median(TNC$gray_scale, na.rm = TRUE) # 70.23158

# median native plant species
median(TNC$ntv_plnt, na.rm = TRUE) #  1076

# median native animal species
median(TNC$ntv_anm, na.rm = TRUE) #  218

# median rare plant species
median(TNC$rar_plnt, na.rm = TRUE) #  2

# median rare animal species
median(TNC$rare_anm, na.rm = TRUE) #  4

## USFS

length(unique(UC$suid_nma)) # 39

UC_unique <- UC %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(UC_unique$shape_area) / 1000000 # 84149.4

# percent in GAP 1/2
sum(filter(UC_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(UC_unique$shape_area) * 100
# 10.50222

# median HFP
median(UC$HFP2009, na.rm = TRUE) # 1.569007

# median strava
median(UC$gray_scale, na.rm = TRUE) # 27.88435

# median native plant species
median(UC$ntv_plnt, na.rm = TRUE) #  1564

# median native animal species
median(UC$ntv_anm, na.rm = TRUE) #  208

# median rare plant species
median(UC$rar_plnt, na.rm = TRUE) #  1

# median rare animal species
median(UC$rare_anm, na.rm = TRUE) #  3

## CDFW

length(unique(CDFW$suid_nma)) # 392

CDFW_unique <- CDFW %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(CDFW_unique$shape_area) / 1000000 # 3622.484

# percent in GAP 1/2
sum(filter(CDFW_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(CDFW_unique$shape_area) * 100
# 94.13079

# median HFP
median(CDFW$HFP2009, na.rm = TRUE) # 6.835479

# median strava
median(CDFW$gray_scale, na.rm = TRUE) # 73.4608

# median native plant species
median(CDFW$ntv_plnt, na.rm = TRUE) #  1094

# median native animal species
median(CDFW$ntv_anm, na.rm = TRUE) #  217

# median rare plant species
median(CDFW$rar_plnt, na.rm = TRUE) #  2

# median rare animal species
median(CDFW$rare_anm, na.rm = TRUE) #  4

## CDPR

length(unique(CDPR$suid_nma)) # 266

CDPR_unique <- CDPR %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(CDPR_unique$shape_area) / 1000000 # 6540.609

# percent in GAP 1/2
sum(filter(CDPR_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(CDPR_unique$shape_area) * 100
# 85.82902

# median HFP
median(CDPR$HFP2009, na.rm = TRUE) # 6.567569

# median strava
median(CDPR$gray_scale, na.rm = TRUE) # 130.2696

# median native plant species
median(CDPR$ntv_plnt, na.rm = TRUE) #  1224

# median native animal species
median(CDPR$ntv_anm, na.rm = TRUE) #  221

# median rare plant species
median(CDPR$rar_plnt, na.rm = TRUE) #  3

# median rare animal species
median(CDPR$rare_anm, na.rm = TRUE) #  5

# GAP Summary -------------------------------------------------------------

# GAP 1

length(unique(gap1$suid_nma)) # 372

gap1_unique <- gap1 %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(gap1_unique$shape_area) / 1000000 # 39428.07

# median HFP
median(gap1$HFP2009, na.rm = TRUE) # 1.414166

# median strava
median(gap1$gray_scale, na.rm = TRUE) # 26.52543

# median native plant species
median(gap1$ntv_plnt, na.rm = TRUE) #  1044

# median native animal species
median(gap1$ntv_anm, na.rm = TRUE) #  136

# median rare plant species
median(gap1$rar_plnt, na.rm = TRUE) #  1

# median rare animal species
median(gap1$rare_anm, na.rm = TRUE) #  1

# GAP 2

length(unique(gap2$suid_nma)) # 1674

gap2_unique <- gap2 %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(gap2_unique$shape_area) / 1000000 # 17983.81

# median HFP
median(gap2$HFP2009, na.rm = TRUE) # 9.557339

# median strava
median(gap2$gray_scale, na.rm = TRUE) # 26.52543

# median native plant species
median(gap2$ntv_plnt, na.rm = TRUE) #  1224

# median native animal species
median(gap2$ntv_anm, na.rm = TRUE) #  227

# median rare plant species
median(gap2$rar_plnt, na.rm = TRUE) #  3

# median rare animal species
median(gap2$rare_anm, na.rm = TRUE) #  5

# GAP 3

length(unique(gap3$suid_nma)) # 334

gap3_unique <- gap3 %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(gap3_unique$shape_area) / 1000000 # 137701.4

# median HFP
median(gap3$HFP2009, na.rm = TRUE) # 2.100229

# median strava
median(gap3$gray_scale, na.rm = TRUE) # 22.55111

# median native plant species
median(gap3$ntv_plnt, na.rm = TRUE) #  1257

# median native animal species
median(gap3$ntv_anm, na.rm = TRUE) #  198

# median rare plant species
median(gap3$rar_plnt, na.rm = TRUE) #  1

# median rare animal species
median(gap3$rare_anm, na.rm = TRUE) #  2

# GAP 4

length(unique(gap4$suid_nma)) # 8211

gap4_unique <- gap4 %>%
  select(suid_nma, shape_area, gap_status) %>%
  distinct()

# area managed
sum(gap4_unique$shape_area) / 1000000 # 6344.215

# median HFP
median(gap4$HFP2009, na.rm = TRUE) # 34.23607

# median strava
median(gap4$gray_scale, na.rm = TRUE) # 236.4428

# median native plant species
median(gap4$ntv_plnt, na.rm = TRUE) #  1094

# median native animal species
median(gap4$ntv_anm, na.rm = TRUE) #  226

# median rare plant species
median(gap4$rar_plnt, na.rm = TRUE) #  2

# median rare animal species
median(gap4$rare_anm, na.rm = TRUE) #  5

# Map ---------------------------------------------------------------------

mang_sub <- human %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Parks and Recreation",
    "California Department of Fish and Wildlife"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  ))

p11 <- ggplot() +
  geom_sf(data = human, aes(fill = gap_status), color = "transparent") +
  geom_sf(data = mang_sub, aes(color = mng_agncy, fill = gap_status), linewidth = .8) +
  theme_bw(base_size = 20) +
  labs(
    x = "",
    y = "",
    color = "",
    fill = ""
  ) +
  scale_color_manual(values = colors2) +
  scale_fill_manual(values = viridis::viridis(5, end = .9)[1:4])

p1 <- ggplot() +
  geom_sf(data = human, aes(fill = gap_status), color = "transparent") +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "",
    color = "",
    fill = ""
  ) +
  scale_fill_manual(values = viridis::viridis(5, end = .9)[1:4])

p2 <- ggplot() +
  geom_sf(data = mang_sub, aes(fill = mng_agncy), color = "transparent") +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "",
    color = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2)

p3 <- ggplot() +
  geom_sf(data = human, aes(fill = ntv_med), color = "transparent") +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "",
    color = "",
    fill = "Median Native Species Count"
  ) +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

p4 <- ggplot() +
  geom_sf(data = human, aes(fill = rare_med), color = "transparent") +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "",
    color = "",
    fill = "Median Rare Species Count"
  ) +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

p5 <- ggplot() +
  geom_sf(data = human, aes(fill = HFP_median), color = "transparent") +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "",
    color = "",
    fill = "Median Landscape Modification"
  ) +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

p6 <- ggplot() +
  geom_sf(data = human, aes(fill = gray_scale), color = "transparent") +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "",
    color = "",
    fill = "Median Human Mobility"
  ) +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

ggsave(plot = p11, filename = paste0("fig1.pdf"), path = here::here("analysis", "figs"), height = 7, width = 8, device = cairo_pdf)

plot <- (p1 + p2) / (p3 + p4) / (p5 + p6) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("figS1.pdf"), path = here::here("analysis", "figs"), height = 15, width = 19, device = cairo_pdf)

# ANOVA -------------------------------------------------------------------

strava_inside <- spatial_data %>%
  select(gray_scale, gap_status, suid_nma, mng_agncy) %>%
  filter(!is.na(gray_scale))

hfp_inside <- spatial_data %>%
  select(HFP2009, gap_status, suid_nma, mng_agncy) %>%
  filter(!is.na(HFP2009))

bio_inside <- spatial_data %>%
  select(native, gap_status, suid_nma, mng_agncy) %>%
  filter(!is.na(native))

rare_inside <- spatial_data %>%
  select(rare, gap_status, suid_nma, mng_agncy) %>%
  filter(!is.na(rare))

lm1 <- lmer(gray_scale ~ gap_status + (1 | suid_nma), data = strava_inside)
summary(lm1)
anova(lm1)
eta_squared(lm1, partial = TRUE) # gap_status | 0.20 | [0.19, 1.00]

lm2 <- lmer(HFP2009 ~ gap_status + (1 | suid_nma), data = hfp_inside)
summary(lm2)
anova(lm2)
eta_squared(lm2, partial = TRUE) # gap_status |0.32 | [0.31, 1.00]

lm3 <- lmer(native ~ gap_status + (1 | suid_nma), data = bio_inside)
summary(lm3)
anova(lm3)
eta_squared(lm3, partial = TRUE) # gap_status |           0.03 | [0.03, 1.00]

lm4 <- lmer(rare ~ gap_status + (1 | suid_nma), data = rare_inside)
anova(lm4)
summary(lm4)
eta_squared(lm4, partial = TRUE) # gap_status | 0.01 | [0.01, 1.00]

strava_inside2 <- spatial_data %>%
  select(gray_scale, gap_status, suid_nma, mng_agncy) %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Parks and Recreation",
    "California Department of Fish and Wildlife"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  )) %>%
  filter(!is.na(gray_scale))

hfp_inside2 <- spatial_data %>%
  select(HFP2009, gap_status, suid_nma, mng_agncy) %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Parks and Recreation",
    "California Department of Fish and Wildlife"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  )) %>%
  filter(!is.na(HFP2009))

bio_inside2 <- spatial_data %>%
  select(native, gap_status, suid_nma, mng_agncy) %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Parks and Recreation",
    "California Department of Fish and Wildlife"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  )) %>%
  filter(!is.na(native))

rare_inside2 <- spatial_data %>%
  select(rare, gap_status, suid_nma, mng_agncy) %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Parks and Recreation",
    "California Department of Fish and Wildlife"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  )) %>%
  filter(rare > 0) %>%
  filter(!is.na(rare))

lm5 <- lmer(gray_scale ~ mng_agncy + (1 | suid_nma), data = strava_inside2)
anova(lm5)
summary(lm5)
eta_squared(lm5, partial = TRUE) # mng_agncy | 0.12 | [0.08, 1.00]

lm6 <- lmer(HFP2009 ~ mng_agncy + (1 | suid_nma), data = hfp_inside2)
anova(lm6)
summary(lm6)
eta_squared(lm6, partial = TRUE) # mng_agncy |  0.05 | [0.02, 1.00]

lm7 <- lmer(native ~ mng_agncy + (1 | suid_nma), data = bio_inside2)
anova(lm7)
summary(lm7)
eta_squared(lm7, partial = TRUE) # mng_agncy |   0.11 | [0.08, 1.00]

lm8 <- lmer(rare ~ mng_agncy + (1 | suid_nma), data = rare_inside2)
anova(lm8)
summary(lm8)
eta_squared(lm8, partial = TRUE) # mng_agncy | 0.02 | [0.00, 1.00]


# Scatter Plot/PCA ------------------------------------------------------------

cor_check <- human_sum %>%
  select(HFP_median, gray_scale) %>%
  drop_na()

cor(cor_check$HFP_median, cor_check$gray_scale)
# 0.7561948

pca_prep_data <- human_sum %>%
  st_drop_geometry() %>%
  select(suid_nma, HFP_median, gray_scale, ntv_med, rare_med, mng_agncy, gap_status) %>% # median_sg,
  drop_na()

pca_data <- pca_prep_data %>%
  select(-suid_nma, -mng_agncy, -gap_status)

pca_corr <- cor(pca_data)

pca <- prcomp(pca_data, scale. = TRUE)
summary(pca)

# autoplot(pca, data = pca_prep_data)

explained_variance_ratio <- summary(pca)[["importance"]]["Proportion of Variance", ]
explained_variance_ratio <- 100 * explained_variance_ratio
components <- pca[["x"]]
components <- data.frame(components)
components <- cbind(components, pca_prep_data$suid_nma)
components <- cbind(components, pca_prep_data$mng_agncy) %>%
  as.data.frame()
components <- cbind(components, pca_prep_data$gap_status) %>%
  as.data.frame()

names(components) <- c(
  "PC1", "PC2", "PC3", "PC4",
  "suid_nma", "mng_agncy", "gap_status"
)

NPS_components <- components %>%
  filter(mng_agncy == "United States National Park Service") %>%
  mutate(
    centroid1 = mean(PC1),
    centroid2 = mean(PC2)
  )

TNC_components <- components %>%
  filter(mng_agncy == "The Nature Conservancy") %>%
  mutate(
    centroid1 = mean(PC1),
    centroid2 = mean(PC2)
  )

UC_components <- components %>%
  filter(mng_agncy == "United States Forest Service") %>%
  mutate(
    centroid1 = mean(PC1),
    centroid2 = mean(PC2)
  )

CDFW_components <- components %>%
  filter(mng_agncy == "California Department of Fish and Wildlife") %>%
  mutate(
    centroid1 = mean(PC1),
    centroid2 = mean(PC2)
  )

SP_components <- components %>%
  filter(mng_agncy == "California Department of Parks and Recreation") %>%
  mutate(
    centroid1 = mean(PC1),
    centroid2 = mean(PC2)
  )

components <- components %>%
  filter(PC2 > -10)

loadings <- as.data.frame(pca["rotation"])

gap_mean <- components %>%
  group_by(gap_status) %>%
  summarize(
    centroid1 = mean(PC1),
    centroid2 = mean(PC2)
  )

HFI_icon <- rsvg_png(here::here("icons", "building-regular-full.svg"))
strava_icon <- rsvg_png(here::here("icons", "person-running-solid-full.svg"))
native_icon <- rsvg_png(here::here("icons", "seedling-solid-full.svg"))
rare_icon <- rsvg_png(here::here("icons", "kiwi-bird-solid-full.svg"))

readPNG_alpha <- function(png_path, alpha = 1, width_lines = 4, height_lines = 4) {
  img <- readPNG(png_path)

  # If PNG has no alpha channel, add one
  if (dim(img)[3] == 3) {
    alpha_channel <- array(1, dim = c(dim(img)[1], dim(img)[2], 1))
    img <- abind::abind(img, alpha_channel, along = 3)
  }

  # Multiply alpha channel by desired alpha
  img[, , 4] <- img[, , 4] * alpha

  # Make grob
  rasterGrob(img, width = unit(width_lines, "lines"), height = unit(height_lines, "lines"))
}

HFI_grob <- readPNG_alpha(HFI_icon, 1)
strava_grob <- readPNG_alpha(strava_icon, 1)
native_grob <- readPNG_alpha(native_icon, 1)
rare_grob <- readPNG_alpha(rare_icon, 1)

rm(HFI_icon, strava_icon, native_icon, rare_icon)

p1 <- ggplot() +
  geom_point(data = components %>% filter(PC2 < 10), aes(PC1, PC2), color = "grey") +
  geom_point(data = CDFW_components, aes(PC1, PC2, color = "California Department of Fish and Wildlife")) +
  geom_point(data = SP_components, aes(PC1, PC2, color = "California Department of Parks and Recreation")) +
  geom_point(data = NPS_components, aes(PC1, PC2, color = "United States National Park Service")) +
  geom_point(data = UC_components, aes(PC1, PC2, color = "United States Forest Service")) +
  geom_point(data = TNC_components, aes(PC1, PC2, color = "The Nature Conservancy")) +
  stat_ellipse(data = CDFW_components, aes(PC1, PC2, color = "California Department of Fish and Wildlife"), level = 0.95, type = "t", alpha = 0.7, linewidth = 1.5) +
  stat_ellipse(data = SP_components, aes(PC1, PC2, color = "California Department of Parks and Recreation"), level = 0.95, type = "t", alpha = 0.7, linewidth = 1.5) +
  stat_ellipse(data = NPS_components, aes(PC1, PC2, color = "United States National Park Service"), level = 0.95, type = "t", alpha = 0.7, linewidth = 1.5) +
  stat_ellipse(data = UC_components, aes(PC1, PC2, color = "United States Forest Service"), level = 0.95, type = "t", alpha = 0.7, linewidth = 1.5) +
  stat_ellipse(data = TNC_components, aes(PC1, PC2, color = "The Nature Conservancy"), level = 0.95, type = "t", alpha = 0.7, linewidth = 1.5) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("PC1 (", round(explained_variance_ratio[1], 2), "%)"),
    y = paste0("PC2 (", round(explained_variance_ratio[2], 2), "%)"),
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

p3 <- ggplot() +
  geom_segment(data = loadings, aes(x = 0, xend = rotation.PC1, y = 0, yend = rotation.PC2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 2, colour = "grey20") +
  annotation_custom(strava_grob, xmin = loadings$rotation.PC1[1] - 0.04, xmax = loadings$rotation.PC1[1], ymin = loadings$rotation.PC2[1], ymax = loadings$rotation.PC2[1]) +
  annotation_custom(HFI_grob, xmin = loadings$rotation.PC1[2] - 0.04, xmax = loadings$rotation.PC1[2], ymin = loadings$rotation.PC2[2], ymax = loadings$rotation.PC2[2]) +
  annotation_custom(native_grob, xmin = loadings$rotation.PC1[3], xmax = loadings$rotation.PC1[3], ymin = loadings$rotation.PC2[3] + 0.15, ymax = loadings$rotation.PC2[3] + 0.08) +
  annotation_custom(rare_grob, xmin = loadings$rotation.PC1[4] - 0.05, xmax = loadings$rotation.PC1[4], ymin = loadings$rotation.PC2[4], ymax = loadings$rotation.PC2[4] + 0.05) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("PC1 (", round(explained_variance_ratio[1], 2), "%)"),
    y = paste0("PC2 (", round(explained_variance_ratio[2], 2), "%)"),
    color = ""
  ) +
  theme(legend.position = "none") +
  ylim(c(-0.4, 0.9)) +
  xlim(c(-0.8, 0.1))

p2 <- ggplot() +
  geom_point(data = components %>% filter(PC2 < 10), aes(PC1, PC2, color = gap_status), alpha = 0.5) +
  stat_ellipse(data = components %>% filter(PC2 < 10), aes(PC1, PC2, color = gap_status), level = 0.95, type = "t", alpha = 0.7, linewidth = 1.5) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("PC1 (", round(explained_variance_ratio[1], 2), "%)"),
    y = paste0("PC2 (", round(explained_variance_ratio[2], 2), "%)"),
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = viridis::viridis(5, end = .9)[1:4]) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave((p3) / (p2 + p1) + plot_annotation(tag_levels = "A"), file = paste0("fig2.pdf"), path = here::here("analysis", "figs"), height = 15, width = 22)

# Spatial Analysis --------------------------------------------------------

spatial_data <- spatial_data %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(count >= 30) %>%
  select(-count)

spatial_data_sf <- spatial_data %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3310")

spatial_data <- spatial_data %>%
  select(-x, -y)

spatial_data$x <- st_coordinates(spatial_data_sf)[, 1] / 1000
spatial_data$y <- st_coordinates(spatial_data_sf)[, 2] / 1000

park_models <- spatial_data %>%
  select(gray_scale, suid_nma, HFP2009, y, x, mng_agncy, ntv_anm, rare_anm, shape_area) %>%
  distinct()

park_var <- park_models %>%
  group_by(suid_nma) %>%
  summarize(
    hfp = sd(HFP2009, na.rm = TRUE),
    native_count = sd(ntv_anm, na.rm = TRUE),
    rare_count = sd(rare_anm, na.rm = TRUE),
    strava = sd(gray_scale, na.rm = TRUE)
  ) %>%
  group_by(suid_nma) %>%
  mutate(total_var = sum(hfp, native_count, rare_count, strava)) %>%
  filter(total_var > 0)

park_models <- park_models %>%
  filter(suid_nma %in% park_var$suid_nma) %>%
  group_by(suid_nma) %>%
  mutate(
    mean_hfp = mean(HFP2009, na.rm = TRUE),
    mean_native_count = mean(ntv_anm, na.rm = TRUE),
    mean_rare_count = mean(rare_anm, na.rm = TRUE),
    mean_strava = mean(gray_scale, na.rm = TRUE),
    sd_hfp = sd(HFP2009, na.rm = TRUE),
    sd_native_count = sd(ntv_anm, na.rm = TRUE),
    sd_rare_count = sd(rare_anm, na.rm = TRUE),
    sd_strava = sd(gray_scale, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    HFP2009 = (HFP2009 - mean_hfp) / sd_hfp,
    native_count = (ntv_anm - mean_native_count) / sd_native_count,
    rare_count = (rare_anm - mean_rare_count) / sd_rare_count,
    gray_scale = (gray_scale - mean_strava) / sd_strava
  ) %>%
  distinct() %>%
  group_by(suid_nma) %>%
  mutate(count = n())

length(unique(park_models$suid_nma))

beta_animal <- data.frame()

for (i in 1:length(unique(park_models$suid_nma))) {
  gam1 <- gam(gray_scale ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["HFP2009", "Estimate"]
  se <- summary(gam1)$p.table["HFP2009", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Landscape Modification and Human Mobility")

  beta_animal <- rbind(beta_animal, coef)

  gam1 <- gam(native_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["HFP2009", "Estimate"]
  se <- summary(gam1)$p.table["HFP2009", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Landscape Modification")

  beta_animal <- rbind(beta_animal, coef)

  gam1 <- gam(native_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["gray_scale", "Estimate"]
  se <- summary(gam1)$p.table["gray_scale", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Human Mobility")

  beta_animal <- rbind(beta_animal, coef)

  gam1 <- gam(rare_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["HFP2009", "Estimate"]
  se <- summary(gam1)$p.table["HFP2009", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Landscape Modification")

  beta_animal <- rbind(beta_animal, coef)

  gam1 <- gam(rare_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["gray_scale", "Estimate"]
  se <- summary(gam1)$p.table["gray_scale", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Human Mobility")

  beta_animal <- rbind(beta_animal, coef)
}

beta_animal <- full_join(beta_animal, park_models %>% select(suid_nma, count)) %>%
  distinct()

park_models <- spatial_data %>%
  select(gray_scale, suid_nma, HFP2009, y, x, mng_agncy, ntv_plnt, rar_plnt, shape_area) %>%
  distinct()

park_var <- park_models %>%
  group_by(suid_nma) %>%
  summarize(
    hfp = sd(HFP2009, na.rm = TRUE),
    native_count = sd(ntv_plnt, na.rm = TRUE),
    rare_count = sd(rar_plnt, na.rm = TRUE),
    strava = sd(gray_scale, na.rm = TRUE)
  ) %>%
  group_by(suid_nma) %>%
  mutate(total_var = sum(hfp, native_count, rare_count, strava)) %>%
  filter(total_var > 0)

park_models <- park_models %>%
  filter(suid_nma %in% park_var$suid_nma) %>%
  group_by(suid_nma) %>%
  mutate(
    mean_hfp = mean(HFP2009, na.rm = TRUE),
    mean_native_count = mean(ntv_plnt, na.rm = TRUE),
    mean_rare_count = mean(rar_plnt, na.rm = TRUE),
    mean_strava = mean(gray_scale, na.rm = TRUE),
    sd_hfp = sd(HFP2009, na.rm = TRUE),
    sd_native_count = sd(ntv_plnt, na.rm = TRUE),
    sd_rare_count = sd(rar_plnt, na.rm = TRUE),
    sd_strava = sd(gray_scale, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    HFP2009 = (HFP2009 - mean_hfp) / sd_hfp,
    native_count = (ntv_plnt - mean_native_count) / sd_native_count,
    rare_count = (rar_plnt - mean_rare_count) / sd_rare_count,
    gray_scale = (gray_scale - mean_strava) / sd_strava
  ) %>%
  distinct() %>%
  filter(!(suid_nma %in% c(14013, 14029, 14047, 14172, 14248, 14310))) %>%
  group_by(suid_nma) %>%
  mutate(count = n())

length(unique(park_models$suid_nma))

beta_plant <- data.frame()

for (i in 1:length(unique(park_models$suid_nma))) {
  gam1 <- gam(native_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["HFP2009", "Estimate"]
  se <- summary(gam1)$p.table["HFP2009", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Landscape Modification")

  beta_plant <- rbind(beta_plant, coef)

  gam1 <- gam(native_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["gray_scale", "Estimate"]
  se <- summary(gam1)$p.table["gray_scale", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Human Mobility")

  beta_plant <- rbind(beta_plant, coef)

  gam1 <- gam(rare_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["HFP2009", "Estimate"]
  se <- summary(gam1)$p.table["HFP2009", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Landscape Modification")

  beta_plant <- rbind(beta_plant, coef)

  gam1 <- gam(rare_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))

  est <- summary(gam1)$p.table["gray_scale", "Estimate"]
  se <- summary(gam1)$p.table["gray_scale", "Std. Error"]

  upper <- est + (1.96 * se)
  lower <- est - (1.96 * se)

  coef <- data.frame(value = est, upper = upper, lower = lower, suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Human Mobility")

  beta_plant <- rbind(beta_plant, coef)
}

beta_plant <- full_join(beta_plant, park_models %>% select(suid_nma, count)) %>%
  distinct()

beta <- full_join(beta_plant %>% mutate(type = "Plant"), beta_animal %>% mutate(type = "Animal")) %>%
  mutate(type = case_when(
    model == "Landscape Modification and Human Mobility" ~ NA,
    TRUE ~ type
  )) %>%
  mutate(suid_nma = fct_reorder(interaction(model, suid_nma, lex.order = TRUE), value)) %>%
  mutate(sig = case_when(
    upper > 0 & lower > 0 ~ "Positive Significant",
    upper < 0 & lower < 0 ~ "Negative Significant",
    TRUE ~ "Not Significant"
  ))

t_test(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Animal"), "value", weights = "count", alternative = "greater")
# p-value < .001

limits1 <- ifelse(max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Animal") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
    type == "Animal") %>% pull(lower))),
max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Animal") %>% pull(upper))),
max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Animal") %>% pull(lower)))
)

p1 <- ggplot(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Animal") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.14), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Animal \nNative Species and Landscape Modification",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#D01C8B", "darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits1, limits1))

t_test(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Animal"), "value", weights = "count", alternative = "greater")
# < .001

limits2 <- ifelse(max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Animal") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
    type == "Animal") %>% pull(lower))),
max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Animal") %>% pull(upper))),
max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Animal") %>% pull(lower)))
)

p2 <- ggplot(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Animal") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.11), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Animal \nNative Species and Human Mobility",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits2, limits2))

t_test(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Animal"), "value", weights = "count", alternative = "greater")
# < .001

limits3 <- ifelse(max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Animal") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
    type == "Animal") %>% pull(lower))),
max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Animal") %>% pull(upper))),
max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Animal") %>% pull(lower)))
)

p3 <- ggplot(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Animal") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.21), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Animal \nRare Species and Landscape Modification",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits3, limits3))

t_test(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Animal"), "value", weights = "count", alternative = "greater")
# < .001

limits4 <- ifelse(max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Animal") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
    type == "Animal") %>% pull(lower))),
max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Animal") %>% pull(upper))),
max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Animal") %>% pull(lower)))
)

p4 <- ggplot(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Animal") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.26), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Animal \nRare Species and Human Mobility",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits4, limits4))

t_test(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Plant"), "value", weights = "count", alternative = "greater")
# p-value > .999

limits5 <- ifelse(max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Plant") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
    type == "Plant") %>% pull(lower))),
max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Plant") %>% pull(upper))),
max(abs(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Plant") %>% pull(lower)))
)

p5 <- ggplot(beta %>% filter(model == "Native Species and Landscape Modification" &
  type == "Plant") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = -0.07), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Plant \nNative Species and Landscape Modification",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#D01C8B", "darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits5, limits5))

t_test(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Plant"), "value", weights = "count", alternative = "greater")
# p-value = 0.343

limits6 <- ifelse(max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Plant") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
    type == "Plant") %>% pull(lower))),
max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Plant") %>% pull(upper))),
max(abs(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Plant") %>% pull(lower)))
)

p6 <- ggplot(beta %>% filter(model == "Native Species and Human Mobility" &
  type == "Plant") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.01), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Plant \nNative Species and Human Mobility",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#D01C8B", "darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits6, limits6))


t_test(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Plant"), "value", weights = "count", alternative = "greater")
# p-value = 0.004

limits7 <- ifelse(max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Plant") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
    type == "Plant") %>% pull(lower))),
max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Plant") %>% pull(upper))),
max(abs(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Plant") %>% pull(lower)))
)

p7 <- ggplot(beta %>% filter(model == "Rare Species and Landscape Modification" &
  type == "Plant") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.06), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Plant \nRare Species and Landscape Modification",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#D01C8B", "darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits7, limits7))

t_test(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Plant"), "value", weights = "count", alternative = "greater")
# p-value < .001

limits8 <- ifelse(max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Plant") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
    type == "Plant") %>% pull(lower))),
max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Plant") %>% pull(upper))),
max(abs(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Plant") %>% pull(lower)))
)

p8 <- ggplot(beta %>% filter(model == "Rare Species and Human Mobility" &
  type == "Plant") %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.18), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "Plant \nRare Species and Human Mobility",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits8, limits8))

t_test(beta %>% filter(model == "Landscape Modification and Human Mobility"),
  "value",
  weights = "count", alternative = "greater"
)
# p-value < .001

limits9 <- ifelse(max(abs(beta %>% filter(model == "Landscape Modification and Human Mobility") %>% pull(upper)))
>
  max(abs(beta %>% filter(model == "Landscape Modification and Human Mobility") %>% pull(lower))),
max(abs(beta %>% filter(model == "Landscape Modification and Human Mobility") %>% pull(upper))),
max(abs(beta %>% filter(model == "Landscape Modification and Human Mobility") %>% pull(lower)))
)

p9 <- ggplot(beta %>% filter(is.na(type)) %>% mutate(suid_nma = fct_reorder(suid_nma, value))) +
  geom_segment(aes(x = lower, xend = upper, y = suid_nma, color = sig)) +
  geom_point(aes(x = value, y = suid_nma, color = sig), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", alpha = 0.3) +
  geom_vline(aes(xintercept = 0.36), linetype = "dashed", alpha = 0.3, color = "red") +
  theme_bw(base_size = 20) +
  labs(
    x = "Beta",
    y = "\nLandscape Modification and Human Mobility",
    color = ""
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("darkgrey", "#1B9E77")) +
  # scale_color_gradient2(low = "#762A83", mid = "darkgrey", high = "#1B7837", midpoint = 0, breaks = c(-.5, -0.25, 0, 0.25, 0.5, 0.75, 1)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "white")
  ) +
  xlim(c(-limits9, limits9))

layout <- "
ABCD
EFGH
"

plot <- (p1 + p3 + p7 + p8) / (p2 + p4 + p9 + plot_spacer()) + plot_annotation(tag_levels = "A") + plot_layout(design = layout)

ggsave(plot, file = paste0("fig3.pdf"), path = here::here("analysis", "figs"), height = 25, width = 35)

ggsave(p5 / p6 + plot_annotation(tag_levels = "A"), file = paste0("figS2.pdf"), path = here::here("analysis", "figs"), height = 12, width = 10)
