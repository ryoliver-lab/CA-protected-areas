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
library(ggridges)
library(ggExtra)
library(gghalves)
library(ggtext)
library(ggimage)
library(rsvg)
library(geosphere)
library(ggfortify)
library(ggpmisc)
library(philentropy)
library(spdep)
library(base64enc)
library(grid)
library(png)
library(effectsize)

# coastal is 8 km inland due to sea level rise projections

sf_use_s2(FALSE)

# Data Input --------------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

strava <- st_read(here::here("data", "processed_data", "cpad_strava.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super))
hfp <- st_read(here::here("data", "processed_data", "cpad_hfp.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super)) %>% 
  st_drop_geometry()
bio <- st_read(here::here("data", "processed_data", "cpad_bio.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super))

human <- list(hfp, bio, strava) %>%
  reduce(full_join) %>%
  st_as_sf()

rm(hfp, bio, strava)

# take out hoas and other protected areas

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
    suid_nma, gry_scl,
    access_typ, park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    HFP_median,  bio_mdn, rar_mdn
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

cpad_no_geom <- cpad_super %>%
  st_drop_geometry()

strava <- st_read(here::here("data", "processed_data", "in_out_strava.shp")) %>%
  st_drop_geometry() %>%
  filter(!is.na(gray_scale)) %>% 
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  full_join(cpad_no_geom %>% select(ID, suid_nma)) %>% 
  filter(suid_nma %in% human$suid_nma) %>%           
  full_join(human %>% select(suid_nma, gap_status, mng_agncy, shape_area)) %>%
  select(-ID)

hfp <- st_read(here::here("data", "processed_data", "in_out_hfp.shp")) %>%
  st_drop_geometry() %>%
  filter(!is.na(HFP2009)) %>% 
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  full_join(cpad_no_geom %>% select(ID, suid_nma)) %>%
  filter(suid_nma %in% human$suid_nma) %>%   
  full_join(human %>% select(suid_nma, gap_status, mng_agncy, shape_area)) %>%
  select(-ID)

bio <- st_read(here::here("data", "processed_data", "in_out_bio.shp")) %>%
  st_drop_geometry() %>%
  filter(!is.na(ntv_cnt)) %>% 
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  full_join(cpad_no_geom %>% select(ID, suid_nma)) %>%   
  filter(suid_nma %in% human$suid_nma) %>%   
  full_join(human %>% select(suid_nma, gap_status, mng_agncy, shape_area)) %>%
  select(-ID)

rare <- st_read(here::here("data", "processed_data", "in_out_rare.shp")) %>%
  st_drop_geometry() %>%
  filter(!is.na(rare_count)) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  full_join(cpad_no_geom %>% select(ID, suid_nma)) %>% 
  filter(suid_nma %in% human$suid_nma) %>%   
  full_join(human %>% select(suid_nma, gap_status, mng_agncy, shape_area)) %>%
  select(-ID)

pixel_sum = list(strava %>% rename(value = gray_scale) %>% mutate(measure = "recreation"), 
                 hfp%>% rename(value = HFP2009) %>% mutate(measure = "mod"), 
                 bio%>% rename(value = ntv_cnt) %>% mutate(measure = "bio"), 
                 rare%>% rename(value = rare_count) %>% mutate(measure = "rare")) %>% 
  reduce(rbind)

NPS <- pixel_sum %>%
  filter(mng_agncy == "United States National Park Service")

TNC <- pixel_sum %>%
  filter(mng_agncy == "The Nature Conservancy")

UC <- pixel_sum %>%
  filter(mng_agncy == "United States Forest Service")

CDFW <- pixel_sum %>%
  filter(mng_agncy == "California Department of Fish and Wildlife")

CDPR <- pixel_sum %>%
  filter(mng_agncy == "California Department of Parks and Recreation")

gap1 <- pixel_sum %>%
  filter(gap_status == "GAP 1")

gap2 <- pixel_sum %>%
  filter(gap_status == "GAP 2")

gap3 <- pixel_sum %>%
  filter(gap_status == "GAP 3")

gap4 <- pixel_sum %>%
  filter(gap_status == "GAP 4")

rm(pixel_sum)

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  filter(suid_nma %in% human$suid_nma) %>%            
  full_join(human%>% st_drop_geometry() %>% select(suid_nma, gap_status, mng_agncy, shape_area))  %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(count >= 30) %>% 
  select(-count)  

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

length(unique(NPS$suid_nma)) #64

NPS_unique = NPS %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(NPS_unique$shape_area) / 1000000 #30743.97

# percent in GAP 1/2
sum(filter(NPS_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(NPS_unique$shape_area) * 100
#99.69799

# median HFP
median(NPS[grepl("mod", NPS[, 'measure']), ]$value, na.rm = TRUE) # 1.237312

# median strava
median(NPS[grepl("recreation", NPS[, 'measure']), ]$value, na.rm = TRUE) # 17.90872

# median native species
median(NPS[grepl("bio", NPS[, 'measure']), ]$value, na.rm = TRUE)  #114

# median rare
median(NPS[grepl("rare", NPS[, 'measure']), ]$value, na.rm = TRUE)  #2

## TNC

length(unique(TNC$suid_nma)) #40

TNC_unique = TNC %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(TNC_unique$shape_area) / 1000000 #733.2876

# percent in GAP 1/2
sum(filter(TNC_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(TNC_unique$shape_area) * 100
#92.41046

# median HFP
median(TNC[grepl("mod", TNC[, 'measure']), ]$value, na.rm = TRUE) # 4.927719

# median strava
median(TNC[grepl("recreation", TNC[, 'measure']), ]$value, na.rm = TRUE) # 70.23158

# median native species
median(TNC[grepl("bio", TNC[, 'measure']), ]$value, na.rm = TRUE)  #218

# median rare
median(TNC[grepl("rare", TNC[, 'measure']), ]$value, na.rm = TRUE)  #7

## USFS

length(unique(UC$suid_nma)) #39

UC_unique = UC %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(UC_unique$shape_area) / 1000000 #84149.4

# percent in GAP 1/2
sum(filter(UC_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(UC_unique$shape_area) * 100
#10.50222

# median HFP
median(UC[grepl("mod", UC[, 'measure']), ]$value, na.rm = TRUE) # 1.567609

# median strava
median(UC[grepl("recreation", UC[, 'measure']), ]$value, na.rm = TRUE) #  27.88435

# median native species
median(UC[grepl("bio", UC[, 'measure']), ]$value, na.rm = TRUE)  #208

# median rare
median(UC[grepl("rare", UC[, 'measure']), ]$value, na.rm = TRUE)  #5

## CDFW

length(unique(CDFW$suid_nma)) #392

CDFW_unique = CDFW %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(CDFW_unique$shape_area) / 1000000 #3622.484

# percent in GAP 1/2
sum(filter(CDFW_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(CDFW_unique$shape_area) * 100
#94.13079

# median HFP
median(CDFW[grepl("mod", CDFW[, 'measure']), ]$value, na.rm = TRUE) # 6.826514

# median strava
median(CDFW[grepl("recreation", CDFW[, 'measure']), ]$value, na.rm = TRUE) # 73.4608

# median native species
median(CDFW[grepl("bio", CDFW[, 'measure']), ]$value, na.rm = TRUE)  #217

# median rare
median(CDFW[grepl("rare", CDFW[, 'measure']), ]$value, na.rm = TRUE)  #7

## CDPR

length(unique(CDPR$suid_nma)) #266

CDPR_unique = CDPR %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(CDPR_unique$shape_area) / 1000000 #6540.609

# percent in GAP 1/2
sum(filter(CDPR_unique, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(CDPR_unique$shape_area) * 100
#85.82902

# median HFP
median(CDPR[grepl("mod", CDPR[, 'measure']), ]$value, na.rm = TRUE) # 6.585459

# median strava
median(CDPR[grepl("recreation", CDPR[, 'measure']), ]$value, na.rm = TRUE) # 130.2696

# median native species
median(CDPR[grepl("bio", CDPR[, 'measure']), ]$value, na.rm = TRUE)  #221

# median rare
median(CDPR[grepl("rare", CDPR[, 'measure']), ]$value, na.rm = TRUE)  #8

# GAP Summary -------------------------------------------------------------

# GAP 1

length(unique(gap1$suid_nma)) #372

gap1_unique = gap1 %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(gap1_unique$shape_area) / 1000000 #39428.07

# median HFP
median(gap1[grepl("mod", gap1[, 'measure']), ]$value, na.rm = TRUE) # 1.413888

# median strava
median(gap1[grepl("recreation", gap1[, 'measure']), ]$value, na.rm = TRUE) # 26.52543

# median native species
median(gap1[grepl("bio", gap1[, 'measure']), ]$value, na.rm = TRUE)  #136

# median rare
median(gap1[grepl("rare", gap1[, 'measure']), ]$value, na.rm = TRUE)  #3

# GAP 2

length(unique(gap2$suid_nma)) #1674

gap2_unique = gap2 %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(gap2_unique$shape_area) / 1000000 #17983.81

# median HFP
median(gap2[grepl("mod", gap2[, 'measure']), ]$value, na.rm = TRUE) # 9.556896

# median strava
median(gap2[grepl("recreation", gap2[, 'measure']), ]$value, na.rm = TRUE) # 146.8858

# median native species
median(gap2[grepl("bio", gap2[, 'measure']), ]$value, na.rm = TRUE)  #227

# median rare
median(gap2[grepl("rare", gap2[, 'measure']), ]$value, na.rm = TRUE)  #9

# GAP 3

length(unique(gap3$suid_nma)) #334

gap3_unique = gap3 %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(gap3_unique$shape_area) / 1000000 #137701.4

# median HFP
median(gap3[grepl("mod", gap3[, 'measure']), ]$value, na.rm = TRUE) # 2.10075

# median strava
median(gap3[grepl("recreation", gap3[, 'measure']), ]$value, na.rm = TRUE) # 22.55111

# median native species
median(gap3[grepl("bio", gap3[, 'measure']), ]$value, na.rm = TRUE)  #198

# median rare
median(gap3[grepl("rare", gap3[, 'measure']), ]$value, na.rm = TRUE)  #4

# GAP 4

length(unique(gap4$suid_nma)) #8211

gap4_unique = gap4 %>% 
  select(suid_nma, shape_area, gap_status) %>% 
  distinct()

# area managed
sum(gap4_unique$shape_area) / 1000000 #6344.215

# median HFP
median(gap4[grepl("mod", gap4[, 'measure']), ]$value, na.rm = TRUE) # 28.62146

# median strava
median(gap4[grepl("recreation", gap4[, 'measure']), ]$value, na.rm = TRUE) #200.7714

# median native species
median(gap4[grepl("bio", gap4[, 'measure']), ]$value, na.rm = TRUE)  #216

# median rare
median(gap4[grepl("rare", gap4[, 'measure']), ]$value, na.rm = TRUE)  #5

# Map ---------------------------------------------------------------------

mang_sub = human %>% 
  filter(mng_agncy %in% c("United States National Park Service",
                                   "The Nature Conservancy",
                                   "United States Forest Service",
                                   "California Department of Parks and Recreation",
                                   "California Department of Fish and Wildlife")) %>% 
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  ))

p11 = ggplot() +
  geom_sf(data = human, aes(fill = gap_status), color = "transparent") +
  geom_sf(data = mang_sub, aes(color = mng_agncy, fill = gap_status), linewidth = .8) +
  theme_bw(base_size = 16) + 
  labs(x = "", 
       y = "", 
       color = "", 
       fill = "") +
  scale_color_manual(values = colors2) +
  scale_fill_manual(values = viridis::viridis(5, end = .9)[1:4]) 

p1 = ggplot() +
  geom_sf(data = human, aes(fill = gap_status), color = "transparent") +
  theme_bw(base_size = 16) + 
  labs(x = "", 
       y = "", 
       color = "", 
       fill = "") +
  scale_fill_manual(values = viridis::viridis(5, end = .9)[1:4]) 

p2 = ggplot() +
  geom_sf(data = mang_sub, aes(fill = mng_agncy), color = "transparent") +
  theme_bw(base_size = 16) + 
  labs(x = "", 
       y = "", 
       color = "", 
       fill = "") +
  scale_fill_manual(values = colors2) 

p3 = ggplot() +
  geom_sf(data = human, aes(fill = bio_mdn), color = "transparent") +
  theme_bw(base_size = 16) + 
  labs(x = "", 
       y = "", 
       color = "", 
       fill = "Median Native Species Count") +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

p4 = ggplot() +
  geom_sf(data = human, aes(fill = rar_mdn), color = "transparent") +
  theme_bw(base_size = 16) + 
  labs(x = "", 
       y = "", 
       color = "", 
       fill = "Median Rare Species Count") +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

p5 = ggplot() +
  geom_sf(data = human, aes(fill = HFP_median), color = "transparent") +
  theme_bw(base_size = 16) + 
  labs(x = "", 
       y = "", 
       color = "", 
       fill = "Median Landscape Modification") +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

p6 = ggplot() +
  geom_sf(data = human, aes(fill = gry_scl), color = "transparent") +
  theme_bw(base_size = 16) + 
  labs(x = "", 
       y = "", 
       color = "", 
       fill = "Median Recreation") +
  scale_fill_viridis_c(end = 0.9, na.value = "white")

plot <- (p1 + p2) / (p3 + p4) / (p5 + p6) + plot_annotation(tag_levels = "A") 

ggsave(plot, file = paste0("figS1.pdf"), path = here::here("analysis", "figs"), height = 15, width = 19)

# Distributions -----------------------------------------------------------

strava <- strava %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

hfp <- hfp %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

bio <- bio %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

rare <- rare %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

# rsvg_png(here::here("icons", "building-regular-full.svg"), file = here::here("icons", "building-regular-full.png"))
# rsvg_png(here::here("icons", "kiwi-bird-solid-full.svg"), file = here::here("icons", "kiwi-bird-solid-full.png"))
# rsvg_png(here::here("icons", "person-running-solid-full.svg"), file = here::here("icons", "person-running-solid-full.png"))
# rsvg_png(here::here("icons", "seedling-solid-full.svg"), file = here::here("icons", "seedling-solid-full.png"))

HFI_png   <- here::here("icons", "building-regular-full.png")
strava_png  <- here::here("icons", "person-running-solid-full.png")
native_png <- here::here("icons", "seedling-solid-full.png")
rare_png   <- here::here("icons", "kiwi-bird-solid-full.png")

p1 <- ggplot(hfp) +
  geom_density_ridges(aes(x = HFP2009, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Landscape Modification ", "<img src='", HFI_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none",
        axis.title.x = element_markdown())

p2 <- ggplot(bio) +
  geom_density_ridges(aes(x = ntv_cnt, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Native Species Count ", "<img src='", native_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none",
        axis.title.x = element_markdown())

p3 <- ggplot(rare) +
  geom_density_ridges(aes(x = rare_count, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Rare Species Count ", "<img src='", rare_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    axis.title.x = element_markdown()
  )

p6 <- ggplot(strava) +
  geom_density_ridges(aes(x = gray_scale, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Recreation ", "<img src='", strava_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_markdown()
  )

strava_combine_sub <- strava %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))

hfp_combine_sub <- hfp %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))

bio_combine_sub <- bio %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))


rare_combine_sub <- rare %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))

p7 <- ggplot(hfp_combine_sub) +
  geom_density_ridges(aes(x = HFP2009, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Landscape Modification ", "<img src='", HFI_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none",
        axis.title.x = element_markdown())

p8 <- ggplot(bio_combine_sub) +
  geom_density_ridges(aes(x = ntv_cnt, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Native Species Count ", "<img src='", native_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none",
        axis.title.x = element_markdown())

p9 <- ggplot(rare_combine_sub) +
  geom_density_ridges(aes(x = rare_count, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Rare Species Count ", "<img src='", rare_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none",
        axis.title.x = element_markdown())

p10 <- ggplot(strava_combine_sub) +
  geom_density_ridges(aes(x = gray_scale, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("Recreation ", "<img src='", strava_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none",
        axis.title.x = element_markdown())

layout <- "
AABF
AACG
AADH
AAEI
"

plot <- p11 + p2 + p3 + p1 + p6 +  p8 + p9 + p7 + p10 + plot_annotation(tag_levels = "A") + plot_layout(design = layout)

ggsave(plot = plot, filename = paste0("fig1.pdf"), path = here::here("analysis", "figs"), height = 15, width = 30, device = cairo_pdf)

# JSD Test ----------------------------------------------------------------

strava_inside <- strava %>%
  filter(loc == "Inside Protected Areas") %>%
  # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
  # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  filter(!is.na(gray_scale)) %>% 
  pull(gray_scale)

strava_outside <- strava %>%
  filter(loc == "Outside Protected Areas") %>%
  filter(!is.na(gray_scale)) %>% 
  pull(gray_scale)

breaks <- seq(min(c(strava_inside,strava_outside)), max(c(strava_inside,strava_outside)), length.out = 100)
p1 <- hist(strava_inside, breaks = breaks, plot = FALSE)$counts / length(strava_inside)
p2 <- hist(strava_outside, breaks = breaks, plot = FALSE)$counts / length(strava_outside)

JSD(rbind(p1, p2), unit = "log2") # 0.1871658 bounded between 0 = same and 1
# GAP 1/2  0.1092982 
# GAP 3/4 0.2204127 

hfp_inside <- hfp %>%
  filter(loc == "Inside Protected Areas") %>% 
  # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
  # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  filter(!is.na(HFP2009)) %>% 
  pull(HFP2009)

hfp_outside <- hfp %>%
  filter(loc == "Outside Protected Areas") %>%
  filter(!is.na(HFP2009)) %>% 
  pull(HFP2009)

breaks <- seq(min(c(hfp_inside,hfp_outside)), max(c(hfp_inside,hfp_outside)), length.out = 100)
p1 <- hist(hfp_inside, breaks = breaks, plot = FALSE)$counts / length(hfp_inside)
p2 <- hist(hfp_outside, breaks = breaks, plot = FALSE)$counts / length(hfp_outside)

JSD(rbind(p1, p2), unit = "log2") # 0.2390168  bounded between 0 = same and 1
# GAP 1/2 0.11747 
# GAP 3/4 0.289343 

bio_inside <- bio %>%
  filter(loc == "Inside Protected Areas") %>% 
  # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
  # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  pull(ntv_cnt)

bio_outside <- bio %>%
  filter(loc == "Outside Protected Areas") %>% 
  pull(ntv_cnt)

breaks <- seq(min(c(bio_inside,bio_outside)), max(c(bio_inside,bio_outside)), length.out = 100)
p1 <- hist(bio_inside, breaks = breaks, plot = FALSE)$counts / length(bio_inside)
p2 <- hist(bio_outside, breaks = breaks, plot = FALSE)$counts / length(bio_outside)

JSD(rbind(p1, p2), unit = "log2") #  0.07686548
# GAP 1/2 0.1081752 
# GAP 3/4 0.07450112 

rare_inside <- rare %>%
  filter(loc == "Inside Protected Areas") %>% 
  # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
  # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  filter(rare_count > 0) %>% 
  pull(rare_count)

rare_outside <- rare %>%
  filter(loc == "Outside Protected Areas") %>%
  filter(rare_count > 0) %>% 
  pull(rare_count)

breaks <- seq(min(c(rare_inside,rare_outside)), max(c(rare_inside,rare_outside)), length.out = 100)
p1 <- hist(rare_inside, breaks = breaks, plot = FALSE)$counts / length(rare_inside)
p2 <- hist(rare_outside, breaks = breaks, plot = FALSE)$counts / length(rare_outside)

JSD(rbind(p1, p2), unit = "log2") # 0.1141226 
# GAP 1/2  0.1284492 
# GAP 3/4   0.1099759 

# ANOVA -------------------------------------------------------------------

strava_inside <- strava %>%
  filter(loc == "Inside Protected Areas") %>%
  filter(!is.na(gray_scale)) 

hfp_inside <- hfp %>%
  filter(loc == "Inside Protected Areas") %>% 
  filter(!is.na(HFP2009))

bio_inside <- bio %>%
  filter(loc == "Inside Protected Areas")

rare_inside <- rare %>%
  filter(loc == "Inside Protected Areas") %>%
  filter(rare_count > 0)

lm1 = lm(gray_scale ~ gap_status, data = strava_inside)
anova(lm1)
aov1 = aov(lm1)
TukeyHSD(aov1)
eta_squared(aov1, partial = TRUE) # gap_status | 0.61 | [0.60, 1.00]

lm2 = lm(HFP2009 ~ gap_status, data = hfp_inside)
anova(lm2)
aov2 = aov(lm2)
TukeyHSD(aov2)
eta_squared(aov2, partial = TRUE) # gap_status | 0.66 | [0.66, 1.00]

lm3 = lm(ntv_cnt ~ gap_status, data = bio_inside)
anova(lm3)
aov3 = aov(lm3)
TukeyHSD(aov3)
eta_squared(aov3, partial = TRUE) # gap_status | 0.29 | [0.28, 1.00]

lm4 = lm(rare_count ~ gap_status, data = rare_inside)
anova(lm4)
aov4 = aov(lm4)
TukeyHSD(aov4)
eta_squared(aov4, partial = TRUE) # gap_status | 0.11 | [0.11, 1.00]

strava_inside2 <- strava %>%
  filter(mng_agncy %in% c("United States National Park Service",
                          "The Nature Conservancy",
                          "United States Forest Service",
                          "California Department of Parks and Recreation",
                          "California Department of Fish and Wildlife")) %>% 
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  )) %>% 
  filter(!is.na(gray_scale)) 

hfp_inside2 <- hfp %>%
  filter(mng_agncy %in% c("United States National Park Service",
                          "The Nature Conservancy",
                          "United States Forest Service",
                          "California Department of Parks and Recreation",
                          "California Department of Fish and Wildlife")) %>% 
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  )) %>% 
  filter(!is.na(HFP2009))

bio_inside2 <- bio %>%
  filter(mng_agncy %in% c("United States National Park Service",
                          "The Nature Conservancy",
                          "United States Forest Service",
                          "California Department of Parks and Recreation",
                          "California Department of Fish and Wildlife")) %>% 
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  ))

rare_inside2 <- rare %>%
  filter(mng_agncy %in% c("United States National Park Service",
                          "The Nature Conservancy",
                          "United States Forest Service",
                          "California Department of Parks and Recreation",
                          "California Department of Fish and Wildlife")) %>% 
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW"
  )) %>% 
  filter(rare_count > 0)

lm5 = lm(gray_scale ~ mng_agncy, data = strava_inside2)
anova(lm5)
aov5 = aov(lm5)
TukeyHSD(aov5)
eta_squared(aov5, partial = TRUE) # mng_agncy | 0.18 | [0.17, 1.00]

lm6 = lm(HFP2009 ~ mng_agncy, data = hfp_inside2)
anova(lm6)
aov6 = aov(lm6)
TukeyHSD(aov6)
eta_squared(aov6, partial = TRUE) #mng_agncy | 0.24 | [0.23, 1.00]

lm7 = lm(ntv_cnt ~ mng_agncy, data = bio_inside2)
anova(lm7)
aov7 = aov(lm7)
TukeyHSD(aov7)
eta_squared(aov7, partial = TRUE) #mng_agncy | 0.44 | [0.43, 1.00]

lm8 = lm(rare_count ~ mng_agncy, data = rare_inside2)
anova(lm8)
aov8 = aov(lm8)
TukeyHSD(aov8)
eta_squared(aov8, partial = TRUE) #mng_agncy | 0.09 | [0.08, 1.00]

# Scatter Plot/PCA ------------------------------------------------------------

cor_check <- human_sum %>%
  select(HFP_median, gry_scl) %>%
  drop_na()

cor(cor_check$HFP_median, cor_check$gry_scl)
#0.7560691

pca_prep_data <- human_sum %>%
  st_drop_geometry() %>%
  select(suid_nma, HFP_median, gry_scl, bio_mdn, rar_mdn, mng_agncy, gap_status) %>% # median_sg,
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
  filter(mng_agncy == "United States National Park Service")

TNC_components <- components %>%
  filter(mng_agncy == "The Nature Conservancy")

UC_components <- components %>%
  filter(mng_agncy == "United States Forest Service")

CDFW_components <- components %>%
  filter(mng_agncy == "California Department of Fish and Wildlife")

SP_components <- components %>%
  filter(mng_agncy == "California Department of Parks and Recreation")

components <- components %>%
  filter(PC2 > -10)

loadings <- as.data.frame(pca["rotation"])

loadings <- loadings %>%
  mutate(var = case_when(
    row.names(loadings) == "gry_scl" ~ "Recreation",
    row.names(loadings) == "HFP_median" ~ "Human Footprint Index",
    row.names(loadings) == "bio_mdn" ~ "Native, Rare Species",
    row.names(loadings) == "rar_mdn" ~ ""
  ))

HFI_icon = rsvg_png(here::here("icons", "building-regular-full.svg"))
strava_icon = rsvg_png(here::here("icons", "person-running-solid-full.svg"))
native_icon = rsvg_png(here::here("icons", "seedling-solid-full.svg"))
rare_icon = rsvg_png(here::here("icons", "kiwi-bird-solid-full.svg"))

readPNG_alpha <- function(png_path, alpha = 1, width_lines = 1.5, height_lines = 1.5) {
  img <- readPNG(png_path)
  
  # If PNG has no alpha channel, add one
  if(dim(img)[3] == 3) {
    alpha_channel <- array(1, dim = c(dim(img)[1], dim(img)[2], 1))
    img <- abind::abind(img, alpha_channel, along = 3)
  }
  
  # Multiply alpha channel by desired alpha
  img[,,4] <- img[,,4] * alpha
  
  # Make grob
  rasterGrob(img, width = unit(width_lines, "lines"), height = unit(height_lines, "lines"))
}

HFI_grob   <- readPNG_alpha(HFI_icon, .8)
strava_grob   <- readPNG_alpha(strava_icon, .8)
native_grob <- readPNG_alpha(native_icon, .8)
rare_grob    <- readPNG_alpha(rare_icon, .8)

rm(HFI_icon, strava_icon, native_icon, rare_icon)

p1 <- ggplot() +
  geom_point(data = components %>% filter(PC2 < 10), aes(PC1, PC2), color = "grey") +
  geom_point(data = CDFW_components, aes(PC1, PC2, color = "California Department of Fish and Wildlife")) +
  geom_point(data = SP_components, aes(PC1, PC2, color = "California Department of Parks and Recreation")) +
  geom_point(data = NPS_components, aes(PC1, PC2, color = "United States National Park Service")) +
  geom_point(data = UC_components, aes(PC1, PC2, color = "United States Forest Service")) +
  geom_point(data = TNC_components, aes(PC1, PC2, color = "The Nature Conservancy")) +
  stat_ellipse(data = CDFW_components, aes(PC1, PC2, color = "California Department of Fish and Wildlife"), level = 0.95, type = "t", alpha = 0.7) +
  stat_ellipse(data = SP_components, aes(PC1, PC2, color = "California Department of Parks and Recreation"), level = 0.95, type = "t", alpha = 0.7) +
  stat_ellipse(data = NPS_components, aes(PC1, PC2, color = "United States National Park Service"), level = 0.95, type = "t", alpha = 0.7) +
  stat_ellipse(data = UC_components, aes(PC1, PC2, color = "United States Forest Service"), level = 0.95, type = "t", alpha = 0.7) +
  stat_ellipse(data = TNC_components, aes(PC1, PC2, color = "The Nature Conservancy"), level = 0.95, type = "t", alpha = 0.7) +
  geom_segment(data = loadings, aes(x = 0, xend = rotation.PC1, y = 0, yend = rotation.PC2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  annotation_custom(strava_grob, xmin = loadings$rotation.PC1[1] - 0.2, xmax = loadings$rotation.PC1[1] - 0.2, ymin = loadings$rotation.PC2[1] + 0.3, ymax = loadings$rotation.PC2[1] + 0.3) + 
  annotation_custom(HFI_grob, xmin = loadings$rotation.PC1[2] - 0.2, xmax = loadings$rotation.PC1[2] - 0.2, ymin = loadings$rotation.PC2[2], ymax = loadings$rotation.PC2[2]) +
  annotation_custom(native_grob, xmin = loadings$rotation.PC1[3] - .2, xmax = loadings$rotation.PC1[3] - 0.2, ymin = loadings$rotation.PC2[3] - 0.2, ymax = loadings$rotation.PC2[3] - 0.2) +
  annotation_custom(rare_grob, xmin = loadings$rotation.PC1[4] + .2, xmax = loadings$rotation.PC1[4] + 0.2, ymin = loadings$rotation.PC2[4] - 0.2, ymax = loadings$rotation.PC2[4] - 0.2) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("PC1 (", round(explained_variance_ratio[1], 2), "%)"),
    y = paste0("PC2 (", round(explained_variance_ratio[2], 2), "%)"),
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

p2 <- ggplot() +
  geom_point(data = components %>% filter(PC2 < 10), aes(PC1, PC2, color = gap_status), alpha = 0.5) +
  stat_ellipse(data = components %>% filter(PC2 < 10), aes(PC1, PC2, color = gap_status), level = 0.95, type = "t", alpha = 0.7) +
  geom_segment(data = loadings, aes(x = 0, xend = rotation.PC1, y = 0, yend = rotation.PC2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  annotation_custom(strava_grob, xmin = loadings$rotation.PC1[1] - 0.2, xmax = loadings$rotation.PC1[1] - 0.2, ymin = loadings$rotation.PC2[1] + 0.3, ymax = loadings$rotation.PC2[1] + 0.3) + 
  annotation_custom(HFI_grob, xmin = loadings$rotation.PC1[2] - 0.2, xmax = loadings$rotation.PC1[2] - 0.2, ymin = loadings$rotation.PC2[2], ymax = loadings$rotation.PC2[2]) +
  annotation_custom(native_grob, xmin = loadings$rotation.PC1[3] - .2, xmax = loadings$rotation.PC1[3] - 0.2, ymin = loadings$rotation.PC2[3] - 0.2, ymax = loadings$rotation.PC2[3] - 0.2) +
  annotation_custom(rare_grob, xmin = loadings$rotation.PC1[4] + .2, xmax = loadings$rotation.PC1[4] + 0.2, ymin = loadings$rotation.PC2[4] - 0.2, ymax = loadings$rotation.PC2[4] - 0.2) +
  theme_bw(base_size = 16) +
  labs(
    x = paste0("PC1 (", round(explained_variance_ratio[1], 2), "%)"),
    y = paste0("PC2 (", round(explained_variance_ratio[2], 2), "%)"),
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = viridis::viridis(5, end = .9)[1:4]) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(p2 + p1 + plot_annotation(tag_levels = "A"), file = paste0("fig2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 25)

# Spatial Analysis --------------------------------------------------------
  
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
  distinct()

length(unique(park_models$suid_nma))

beta_animal <- data.frame()

for (i in 1:length(unique(park_models$suid_nma))) {
  gam1 <- gam(gray_scale ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Landscape Modification and Recreation")
  
  beta_animal <- rbind(beta_animal, coef)
  
  gam1 <- gam(native_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Landscape Modification")
  
  beta_animal <- rbind(beta_animal, coef)
  
  gam1 <- gam(native_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Recreation")
  
  beta_animal <- rbind(beta_animal, coef)
  
  gam1 <- gam(rare_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Landscape Modification")
  
  beta_animal <- rbind(beta_animal, coef)
  
  gam1 <- gam(rare_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 25), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Recreation")
  
  beta_animal <- rbind(beta_animal, coef)
}

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
  distinct() 

length(unique(park_models$suid_nma))

beta_plant <- data.frame()

for (i in 1:length(unique(park_models$suid_nma))) {
  gam1 <- gam(native_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Landscape Modification")
  
  beta_plant <- rbind(beta_plant, coef)
  
  gam1 <- gam(native_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Native Species and Recreation")
  
  beta_plant <- rbind(beta_plant, coef)
  
  gam1 <- gam(rare_count ~ HFP2009 + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Landscape Modification")
  
  beta_plant <- rbind(beta_plant, coef)
  
  gam1 <- gam(rare_count ~ gray_scale + s(x, y, bs = "gp", m = 1, k = 10), data = park_models %>% filter(suid_nma == unique(park_models$suid_nma)[i]))
  
  coef <- data.frame(value = gam1$coefficients[2], suid_nma = unique(park_models$suid_nma)[i], model = "Rare Species and Recreation")
  
  beta_plant <- rbind(beta_plant, coef)
}

beta <- full_join(beta_plant %>% mutate(type = "Plant"), beta_animal %>% mutate(type = "Animal")) %>%
  mutate(type = case_when(
    model == "Landscape Modification and Recreation" ~ NA,
    TRUE ~ type
  ))

beta_avg <- beta %>%
  group_by(type, model) %>%
  summarize(mean_b = mean(value, na.rm = TRUE))

p1 <- ggplot(beta) +
  geom_density_ridges(aes(x = value, y = model, fill = type), alpha = 0.7) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 16) +
  labs(
    x = "Beta",
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  scale_y_discrete(limits = rev)

ggsave(p1, file = paste0("fig3.pdf"), path = here::here("analysis", "figs"), height = 8, width = 12)
