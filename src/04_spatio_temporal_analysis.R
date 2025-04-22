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
library(geosphere)
library(ggfortify)
library(ggpmisc)
library(philentropy)

# coastal is 8 km inland due to sea level rise projections

sf_use_s2(FALSE)

# Data Input --------------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) 

ca = st_read(here::here("data", "raw_data", "CA_State_TIGER2016.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super)) %>% 
  mutate(area = st_area(geometry)) %>% 
  select(area) %>% 
  st_drop_geometry()

sg <- read_csv(here::here("data", "processed_data", "cpad_sg.csv"))
strava <- st_read(here::here("data", "processed_data", "cpad_strava.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super))
hfp <- st_read(here::here("data", "processed_data", "cpad_hfp.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super))
bio <- st_read(here::here("data", "processed_data", "cpad_bio.shp")) %>%
  sf::st_transform(crs = st_crs(cpad_super))

human <- list(sg, hfp, bio, strava) %>%
  reduce(full_join) %>%
  st_as_sf()

rm(sg, hfp, bio, strava)

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
    suid_nma, med_sg, median_sg, gry_scl,
    access_typ, park_name, park_url, mng_ag_id, mng_agncy, mng_ag_lev, mng_ag_typ, agncy_web,
    label_name, gap1_acres, gap2_acres, gap3_acres, gap4_acres, gap_tot_ac, shape_leng, shape_area,
    HFP_min, HFP_mean, HFP_max, HFP_median, HFP_sd, HFP_range, HFP_cv, year, bio_men, bio_mdn,rare_men, rar_mdn
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
  ungroup()

cpad_no_geom = cpad_super %>%
  st_drop_geometry()

strava <- st_read(here::here("data", "processed_data", "in_out_strava.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  st_drop_geometry() %>%
  full_join(cpad_no_geom) %>%
  full_join(human %>% select(suid_nma, gap_status, mng_agncy)) %>%
  filter(!is.na(geometry))

strava_sf <- st_read(here::here("data", "processed_data", "in_out_strava.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas")))

hfp <- st_read(here::here("data", "processed_data", "in_out_hfp.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  st_drop_geometry() %>%
  full_join(cpad_no_geom) %>%
  full_join(human %>% select(suid_nma, gap_status, mng_agncy)) %>%
  filter(!is.na(HFP2009))

hfp_sf <- st_read(here::here("data", "processed_data", "in_out_hfp.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas")))

bio <- st_read(here::here("data", "processed_data", "in_out_bio.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  st_drop_geometry() %>%
  full_join(cpad_no_geom) %>%
  full_join(human %>% select(suid_nma, gap_status, mng_agncy))%>%
  filter(!is.na(ntv_cnt))

bio_sf <- st_read(here::here("data", "processed_data", "in_out_bio.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas")))

rare <- st_read(here::here("data", "processed_data", "in_out_rare.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas"))) %>%
  st_drop_geometry() %>%
  full_join(cpad_no_geom) %>%
  full_join(human %>% select(suid_nma, gap_status, mng_agncy))%>%
  filter(!is.na(rare_count))

rare_sf = st_read(here::here("data", "processed_data", "in_out_rare.shp")) %>%
  mutate(loc = fct_relevel(as.factor(loc), c("Outside Protected Areas", "Inside Protected Areas")))

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  filter(!is.na(hex_int)) %>%
  filter(suid_nma %in% human$suid_nma) %>%
  full_join(human %>% select(suid_nma, mng_agncy)) %>% 
  filter(!is.na(HFP2009))

temporal_data <- read_csv(here::here("data", "processed_data", "temporal_analysis.csv")) %>%
  rowwise() %>%
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4"
  )) %>% 
  select(suid_nma, date, sg, park_name, mng_ag_lev, mng_agncy, gap_status) %>%
  filter(!is.na(sg)) %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(suid_nma %in% human$suid_nma) %>%
  mutate(year = year(date))

human_sum <- human %>%
  st_drop_geometry() %>%
  select(-year, -med_sg) %>%
  distinct()

NPS = human_sum %>% 
  filter(mng_agncy == "United States National Park Service")

TNC = human_sum %>% 
  filter(mng_agncy == "The Nature Conservancy")

UC = human_sum %>% 
  filter(mng_agncy == "United States Forest Service")

BLM = human_sum %>%
  filter(mng_agncy == "California Department of Parks and Recreation")

CDFW = human_sum %>% 
  filter(mng_agncy == "California Department of Fish and Wildlife")

colors = c("United States National Park Service" = "#031D3A",
           "The Nature Conservancy" = "#00ABF5", 
           "United States Forest Service" = "#047658",
           "California Department of Fish and Wildlife" = "#C4AF45",
           "California Department of Parks and Recreation" = "#8A716A")

# Manager Summary ---------------------------------------------------------

## NPS

#area managed
sum(NPS$shape_area) / 1000000

# percent in GAP 1/2
sum(filter(NPS, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(NPS$shape_area) * 100

#median HFP
median(NPS$HFP_median, na.rm=TRUE)

#median safegraph
median(NPS$median_sg, na.rm=TRUE)

#median strava
median(NPS$gry_scl, na.rm=TRUE)

#median native species
median(NPS$bio_mdn, na.rm=TRUE)

#median rare
median(NPS$rar_mdn, na.rm=TRUE)

## TNC

#area managed
sum(TNC$shape_area) / 1000000

# percent in GAP 1/2
sum(filter(TNC, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(TNC$shape_area) * 100

#median HFP
median(TNC$HFP_median, na.rm=TRUE)

#median safegraph
median(TNC$median_sg, na.rm=TRUE)

#median strava
median(TNC$gry_scl, na.rm=TRUE)

#median native species
median(TNC$bio_mdn, na.rm=TRUE)

#median rare
median(TNC$rar_mdn, na.rm=TRUE)

## USFS

#area managed
sum(UC$shape_area) / 1000000

# percent in GAP 1/2
sum(filter(UC, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(UC$shape_area) * 100

#median HFP
median(UC$HFP_median, na.rm=TRUE)

#median safegraph
median(UC$median_sg, na.rm=TRUE)

#median strava
median(UC$gry_scl, na.rm=TRUE)

#median native species
median(UC$bio_mdn, na.rm=TRUE)

#median rare
median(UC$rar_mdn, na.rm=TRUE)


## CDFW

#area managed
sum(CDFW$shape_area) / 1000000

# percent in GAP 1/2
sum(filter(CDFW, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(CDFW$shape_area) * 100

#median HFP
median(CDFW$HFP_median, na.rm=TRUE)

#median safegraph
median(CDFW$median_sg, na.rm=TRUE)

#median strava
median(CDFW$gry_scl, na.rm=TRUE)

#median native species
median(CDFW$bio_mdn, na.rm=TRUE)

#median rare
median(CDFW$rar_mdn, na.rm=TRUE)

## BLM

#area managed
sum(BLM$shape_area) / 1000000

# percent in GAP 1/2
sum(filter(BLM, gap_status %in% c("GAP 1", "GAP 2"))$shape_area) / sum(BLM$shape_area) * 100

#median HFP
median(BLM$HFP_median, na.rm=TRUE)

#median safegraph
median(BLM$median_sg, na.rm=TRUE)

#median strava
median(BLM$gry_scl, na.rm=TRUE)

#median native species
median(BLM$bio_mdn, na.rm=TRUE)

#median rare
median(BLM$rar_mdn, na.rm=TRUE)

# Quartile Analysis -------------------------------------------------------

total = ca %>%  
  mutate(thirty_goal = area %>% units::drop_units() * .3)

strava = strava %>% 
  mutate(strava_quartile = case_when(
    gray_scale < quantile(gray_scale, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
    gray_scale >= quantile(gray_scale, probs = 0.25, na.rm = TRUE) & gray_scale < quantile(gray_scale, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
    gray_scale <= quantile(gray_scale, probs = 0.75, na.rm = TRUE) & gray_scale >= quantile(gray_scale, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
    gray_scale > quantile(gray_scale, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  )) 

hfp = hfp %>% 
  mutate(hfp_quartile = case_when(
    HFP2009 < quantile(HFP2009, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
    HFP2009 >= quantile(HFP2009, probs = 0.25, na.rm = TRUE) & HFP2009 < quantile(HFP2009, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
    HFP2009 <= quantile(HFP2009, probs = 0.75, na.rm = TRUE) & HFP2009 >= quantile(HFP2009, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
    HFP2009 > quantile(HFP2009, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  )) 
  
bio = bio %>% 
  mutate(bio_quartile = case_when(
    ntv_cnt < quantile(ntv_cnt, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
    ntv_cnt >= quantile(ntv_cnt, probs = 0.25, na.rm = TRUE) & ntv_cnt < quantile(ntv_cnt, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
    ntv_cnt <= quantile(ntv_cnt, probs = 0.75, na.rm = TRUE) & ntv_cnt >= quantile(ntv_cnt, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
    ntv_cnt > quantile(ntv_cnt, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  ))

rare = rare %>% 
  mutate(rare_quartile = case_when(
    rare_count < quantile(rare_count, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
    rare_count >= quantile(rare_count, probs = 0.25, na.rm = TRUE) & rare_count < quantile(rare_count, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
    rare_count <= quantile(rare_count, probs = 0.75, na.rm = TRUE) & rare_count >= quantile(rare_count, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
    rare_count > quantile(rare_count, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  ))
  
  # mutate(sg_quartile = case_when(
  #   median_sg < quantile(median_sg, probs = 0.25, na.rm = TRUE) ~ "1st Quartile",
  #   median_sg >= quantile(median_sg, probs = 0.25, na.rm = TRUE) & median_sg < quantile(median_sg, probs = 0.5, na.rm = TRUE) ~ "2nd Quartile",
  #   median_sg <= quantile(median_sg, probs = 0.75, na.rm = TRUE) & median_sg >= quantile(median_sg, probs = 0.5, na.rm = TRUE) ~ "3rd Quartile",
  #   median_sg > quantile(median_sg, probs = 0.75, na.rm = TRUE) ~ "4th Quartile",
  # )) 

### Strava

strava_quartiles = strava %>% 
  filter(gap_status %in% c("GAP 1", "GAP 2")) %>% 
  group_by(strava_quartile) %>% 
  summarize(area = n()) %>% 
  rename(quartile = strava_quartile) %>% 
  mutate(type = "Recreation (Strava)")%>% 
  ungroup() %>% 
  mutate(percent = area / sum(area) * 100)

### HFI

hfi_quartiles = hfp %>% 
  filter(gap_status %in% c("GAP 1", "GAP 2")) %>% 
  group_by(hfp_quartile) %>% 
  summarize(area = n()) %>% 
  rename(quartile = hfp_quartile) %>% 
  mutate(type = "Human Footprint Index")%>% 
  ungroup() %>% 
  mutate(percent = area / sum(area) * 100)

### sg

# sg_quartiles = sg %>% 
#   filter(gap_status %in% c("GAP 1", "GAP 2")) %>% 
#   group_by(sg_quartile) %>% 
#   summarize(area = n()) %>% 
#   select(area, sg_quartile) %>% 
#   rename(quartile = sg_quartile) %>% 
#   mutate(type = "Safegraph Counts") %>% 
#   ungroup()  %>% 
# mutate(percent = area / sum(area) * 100)

### bio

bio_quartile = bio %>% 
  filter(gap_status %in% c("GAP 1", "GAP 2")) %>% 
  group_by(bio_quartile) %>% 
  summarize(area = n()) %>% 
  rename(quartile = bio_quartile) %>% 
  mutate(type = "Native Species Count") %>% 
  ungroup() %>% 
  mutate(percent = area / sum(area) * 100)

### rare

rare_quartile = rare %>% 
  filter(gap_status %in% c("GAP 1", "GAP 2")) %>% 
  select(rare_quartile) %>% 
  group_by(rare_quartile) %>% 
  summarize(area = n()) %>% 
  rename(quartile = rare_quartile) %>% 
  mutate(type = "Rare Species Count") %>% 
  ungroup()   %>% 
  mutate(percent = area / sum(area) * 100)

gap_status = human %>% 
  select(gap_status, park_name) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(gap_status) %>% 
  mutate(area = sum(area)) %>% 
  select(area, gap_status) %>% 
  distinct() %>% 
  mutate(ca_area = total$area) %>% 
  mutate(percent = area / ca_area * 100) %>% 
  rename(quartile = gap_status) %>% 
  mutate(type = "GAP Status") %>% 
  mutate(type = as.factor(type),
         quartile = as.factor(quartile)) %>% 
  mutate(quartile = fct_relevel(quartile, c("GAP 4", "GAP 3", "GAP 2", "GAP 1")))

quartile_data = list(strava_quartiles, hfi_quartiles, bio_quartile, rare_quartile) %>%  #sg_quartiles, 
  reduce(full_join) %>% 
  mutate(type = as.factor(type),
         quartile = as.factor(quartile)) %>% 
  mutate(type = fct_relevel(type, c("Recreation (Strava)", "Human Footprint Index","Rare Species Count", "Native Species Count" ))) %>%  #  "Safegraph Counts", 
  mutate(quartile = fct_relevel(quartile, c("4th Quartile" ,"3rd Quartile", "2nd Quartile" ,"1st Quartile")))

p4 = ggplot(gap_status) +
  geom_bar(aes(area %>% units::drop_units() / 1000000, y = type, fill = quartile), position = "stack", stat = "identity") +
  geom_vline(aes(xintercept = total$thirty_goal / 1000000), color = "red", linetype = "dashed", linewidth = 1) +
  theme_bw(base_size= 16) +
  scale_fill_viridis_d(end = 0.45, na.value = "grey") +
  labs(x = "Area", y = "", fill = "")

p5 = ggplot(quartile_data) +
  geom_bar(aes(percent, y = type, fill = quartile), position = "stack", stat = "identity") +
  theme_bw(base_size= 16) +
  scale_fill_viridis_d(begin = 0.55, end = 0.9, na.value = "grey", direction = -1) +
  labs(x = "Area", y = "", fill = "")

# Distributions -----------------------------------------------------------

strava = strava %>% 
  group_by(suid_nma) %>% 
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4"
  )) %>% 
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

hfp = hfp %>% 
  group_by(suid_nma) %>% 
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4"
  )) %>% 
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

bio = bio %>% 
  group_by(suid_nma) %>%
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4"
  )) %>% 
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

rare = rare %>% 
  group_by(suid_nma) %>%
  mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap_status = case_when(
    gap == gap1_acres ~ "GAP 1",
    gap == gap2_acres ~ "GAP 2",
    gap == gap3_acres ~ "GAP 3",
    gap == gap4_acres ~ "GAP 4"
  )) %>% 
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

p1 <- ggplot(hfp) +
  geom_density_ridges(aes(x = HFP2009, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3)+
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none")

p2 <- ggplot(bio) +
  geom_density_ridges(aes(x = ntv_cnt, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(legend.position = "none")

p3 <- ggplot(rare) +
  geom_density_ridges(aes(x = rare_count, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size= 16) +
  labs(
    x = "Rare Species Count",
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

p6 <- ggplot(strava) +
  geom_density_ridges(aes(x = gray_scale, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size= 16) +
  labs(
    x = "Recreation (Strava Heat Map)",
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = .9, na.value = "#fde725") +
  scale_color_manual(values = c("black", "grey")) +
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

plot = (p2 + p3 + p1 + p6)/ (p5 + p4) + plot_annotation(tag_levels = "A") 

ggsave(plot, file = paste0("fig1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 15)

strava_combine_sub = strava %>% 
  filter(mng_agncy %in% c("United States National Park Service",
                          "The Nature Conservancy", 
                          "United States Forest Service", 
                          "California Department of Fish and Wildlife",
                          "California Department of Parks and Recreation"))

hfp_combine_sub = hfp %>% 
  filter(mng_agncy %in% c("United States National Park Service",
                            "The Nature Conservancy", 
                            "United States Forest Service", 
                            "California Department of Fish and Wildlife",
                          "California Department of Parks and Recreation"))

bio_combine_sub = bio %>% 
  filter(mng_agncy %in% c("United States National Park Service",
                          "The Nature Conservancy", 
                          "United States Forest Service", 
                          "California Department of Fish and Wildlife",
                          "California Department of Parks and Recreation"))


rare_combine_sub = rare %>% 
  filter(mng_agncy %in% c("United States National Park Service",
                          "The Nature Conservancy", 
                          "United States Forest Service", 
                          "California Department of Fish and Wildlife",
                          "California Department of Parks and Recreation"))

p1 <- ggplot(hfp_combine_sub) +
  geom_density_ridges(aes(x = HFP2009, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = c("black", "grey"))

p2 <- ggplot(bio_combine_sub) +
  geom_density_ridges(aes(x = ntv_cnt, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "",
    fill = ""
  ) + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = c("black", "grey"))

p3 <- ggplot(rare_combine_sub) +
  geom_density_ridges(aes(x = rare_count, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size= 16) +
  labs(
    x = "Rare Species Count",
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = c("black", "grey"))

p4 <- ggplot(strava_combine_sub) +
  geom_density_ridges(aes(x = gray_scale, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size= 16) +
  labs(
    x = "Recreation (Strava)",
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = c("black", "grey"))

ggsave(p2 / p3 / p1 / p4 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect"), file = paste0("figS1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 15)

# JSD Test ----------------------------------------------------------------

strava_inside = strava %>% 
  filter(loc == "Inside Protected Areas") %>% 
  filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
  filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  filter(!is.na(gray_scale))

max_count = max(strava$gray_scale, na.rm=TRUE) 
min_count = min(strava$gray_scale, na.rm=TRUE) 

count_num = min_count:max_count

strava_inside_break = hist(strava_inside %>% pull(gray_scale), breaks = max_count)$breaks
strava_inside_count = hist(strava_inside %>% pull(gray_scale), breaks = max_count)$counts

strava_inside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in 0:length(count_num)- 2) {
  
  
  if(strava_inside_break[j] == i) {
    strava_inside_hist[i] = strava_inside_count[j]
    j = j + 1
  }
  
  print(strava_inside_hist[i])
}

strava_outside = strava %>% 
  filter(loc == "Outside Protected Areas") %>% 
  filter(!is.na(gray_scale))

strava_outside_break = hist(strava_outside %>% pull(gray_scale), breaks = max_count)$breaks
strava_outside_count = hist(strava_outside %>% pull(gray_scale), breaks = max_count)$counts

strava_outside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in 0:length(count_num) - 2) {

  
  if(strava_outside_break[j] == i & j < length(strava_outside_break)) {
    strava_outside_hist[i] = strava_outside_count[j]
    j = j + 1
    
  }
  
  # print(strava_outside_hist[i])    
}

strava_inside_hist[is.na(strava_inside_hist)] = 0
strava_outside_hist[is.na(strava_outside_hist)] = 0

strava_wide = rbind(strava_inside_hist, strava_outside_hist)

JSD(strava_wide, est.prob = "empirical") #  0.2268064 bounded between 0 = same and 1
#GAP 1/2  0.1241334 
# GAP 3/4 0.2687929 

hfp_inside = hfp %>% 
  filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
  filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  filter(loc == "Inside Protected Areas") 

max_count = max(hfp$HFP2009, na.rm=TRUE) 
min_count = min(hfp$HFP2009, na.rm=TRUE) 

count_num = min_count:max_count

hfp_inside_break = hist(hfp_inside %>% pull(HFP2009), breaks = max_count)$breaks
hfp_inside_count = hist(hfp_inside %>% pull(HFP2009), breaks = max_count)$counts

hfp_inside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in -1:length(count_num)-2) {
  
  
  if(hfp_inside_break[j] == i) {
    hfp_inside_hist[i] = hfp_inside_count[j]
    j = j + 1
  }
  
  print(hfp_inside_hist[i])
}

hfp_outside = hfp %>% 
  filter(loc == "Outside Protected Areas") %>% 
  filter(!is.na(HFP2009))

hfp_outside_break = hist(hfp_outside %>% pull(HFP2009), breaks = max_count)$breaks
hfp_outside_count = hist(hfp_outside %>% pull(HFP2009), breaks = max_count)$counts

hfp_outside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in -1:length(count_num) - 2) {
  
  
  if(hfp_outside_break[j] == i & j < length(hfp_outside_break)) {
    hfp_outside_hist[i] = hfp_outside_count[j]
    j = j + 1
    
  }
  
  # print(hfp_outside_hist[i])    
}

hfp_inside_hist[is.na(hfp_inside_hist)] = 0
hfp_outside_hist[is.na(hfp_outside_hist)] = 0

hfp_wide = rbind(hfp_inside_hist, hfp_outside_hist)

JSD(hfp_wide, est.prob = "empirical") # 0.2641989 bounded between 0 = same and 1
#GAP 1/2 0.1492382
# GAP 3/4 0.3097443 

max_count = max(bio$ntv_cnt) # 319
min_count = min(bio$ntv_cnt) # 34

count_num = min_count:max_count

bio_inside = bio %>% 
  # filter(gap_status %in% c("GAP 1", "GAP 2")) %>% 
  # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  filter(loc == "Inside Protected Areas") 

bio_inside_break = hist(bio_inside %>% pull(ntv_cnt), breaks = max_count)$breaks
bio_inside_count = hist(bio_inside %>% pull(ntv_cnt), breaks = max_count)$counts

bio_inside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in 1:length(count_num)) {
  
  if(bio_inside_break[j] == i) {
    bio_inside_hist[i] = bio_inside_count[j]
    j = j + 1
    
  }
  
  print(bio_inside_hist[i])    
}

bio_outside = bio %>% 
  filter(loc == "Outside Protected Areas")

bio_outside_break = hist(bio_outside %>% pull(ntv_cnt), breaks = max_count)$breaks
bio_outside_count = hist(bio_outside %>% pull(ntv_cnt), breaks = max_count)$counts

bio_outside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in 1:length(count_num)) {
  
  if(bio_outside_break[j] == i) {
    bio_outside_hist[i] = bio_outside_count[j]
    j = j + 1
    
  }
  
  print(bio_outside_hist[i])    
}

bio_wide = rbind(bio_inside_hist, bio_outside_hist)

JSD(bio_wide, est.prob = "empirical") # 0.08372079 
#GAP 1/2 0.09911446 
# GAP 3/4 0.06522453 

max_count = max(rare$rare_count) 
min_count = min(rare$rare_count) 

count_num = min_count:max_count
count_num =count_num[-1]

rare_inside = rare %>% 
  # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
  # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
  filter(loc == "Inside Protected Areas")  %>% 
  filter(rare_count > 0)

rare_inside_break = hist(rare_inside %>% pull(rare_count), breaks = 50)$breaks
rare_inside_count = hist(rare_inside %>% pull(rare_count), breaks = 50)$counts

rare_inside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in 0:length(count_num)-2) {
  
  if(rare_inside_break[j] == i) {
    rare_inside_hist[i] = rare_inside_count[j]
    j = j + 1
    
  }
  
  print(rare_inside_hist[i])    
}

rare_outside = rare %>% 
  filter(loc == "Outside Protected Areas") %>% 
  filter(rare_count > 0)

rare_outside_break = hist(rare_outside %>% pull(rare_count), breaks = 40)$breaks
rare_outside_count = hist(rare_outside %>% pull(rare_count), breaks = 40)$counts

rare_outside_hist = rep(0, length(count_num))

j = 1 # count for count/break 

for(i in 0:length(count_num)-2) {
  
  if(rare_outside_break[j] == i) {
    rare_outside_hist[i] = rare_outside_count[j]
    
    if(j < length(rare_outside_count))
      j = j + 1
    else {
      j = 1
    }
  }
  
  print(rare_outside_hist[i])
}

rare_outside_hist[is.na(rare_outside_hist)] = 0

rare_wide = rbind(rare_inside_hist, rare_outside_hist)

JSD(rare_wide, est.prob = "empirical") #  0.1061642 
#GAP 1/2  0.1255155 
# GAP 3/4  0.1008245 

# Scatter Plot/PCA ------------------------------------------------------------

cor_check = human_sum %>% 
  select(HFP_median, gry_scl) %>% 
  drop_na()

cor(cor_check$HFP_median, cor_check$gry_scl)
# 0.7612141

strava_sf = strava_sf %>% 
  filter(loc == "Outside Protected Areas") %>% 
  rename(gry_scl = gray_scale) %>% 
  select(gry_scl)
hfp_sf = hfp_sf %>% 
  filter(loc == "Outside Protected Areas") %>% 
  rename(HFP_median = HFP2009) %>% 
  select(HFP_median)
bio_sf = bio_sf %>% 
  filter(loc == "Outside Protected Areas") %>% 
  rename(bio_mdn = ntv_cnt) %>% 
  select(bio_mdn) 
rare_sf = rare_sf %>% 
  filter(loc == "Outside Protected Areas") %>% 
  rename(rar_mdn = rare_count) %>% 
  select(rar_mdn)

outside = list(strava_sf, hfp_sf, bio_sf, rare_sf) %>% 
  reduce(st_join)

pca_prep_data = human_sum %>% 
  st_drop_geometry() %>% 
  select(suid_nma, HFP_median, gry_scl, bio_mdn, rar_mdn, mng_agncy, gap_status) %>%  # median_sg, 
  drop_na() %>% 
  mutate(loc = "Inside Protected Areas")

pca_data1 = pca_prep_data %>% 
  select(-suid_nma, -mng_agncy, -gap_status) %>% 
  rbind(outside)

pca_data = pca_data1 %>% 
  select(-loc)

pca_corr = cor(pca_data)

pca = prcomp(pca_data, scale. = TRUE)
summary(pca)

# autoplot(pca, data = pca_prep_data) 

explained_variance_ratio <- summary(pca)[["importance"]]['Proportion of Variance',]
explained_variance_ratio <- 100 * explained_variance_ratio
components <- pca[["x"]]
components <- data.frame(components)
components <- cbind(components, pca_prep_data$suid_nma)
components <- cbind(components, pca_prep_data$mng_agncy) %>% 
  as.data.frame()
components <- cbind(components, pca_prep_data$gap_status) %>% 
  as.data.frame()

names(components) = c("PC1", "PC2", "PC3", "PC4", 
                      "suid_nma", "mng_agncy", "gap_status")

NPS_components = components %>% 
  filter(mng_agncy == "United States National Park Service")

TNC_components = components %>% 
  filter(mng_agncy == "The Nature Conservancy")

UC_components = components %>% 
  filter(mng_agncy == "United States Forest Service")

CDFW_components = components %>% 
  filter(mng_agncy == "California Department of Fish and Wildlife")

SP_components = components %>% 
  filter(mng_agncy == "California Department of Parks and Recreation")

components = components %>% 
  filter(PC2 > -10)

loadings = as.data.frame(pca["rotation"])

loadings = loadings %>% 
  mutate(var = case_when(
    row.names(loadings) == "gry_scl" ~ "Recreation",
    row.names(loadings) == "HFP_median" ~ "Human Footprint Index",
    row.names(loadings) == "bio_mdn" ~ "Native, Rare Species",
    row.names(loadings) == "rar_mdn" ~ ""
  ))

p1 = ggplot() +
  geom_point(data = components %>% filter(PC2 < 10), aes(PC1, PC2), color = "grey") +
  geom_point(data = CDFW_components, aes(PC1, PC2, color = "California Department of Fish and Wildlife")) +
  geom_point(data = SP_components, aes(PC1, PC2, color = "California Department of Parks and Recreation")) +
  geom_point(data = NPS_components, aes(PC1, PC2, color = "United States National Park Service")) +
  geom_point(data = UC_components, aes(PC1, PC2, color = "United States Forest Service")) +
  geom_point(data = TNC_components, aes(PC1, PC2, color = "The Nature Conservancy")) +
  geom_segment(data = loadings, aes(x = 0, xend = rotation.PC1, y = 0, yend = rotation.PC2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = loadings, aes(x = rotation.PC1, y = rotation.PC2, label = var), fill = "transparent", cex = 3, direction = "both", segment.size = .5) +
  theme_bw(base_size= 16) +
  labs(x = paste0("PC1 (", round(explained_variance_ratio[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_variance_ratio[2], 2), "%)"),
       color = "") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave(p1, file = paste0("fig2.pdf"), path = here::here("analysis", "figs"), height = 8, width = 12)

p2 = ggplot() +
  geom_point(data = components %>% filter(PC2 < 10), aes(PC1, PC2, color = gap_status), alpha = 0.5) +
  # geom_point(data = CDFW_components, aes(PC1, PC2, color = "California Department of Fish and Wildlife")) +
  # geom_point(data = SP_components, aes(PC1, PC2, color = "California Department of Parks and Recreation")) +
  # geom_point(data = NPS_components, aes(PC1, PC2, color = "United States National Park Service")) +
  # geom_point(data = UC_components, aes(PC1, PC2, color = "United States Forest Service")) +
  # geom_point(data = TNC_components, aes(PC1, PC2, color = "The Nature Conservancy")) +
  geom_segment(data = loadings, aes(x = 0, xend = rotation.PC1, y = 0, yend = rotation.PC2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = loadings, aes(x = rotation.PC1, y = rotation.PC2, label = var), fill = "transparent", cex = 3, direction = "both", segment.size = .5) +
  theme_bw(base_size= 16) +
  labs(x = paste0("PC1 (", round(explained_variance_ratio[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_variance_ratio[2], 2), "%)"),
       color = "") +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(end = 0.9) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave(p2, file = paste0("figS2.pdf"), path = here::here("analysis", "figs"), height = 8, width = 12)

# Spatial Analysis --------------------------------------------------------

park_models <- spatial_data %>%
  select(gray_scale, suid_nma, HFP2009, y, x, mng_agncy, native_count, rare_count) %>%
  na.omit() %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(count >= 3)

park_var = park_models %>% 
  group_by(suid_nma) %>% 
  summarize(hfp = sd(HFP2009, na.rm=TRUE),
            native_count = sd(native_count, na.rm=TRUE),
            rare_count = sd(rare_count, na.rm=TRUE),
            strava = sd(gray_scale, na.rm=TRUE)) %>% 
  group_by(suid_nma) %>% 
  mutate(total_var = sum(hfp, native_count, rare_count, strava)) %>% 
  filter(total_var > 0)

park_models = park_models %>%
  filter(suid_nma %in% park_var$suid_nma) %>% 
  group_by(suid_nma) %>% 
  mutate(mean_hfp = median(HFP2009, na.rm=TRUE),
         mean_native_count = median(native_count, na.rm=TRUE),
         mean_rare_count = median(rare_count, na.rm=TRUE),
         mean_strava = median(gray_scale, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(HFP2009 = HFP2009 - mean_hfp,
         native_count = native_count - mean_native_count,
         rare_count = rare_count - mean_rare_count,
         gray_scale = gray_scale - mean_strava)

gam1 <- gam(gray_scale ~ s(HFP2009, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models)
gam2 <- gam(gray_scale ~ s(HFP2009, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(mng_agncy == "United States National Park Service"))
gam3 <- gam(gray_scale ~ s(HFP2009, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(mng_agncy == "The Nature Conservancy"))
gam4 <- gam(gray_scale ~ s(HFP2009, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(mng_agncy == "United States Forest Service"))
gam5 <- gam(gray_scale ~ s(HFP2009, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"))
gam6 <- gam(gray_scale ~ s(HFP2009, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, HFP2009, bs = "re"), random = list(suid_nma = ~ 1 + HFP2009), data = park_models %>% filter(mng_agncy == "California Department of Parks and Recreation"))

summary(gam1)
# summary(gam2)
# summary(gam3)
# summary(gam4)
# summary(gam5)
# summary(gam6)

p1 <- plot_predictions(gam1, condition = "HFP2009") +
  geom_rug(data = park_models, aes(HFP2009), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

p2 <- plot_predictions(gam2, condition = "HFP2009") +
  geom_rug(data = park_models %>% filter(mng_agncy == "United States National Park Service"), aes(HFP2009), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p3 <- plot_predictions(gam3, condition = "HFP2009") +
  geom_rug(data = park_models %>% filter(mng_agncy == "The Nature Conservancy"), aes(HFP2009), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p4 <- plot_predictions(gam4, condition = "HFP2009") +
  geom_rug(data = park_models %>% filter(mng_agncy == "United States Forest Service"), aes(HFP2009), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p5 <- plot_predictions(gam5, condition = "HFP2009") +
  geom_rug(data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"), aes(HFP2009), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p6 <- plot_predictions(gam6, condition = "HFP2009") +
  geom_rug(data = park_models %>% filter(mng_agncy == "California Department of Parks and Recreation"), aes(HFP2009), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Human Footprint Index",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

gam1 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models)
gam2 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "United States National Park Service"))
gam3 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "The Nature Conservancy"))
gam4 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "United States Forest Service"))
gam5 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"))
gam6 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "California Department of Parks and Recreation"))

summary(gam1)
# summary(gam2)
# summary(gam3)
# summary(gam4)
# summary(gam5)

p7 <- plot_predictions(gam1, condition = "native_count") +
  geom_rug(data = park_models, aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

p8 <- plot_predictions(gam2, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "United States National Park Service"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p9 <- plot_predictions(gam3, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "The Nature Conservancy"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
)

p10 <- plot_predictions(gam4, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "United States Forest Service"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p11 <- plot_predictions(gam5, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p12 <- plot_predictions(gam6, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

gam1 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models)
gam2 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "United States National Park Service"))
gam3 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "The Nature Conservancy"))
gam4 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "United States Forest Service"))
gam5 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"))
gam6 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models %>% filter(mng_agncy == "California Department of Parks and Recreation"))

summary(gam1)
# summary(gam2)
# summary(gam3)
# summary(gam4)
# summary(gam5)

p13 <- plot_predictions(gam1, condition = "native_count") +
  geom_rug(data = park_models, aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

p14 <- plot_predictions(gam2, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "United States National Park Service"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p15 <- plot_predictions(gam3, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "The Nature Conservancy"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p16 <- plot_predictions(gam4, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "United States Forest Service"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p17 <- plot_predictions(gam5, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

p18 <- plot_predictions(gam6, condition = "native_count") +
  geom_rug(data = park_models %>% filter(mng_agncy == "California Department of Fish and Wildlife"), aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.x = element_blank()
  )

plot <- p13 / p7 / p1 + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig3.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

plot <- (p2 + p4) / (p3 + p5 + p6) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("figS7.pdf"), path = here::here("analysis", "figs"), height = 15, width = 25)

plot <- (p8 + p10) / (p9 + p11 + p12) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("figS6.pdf"), path = here::here("analysis", "figs"), height = 15, width = 25)

plot <- (p14 + p16) / (p15 + p17 + p18) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("figS5.pdf"), path = here::here("analysis", "figs"), height = 15, width = 25)

# animals only
park_models <- spatial_data %>%
  select(gray_scale, suid_nma, HFP2009, y, x, mng_agncy, ntv_anm, rare_anm) %>%
  na.omit() %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(count >= 3) %>%
  rename(native_count = ntv_anm,
         rare_count = rare_anm)

park_var = park_models %>% 
  group_by(suid_nma) %>% 
  summarize(hfp = sd(HFP2009, na.rm=TRUE),
            native_count = sd(native_count, na.rm=TRUE),
            rare_count = sd(rare_count, na.rm=TRUE),
            strava = sd(gray_scale, na.rm=TRUE)) %>% 
  group_by(suid_nma) %>% 
  mutate(total_var = sum(hfp, native_count, rare_count, strava)) %>% 
  filter(total_var > 0)

park_models = park_models %>%
  filter(suid_nma %in% park_var$suid_nma) %>% 
  group_by(suid_nma) %>% 
  mutate(mean_hfp = median(HFP2009, na.rm=TRUE),
         mean_native_count = median(native_count, na.rm=TRUE),
         mean_rare_count = median(rare_count, na.rm=TRUE),
         mean_strava = median(gray_scale, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(HFP2009 = HFP2009 - mean_hfp,
         native_count = native_count - mean_native_count,
         rare_count = rare_count - mean_rare_count,
         gray_scale = gray_scale - mean_strava)


gam1 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models)

p7 <- plot_predictions(gam1, condition = "native_count") +
  geom_rug(data = park_models, aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

gam1 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models)

p13 <- plot_predictions(gam1, condition = "native_count") +
  geom_rug(data = park_models, aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

plot <- p13 / p7 + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("figS3.pdf"), path = here::here("analysis", "figs"), height = 10, width = 15)

#plants only
park_models <- spatial_data %>%
  select(gray_scale, suid_nma, HFP2009, y, x, mng_agncy, ntv_plnt, rar_plnt) %>%
  na.omit() %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(count >= 3) %>%
  rename(native_count = ntv_plnt,
         rare_count = rar_plnt)

park_var = park_models %>% 
  group_by(suid_nma) %>% 
  summarize(hfp = sd(HFP2009, na.rm=TRUE),
            native_count = sd(native_count, na.rm=TRUE),
            rare_count = sd(rare_count, na.rm=TRUE),
            strava = sd(gray_scale, na.rm=TRUE)) %>% 
  group_by(suid_nma) %>% 
  mutate(total_var = sum(hfp, native_count, rare_count, strava)) %>% 
  filter(total_var > 0)

park_models = park_models %>%
  filter(suid_nma %in% park_var$suid_nma) %>% 
  group_by(suid_nma) %>% 
  mutate(mean_hfp = median(HFP2009, na.rm=TRUE),
         mean_native_count = median(native_count, na.rm=TRUE),
         mean_rare_count = median(rare_count, na.rm=TRUE),
         mean_strava = median(gray_scale, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(HFP2009 = HFP2009 - mean_hfp,
         native_count = native_count - mean_native_count,
         rare_count = rare_count - mean_rare_count,
         gray_scale = gray_scale - mean_strava)

gam1 <- gam(gray_scale ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models)

p7 <- plot_predictions(gam1, condition = "native_count") +
  geom_rug(data = park_models, aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Strava Gray Scale"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

gam1 <- gam(HFP2009 ~ s(native_count, k = 3) + s(y, x, bs = "gp", m = 1, k = 3) + s(suid_nma, bs = "re") + s(suid_nma, native_count, bs = "re"), random = list(suid_nma = ~ 1 + native_count), data = park_models)

p13 <- plot_predictions(gam1, condition = "native_count") +
  geom_rug(data = park_models, aes(native_count), alpha = 1 / 2, outside = TRUE, sides = "b") +
  theme_bw(base_size= 16) +
  labs(
    x = "Native Species Count",
    y = "Human Footprint Index"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank()
  )

plot <- p13 / p7 + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("figS4.pdf"), path = here::here("analysis", "figs"), height = 10, width = 15)

# Temporal Analysis -------------------------------------------------------

temporal_data <- temporal_data %>%
  mutate(date_j = yday(date)) %>%
  mutate(date_j = case_when(
    year == 2020 ~ date_j + 365,
    year == 2021 ~ date_j + 731,
    year == 2019 ~ date_j
  )) %>% 
  mutate(month = month(date),
         day = mday(date)) %>% 
  mutate(day_plus_month = day + month * 100) %>%
  mutate(season = case_when(
    day_plus_month %in% 320:620 ~ 'Spring',
    day_plus_month %in% 621:922 ~ 'Summer',
    day_plus_month %in% 923:1221 ~ 'Fall',
    TRUE ~ 'Winter')) %>% 
  select(-month, -day, -day_plus_month) %>% 
  mutate(season = fct_relevel(season, "Winter", "Spring", "Summer", "Fall"))

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
  mutate(date_count = n()) %>% 
  ungroup() %>%
  select(date, bin, date_count) %>% 
  distinct()

temp_date2 <- temporal_bins %>%
  group_by(date_j, bin, mng_ag_lev) %>%
  mutate(date_count = n()) %>% 
  ungroup() %>%
  select(date, bin, date_count, mng_ag_lev) %>% 
  distinct()

temp_date3 <- temporal_bins %>%
  group_by(date_j, bin, mng_agncy) %>%
  mutate(date_count = n()) %>% 
  ungroup() %>%
  select(date, bin, date_count, mng_agncy) %>% 
  distinct()

temp_date4 <- temporal_bins %>%
  group_by(date_j, bin, gap_status) %>%
  mutate(date_count = n()) %>% 
  ungroup() %>%
  select(date, bin, date_count, gap_status) %>% 
  distinct()

p1 <- ggplot(temp_date) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c() + # trans = scales::pseudo_log_trans(sigma = 0.001)
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Tourism Quartiles",
    fill = "Count"
  ) +
  scale_x_date()

seasons = temporal_bins %>% 
  filter(year == 2019) %>% 
  group_by(season, suid_nma) %>% 
  summarize(season_med = median(sg)) %>% 
  group_by(suid_nma) %>% 
  slice_max(order_by = season_med, n = 1) 

season_sum = seasons %>% 
  group_by(season) %>% 
  mutate(season_count = n()) %>% 
  select(season_count) %>% 
  distinct()

p2 = ggplot(data = season_sum) +
  geom_bar(aes(x = (season_count / sum(season_sum$season_count) * 100), y = "", fill = season), position = "stack", stat = "identity") +
  theme_bw(base_size= 16) +
  labs(x = "Percent Parks with Peak Usage in a Season",
       y = "",
       fill = "") +
  scale_fill_viridis_d(end = 0.9)

temp_med_mad = temporal_data %>% 
  group_by(suid_nma, mng_agncy, park_name) %>% 
  summarize(med_sg = median(sg), 
         mad_sg = mad(sg)) %>% 
  full_join(seasons)

p3 = ggplot() +
  geom_point(data = temp_med_mad %>% filter(med_sg < 1000), aes(med_sg, mad_sg, color = season)) +
  # geom_point(data = CDFW_med_mad, aes(med_sg, mad_sg, color = "California Department of Fish and Wildlife")) +
  # geom_point(data = NPS_med_mad, aes(med_sg, mad_sg, color = "United States National Park Service")) +
  # geom_point(data = TNC_med_mad, aes(med_sg, mad_sg, color = "The Nature Conservancy")) +
  # geom_point(data = UC_med_mad, aes(med_sg, mad_sg, color = "United States Forest Service")) +
  theme_bw(base_size= 16) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(end = 0.9) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  labs(x = "Median Safegraph",
       y = "Median Absolute Deviance",
       color = "")


plot <- p1 / (p2 + p3) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig4.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

p1 = ggplot(temp_date2 %>% filter(mng_ag_lev == "City")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "City"
  ) +
  scale_x_date()

p2 = ggplot(temp_date2 %>% filter(mng_ag_lev == "County")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "County"
    
  ) +
  scale_x_date()

p3 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Federal")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "Federal"
  ) +
  scale_x_date()

p4 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Home Owners Association")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "Home Owners Association"
  ) +
  scale_x_date()

p5 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Joint")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "Joint"
  ) +
  scale_x_date()

p6 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Non Profit")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "Non Profit"
  ) +
  scale_x_date()

p7 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Private")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "Private"
  ) +
  scale_x_date()

p8 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Special District")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "Special District"
  ) +
  scale_x_date()

p9 = ggplot(temp_date2 %>% filter(mng_ag_lev == "State")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "State"
  ) +
  scale_x_date()

p = (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9)

ggsave(p, file = paste0("figS8.pdf"), path = here::here("analysis", "figs"), height = 15, width = 20)

p1 = ggplot(temp_date3 %>% filter(mng_agncy == "United States National Park Service")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "National Park Service"
  ) +
  scale_x_date()

p2 = ggplot(temp_date3 %>% filter(mng_agncy == "The Nature Conservancy")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "The Nature Conservancy"
    
  ) +
  scale_x_date()

p3 = ggplot(temp_date3 %>% filter(mng_agncy == "California Department of Fish and Wildlife")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "California Fish and Wildlife"
  ) +
  scale_x_date()

p4 = ggplot(temp_date3 %>% filter(mng_agncy == "United States Forest Service")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "United States Forest Service"
  ) +
  scale_x_date()

p5 = ggplot(temp_date3 %>% filter(mng_agncy == "California Department of Parks and Recreation")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "California Parks and Rec"
  ) +
  scale_x_date()

p = (p1 + p2) /(p3 + p4 + p5)

ggsave(p, file = paste0("figS9.pdf"), path = here::here("analysis", "figs"), height = 15, width = 20)

p1 = ggplot(temp_date4 %>% filter(gap_status == "GAP 1")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "GAP 1"
  ) +
  scale_x_date()

p2 = ggplot(temp_date4 %>% filter(gap_status == "GAP 2")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "GAP 2"
    
  ) +
  scale_x_date()

p3 = ggplot(temp_date4 %>% filter(gap_status == "GAP 3")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "GAP 3"
  ) +
  scale_x_date()

p4 = ggplot(temp_date4 %>% filter(gap_status == "GAP 4")) +
  geom_tile(aes(date, bin, fill = date_count)) +
  geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
  geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
  scale_fill_viridis_c(na.value = "#440154") +
  theme_bw(base_size= 16) +
  labs(
    x = "Date",
    y = "Safegraph Quartiles",
    fill = "Count",
    title = "GAP 4"
  ) +
  scale_x_date()

p = (p1 + p2) /(p3 + p4)

ggsave(p, file = paste0("figS10.pdf"), path = here::here("analysis", "figs"), height = 15, width = 20)
