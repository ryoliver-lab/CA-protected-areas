# Safegraph ---------------------------------------------------------------

# cpad_census = st_intersection(census_blockgroup, cpad_super)

cpad_census = st_read(here::here("data", "processed_data", "cpad_census.shp")) %>% 
  mutate(cbg = id)

list_of_files <- list.files(
  path = here::here("data", "raw_data", "safegraph"),
  recursive = TRUE,
  pattern = "\\.txt$",
  full.names = TRUE
)

safegraph <- read_csv(list_of_files, id = "file_name") %>%
  mutate(file_name = str_replace(file_name, "C:/Users/acaug/Documents/School/PhD/CA-protected-areas/data/raw_data/safegraph/", "")) %>%
  mutate(file_name = str_replace(file_name, "_cbg_daily.txt", "")) %>%
  mutate(date = as.Date(file_name)) %>%
  select(-file_name) %>%
  rename(cbg = cpad_cbg) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(!is.na(year)) %>%
  mutate(cbg = as.factor(cbg))

rm(list_of_files)

### time series processing starting with safegraph

safegraph_sum <- safegraph %>%
  left_join(census_blockgroup) %>%
  select(-value) %>%
  select(-date) %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  group_by(cbg, census_blockgroup_area, year) %>%
  summarise(
    mean_count = mean(device_count, na.rm = TRUE),
    sd_count = sd(device_count, na.rm = TRUE),
    max_count = max(device_count, na.rm = TRUE),
    min_count = min(device_count, na.rm = TRUE),
    cv_count = sd_count / mean_count,
    median_count = median(device_count, na.rm = TRUE)
  ) %>%
  ungroup()

rm(census_blockgroup, safegraph)

safegraph_block <- safegraph_sum %>%
  left_join(census_block) %>%
  st_as_sf()

rm(safegraph_sum, census_block)

safegraph_block_sum <- safegraph_block %>%
  mutate(block_percent = census_block_area / census_blockgroup_area) %>%
  mutate(block_percent = case_when(
    is.infinite(block_percent) ~ 100,
    TRUE ~ block_percent
  )) %>%
  mutate(
    pop_count = mean_count * block_percent,
    median_count = median_count * block_percent
  ) %>%
  select(-census_blockgroup_area) %>%
  mutate(
    max_pop_count = max_count,
    min_pop_count = min_count
  ) %>%
  select(-mean_count, -max_count, -min_count) %>%
  distinct() %>%
  mutate(cbg = substring(GEOID, 1, 12))

rm(safegraph_block, block)

cpad_safgraph <- full_join(safegraph_block_sum, cpad_census %>%
                             st_drop_geometry())

rm(safegraph_block_sum, cpad_census)

safegraph_pa_sum <- cpad_safgraph %>%
  mutate(percent_pa = census_block_area / shape_r)%>%

    st_drop_geometry() %>%
  filter(!is.na(year)) %>%
  group_by(suid_nm, year) %>%
  mutate(
    sg = weighted.mean(pop_count, percent_pa, na.rm = TRUE),
    max_sg = weighted.mean(max_pop_count, percent_pa, na.rm = TRUE),
    min_sg = weighted.mean(min_pop_count, percent_pa, na.rm = TRUE),
    outside_pop = weighted.mean(value, percent_pa, na.rm = TRUE),
    med_sg = spatstat.geom::weighted.median(median_count, percent_pa, na.rm = TRUE)
  ) %>%
  select(
    -GEOID, -pop_count, -min_pop_count, -max_pop_count,
    -census_block_area, -block_percent, -combo_r, -percent_pa, -prcnt__,
    -cbg, -sd_count, -cv_count, -censs_r, -value, -id, -median_count
  ) %>%
  distinct() %>%
  group_by(suid_nm) %>%
  mutate(mean_sg = mean(sg, na.rm = TRUE),
         median_sg = median(med_sg, na.rm=TRUE))

rm(cpad_safgraph)

write_csv(safegraph_pa_sum, here::here("data", "processed_data", "cpad_sg.csv"))

rm(safegraph_pa_sum)


# # JSD Test ----------------------------------------------------------------
# 
# strava_inside <- strava %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   filter(!is.na(gray_scale)) %>%
#   pull(gray_scale)
# 
# strava_outside <- strava %>%
#   filter(loc == "Outside Protected Areas") %>%
#   filter(!is.na(gray_scale)) %>%
#   pull(gray_scale)
# 
# breaks <- seq(min(c(strava_inside, strava_outside)), max(c(strava_inside, strava_outside)), length.out = 100)
# p1 <- hist(strava_inside, breaks = breaks, plot = FALSE)$counts / length(strava_inside)
# p2 <- hist(strava_outside, breaks = breaks, plot = FALSE)$counts / length(strava_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") # 0.1871658 bounded between 0 = same and 1
# # GAP 1/2  0.1092982
# # GAP 3/4 0.2204127
# 
# hfp_inside <- hfp %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   filter(!is.na(HFP2009)) %>%
#   pull(HFP2009)
# 
# hfp_outside <- hfp %>%
#   filter(loc == "Outside Protected Areas") %>%
#   filter(!is.na(HFP2009)) %>%
#   pull(HFP2009)
# 
# breaks <- seq(min(c(hfp_inside, hfp_outside)), max(c(hfp_inside, hfp_outside)), length.out = 100)
# p1 <- hist(hfp_inside, breaks = breaks, plot = FALSE)$counts / length(hfp_inside)
# p2 <- hist(hfp_outside, breaks = breaks, plot = FALSE)$counts / length(hfp_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") # 0.2390168  bounded between 0 = same and 1
# # GAP 1/2 0.11747
# # GAP 3/4 0.289343
# 
# bio_inside <- bio %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   pull(ntv_cnt)
# 
# bio_outside <- bio %>%
#   filter(loc == "Outside Protected Areas") %>%
#   pull(ntv_cnt)
# 
# breaks <- seq(min(c(bio_inside, bio_outside)), max(c(bio_inside, bio_outside)), length.out = 100)
# p1 <- hist(bio_inside, breaks = breaks, plot = FALSE)$counts / length(bio_inside)
# p2 <- hist(bio_outside, breaks = breaks, plot = FALSE)$counts / length(bio_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") #  0.07686548
# # GAP 1/2 0.1081752
# # GAP 3/4 0.07450112
# 
# rare_inside <- rare %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   filter(rare_count > 0) %>%
#   pull(rare_count)
# 
# rare_outside <- rare %>%
#   filter(loc == "Outside Protected Areas") %>%
#   filter(rare_count > 0) %>%
#   pull(rare_count)
# 
# breaks <- seq(min(c(rare_inside, rare_outside)), max(c(rare_inside, rare_outside)), length.out = 100)
# p1 <- hist(rare_inside, breaks = breaks, plot = FALSE)$counts / length(rare_inside)
# p2 <- hist(rare_outside, breaks = breaks, plot = FALSE)$counts / length(rare_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") # 0.1141226
# # GAP 1/2  0.1284492
# # GAP 3/4   0.1099759

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


