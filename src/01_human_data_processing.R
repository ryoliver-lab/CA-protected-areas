library(tidyverse)
library(sf)
library(terra)
library(tidycensus)

# census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

ca <- st_read(here::here("data", "raw_data", "CA_State_TIGER2016.shp")) %>%
  sf::st_transform("EPSG:4326")

census_block <- get_decennial(
  state = "CA",
  geography = "block",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>%
  mutate(census_block_area = units::drop_units(st_area(geometry))) %>%
  select(-variable, -NAME) %>%
  sf::st_transform("EPSG:4326") %>%
  st_buffer(dist = 0) %>%
  mutate(cbg = substring(GEOID, 1, 12)) %>%
  mutate(cbg = as.factor(cbg))

block <- census_block %>%
  st_drop_geometry()

census_blockgroup <- get_decennial(
  state = "CA",
  geography = "cbg",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>%
  mutate(census_blockgroup_area = units::drop_units(st_area(geometry))) %>%
  select(-variable, -NAME) %>%
  sf::st_transform("EPSG:4326") %>%
  st_buffer(dist = 0) %>%
  mutate(cbg = GEOID) %>%
  mutate(cbg = as.factor(cbg))

# Human Footprint Index ---------------------------------------------------

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project(crs(cpad_super))

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)
hfp_masked_inv <- mask(hfp_cropped, cpad_super, inverse = TRUE)

rm(hfp_cropped)

cpad_hfp_values <- terra::extract(x = hfp, y = cpad_super)

missing_hfp <- cpad_hfp_values %>%
  group_by(ID) %>%
  mutate(mean_hfp = mean(HFP2009, na.rm = TRUE)) %>%
  filter(is.na(mean_hfp)) %>%
  distinct()

cpad_super_sub <- cpad_super %>%
  filter(ID %in% missing_hfp$ID) %>%
  mutate(ID2 = row_number())

hfp_sub <- crop(hfp, cpad_super_sub)

ggplot() +
  geom_raster(data = hfp_sub %>% as.data.frame(xy = TRUE), aes(x, y, fill = HFP2009), alpha = 0.3) +
  geom_sf(data = cpad_super_sub, fill = "transparent", color = "red") +
  theme_bw() +
  scale_color_viridis_c()

cpad_hfp_values2 <- terra::extract(x = hfp, y = cpad_super_sub) %>%
  rename(ID2 = ID)

cpad_hfp_summary <- cpad_hfp_values %>%
  rename(HFP = HFP2009) %>%
  group_by(ID) %>%
  summarize(across(HFP, list(min = min, mean = mean, max = max, median = median, sd = sd))) %>%
  mutate(HFP_sd = case_when(
    !is.na(HFP_sd) ~ HFP_sd,
    is.na(HFP_sd) ~ 0
  ))

cpad_hfp <- full_join(cpad_super, cpad_hfp_summary) %>%
  mutate(
    HFP_range = HFP_max - HFP_min,
    HFP_cv = HFP_sd / HFP_mean
  ) %>%
  mutate(HFP_sd = case_when(
    !is.na(HFP_sd) ~ HFP_sd,
    is.na(HFP_sd) ~ 0
  )) %>%
  mutate(HFP_cv = case_when(
    !is.na(HFP_cv) ~ HFP_cv,
    is.na(HFP_cv) ~ 0
  ))

inside_hfp <- terra::extract(hfp_masked, cpad_super, xy = TRUE) %>%
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>%
  mutate(loc = "Inside Protected Areas")

outside_hfp <- terra::extract(hfp_masked_inv, ca, xy = TRUE) %>%
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>%
  mutate(loc = "Outside Protected Areas")

hfp <- list(inside_hfp, outside_hfp) %>%
  reduce(rbind) %>%
  st_set_crs("EPSG:4326")

st_write(cpad_hfp, here::here("data", "processed_data", "cpad_hfp.shp"), append = FALSE)
st_write(hfp, here::here("data", "processed_data", "in_out_hfp.shp"), append = FALSE)

rm(hfp_masked, hfp_masked_inv, cpad_hfp_values, cpad_hfp_summary, cpad_hfp)

# Biodiversity ------------------------------------------------------------

bio <- st_read(here::here("data", "raw_data", "ds2739.gdb")) %>%
  sf::st_transform(crs = st_crs(cpad_super)) %>%
  select(NativeCount, RareCount) %>%
  janitor::clean_names()

bio_raster <- rasterize(bio, hfp, field = "native_count")
rare_raster <- rasterize(bio, hfp, field = "rare_count")

rm(hfp, bio)

bio_cropped <- crop(bio_raster, cpad_super)
bio_masked <- mask(bio_cropped, cpad_super)
bio_masked_inv <- mask(bio_cropped, cpad_super, inverse = TRUE)

rare_cropped <- crop(rare_raster, cpad_super)
rare_masked <- mask(rare_cropped, cpad_super)
rare_masked_inv <- mask(rare_cropped, cpad_super, inverse = TRUE)

rm(bio_cropped, bio_raster, rare_cropped, rare_raster)

cpad_bio_values <- terra::extract(x = bio_masked, y = cpad_super, ID = TRUE)
cpad_rare_values <- terra::extract(x = rare_masked, y = cpad_super, ID = TRUE)

cpad_bio_summary <- cpad_bio_values %>%
  rename(bio = native_count) %>% ### CHECK
  group_by(ID) %>%
  summarize(across(bio, list(min = min, mean = mean, max = max, median = median, sd = sd))) %>%
  mutate(bio_sd = case_when(
    !is.na(bio_sd) ~ bio_sd,
    is.na(bio_sd) ~ 0
  ))

cpad_rare_summary <- cpad_rare_values %>%
  rename(rare = rare_count) %>%
  group_by(ID) %>%
  summarize(across(rare, list(min = min, mean = mean, max = max, median = median, sd = sd))) %>%
  mutate(rare_sd = case_when(
    !is.na(rare_sd) ~ rare_sd,
    is.na(rare_sd) ~ 0
  )) %>%
  full_join(cpad_bio_summary)

cpad_bio <- full_join(cpad_super, cpad_rare_summary) %>%
  mutate(
    bio_range = bio_max - bio_min,
    bio_cv = bio_sd / bio_mean
  ) %>%
  mutate(bio_sd = case_when(
    !is.na(bio_sd) ~ bio_sd,
    is.na(bio_sd) ~ 0
  )) %>%
  mutate(bio_cv = case_when(
    !is.na(bio_cv) ~ bio_cv,
    is.na(bio_cv) ~ 0
  )) %>%
  mutate(
    rare_range = rare_max - rare_min,
    rare_cv = rare_sd / rare_mean
  ) %>%
  mutate(rare_sd = case_when(
    !is.na(rare_sd) ~ rare_sd,
    is.na(rare_sd) ~ 0
  )) %>%
  mutate(rare_cv = case_when(
    !is.na(rare_cv) ~ rare_cv,
    is.na(rare_cv) ~ 0
  ))

inside_bio <- terra::extract(bio_masked, cpad_super, xy = TRUE) %>%
  filter(!is.na(native_count)) %>%
  st_as_sf(coords = c("x", "y")) %>%
  mutate(loc = "Inside Protected Areas")

outside_bio <- terra::extract(bio_masked_inv, ca, xy = TRUE) %>%
  filter(!is.na(native_count)) %>%
  st_as_sf(coords = c("x", "y")) %>%
  mutate(loc = "Outside Protected Areas")

inside_rare <- terra::extract(rare_masked, cpad_super, xy = TRUE) %>%
  filter(!is.na(rare_count)) %>%
  st_as_sf(coords = c("x", "y")) %>%
  mutate(loc = "Inside Protected Areas")

outside_rare <- terra::extract(rare_masked_inv, ca, xy = TRUE) %>%
  filter(!is.na(rare_count)) %>%
  st_as_sf(coords = c("x", "y")) %>%
  mutate(loc = "Outside Protected Areas")

bio <- list(inside_bio, outside_bio) %>%
  reduce(rbind) %>%
  st_set_crs("EPSG:4326")

rare <- list(inside_rare, outside_rare) %>%
  reduce(rbind) %>%
  st_set_crs("EPSG:4326")

st_write(cpad_bio, here::here("data", "processed_data", "cpad_bio.shp"), append = FALSE)
st_write(bio, here::here("data", "processed_data", "in_out_bio.shp"), append = FALSE)
st_write(rare, here::here("data", "processed_data", "in_out_rare.shp"), append = FALSE)

rm(bio_masked, bio_masked_inv, cpad_bio_values, cpad_bio_summary, cpad_bio, inside_bio, outside_bio, inside_rare, outside_rare)

# Strava ------------------------------------------------------------------

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project(crs(cpad_super))

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

rm(strava_coarse, strava_coarse_cropped)

cpad_strava_coarse_values <- terra::extract(x = strava_coarse_masked, y = cpad_super)

rm(strava_coarse)

strava_sum <- cpad_strava_coarse_values %>%
  group_by(ID) %>%
  summarize(
    mean_1 = median(strava_coarse_1, na.rm = TRUE),
    mean_2 = median(strava_coarse_2, na.rm = TRUE),
    mean_3 = median(strava_coarse_3, na.rm = TRUE),
    max_1 = max(strava_coarse_1, na.rm = TRUE),
    max_2 = max(strava_coarse_2, na.rm = TRUE),
    max_3 = max(strava_coarse_3, na.rm = TRUE),
    min_1 = min(strava_coarse_1, na.rm = TRUE),
    min_2 = min(strava_coarse_2, na.rm = TRUE),
    min_3 = min(strava_coarse_3, na.rm = TRUE)
  ) %>%
  distinct() %>%
  filter(!is.na(mean_1))

strava_sum <- strava_sum %>%
  mutate(gray_scale = (0.299 * mean_1) + (0.587 * mean_2) + (0.114 * mean_3)) %>%
  mutate(
    hex_mean = rgb(mean_1, mean_2, mean_3, maxColorValue = 255),
    hex_max = rgb(max_1, max_2, max_3, maxColorValue = 255),
    hex_min = rgb(min_1, min_2, min_3, maxColorValue = 255)
  ) %>%
  mutate(
    hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L),
    hex_max_int = strtoi(str_remove(hex_max, "#"), 16L),
    hex_min_int = strtoi(str_remove(hex_min, "#"), 16L)
  ) %>%
  select(ID, hex_mean_int, hex_max_int, hex_min_int, gray_scale)

cpad_strava <- full_join(cpad_super, strava_sum) %>%
  select(-ID) %>%
  st_drop_geometry() %>%
  distinct() %>%
  full_join(cpad_super)

rm(strava_sum)

st_write(cpad_strava, here::here("data", "processed_data", "cpad_strava.shp"), append = FALSE)

# Safegraph ---------------------------------------------------------------

sf_use_s2(FALSE)

census_blockgroup <- census_blockgroup %>%
  select(-GEOID)

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
    mean_count = mean_count * block_percent,
    value = value * block_percent
  ) %>%
  select(-census_blockgroup_area) %>%
  mutate(value = case_when(
    value == 0 ~ 1,
    is.na(value) ~ 1,
    TRUE ~ value,
  )) %>%
  mutate(
    pop_count = mean_count,
    max_pop_count = max_count,
    min_pop_count = min_count
    # pop_count = mean_count / value,
    # max_pop_count = max_count / value,
    # min_pop_count = min_count / value
  ) %>%
  select(-mean_count, -max_count, -min_count) %>%
  distinct() %>%
  # filter(!is.na(pop_count)) %>%
  mutate(cbg = substring(GEOID, 1, 12))

rm(safegraph_block)

cpad_safgraph_census <- full_join(safegraph_block_sum, cpad_census %>%
  st_drop_geometry())

rm(safegraph_block_sum)

safegraph_pa_sum <- cpad_safgraph_census %>%
  mutate(percent_pa = census_block_area / shape_r) %>%
  st_drop_geometry() %>%
  filter(!is.na(year)) %>%
  group_by(suid_nm, year) %>%
  mutate(
    sg = weighted.mean(pop_count, percent_pa, na.rm = TRUE),
    max_sg = weighted.mean(max_pop_count, percent_pa, na.rm = TRUE),
    min_sg = weighted.mean(min_pop_count, percent_pa, na.rm = TRUE),
    outside_pop = weighted.mean(value, percent_pa, na.rm = TRUE)
  ) %>%
  select(
    -GEOID, -pop_count, -min_pop_count, -max_pop_count,
    -census_block_area, -block_percent, -combo_r, -percent_pa, -prcnt__,
    -cbg, -sd_count, -cv_count, -censs_r, -value
  ) %>%
  distinct() %>%
  group_by(suid_nm) %>%
  mutate(mean_sg = mean(sg, na.rm = TRUE))

rm(cpad_safgraph_census)

write_csv(safegraph_pa_sum, here::here("data", "processed_data", "cpad_sg.csv"))

rm(safegraph_pa_sum)

# Outside PA --------------------------------------------------------------
#
# non_pa_blocks = census_blockgroup %>%
#   filter(!(cbg %in% cpad_census$cbg)) %>%
#   right_join(safegraph) %>%
#   select(-date) %>%
#   st_as_sf() %>%
#   st_drop_geometry() %>%
#   group_by(cbg) %>%
#   mutate(
#     mean_count = mean(device_count, na.rm = TRUE)
#   ) %>%
#   summarize(
#     pop_count = mean_count / value
#   ) %>%
#   ungroup() %>%
#   mutate(loc = "Outside Protected Areas")
#
# rm(safegraph, census_blockgroup, block, cpad_census)
#
# write_csv(non_pa_blocks, here::here("data", "processed_data", "outside_sg.csv"))
