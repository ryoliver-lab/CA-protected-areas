library(tidyverse)
library(sf)
library(terra)
library(tidycensus)

# census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

ca = st_read(here::here("data", "raw_data", "CA_State_TIGER2016.shp")) %>%
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

ej_screen <- st_read(here::here("data", "raw_data", "EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb/")) %>%
  janitor::clean_names() %>%
  st_transform(crs = st_crs(cpad_super)) %>%
  filter(state_name == "California") %>%
  rename(cbg = id) %>%
  select(cbg, peopcolorpct, lowincpct)

ca_census <- ej_screen %>%
  mutate(census_area = st_area(ca_census))

cpad_census <- st_intersection(ca_census, cpad_super)

# cpad_census = st_read(here::here("data", "processed_data", "cpad_census.shp")) %>%
#   sf::st_transform("EPSG:4326") %>%
#   rename(cbg = id)

rm(ca_census, ej_screen)

# Global Human Modification ----------------------------------------------

ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

ghm_cropped <- crop(ghm, cpad_super)
ghm_masked <- mask(ghm_cropped, cpad_super)

cpad_raster <- rasterize(cpad_super, ghm_masked, field = "suid_nma")

ghm_masked <- resample(ghm_masked, cpad_raster, metho = "cubic")

rm(ghm,ghm_cropped)

cpad_ghm_values <- terra::extract(x = ghm_masked, y = cpad_super)

cpad_ghm_summary <- cpad_ghm_values %>%
  group_by(ID) %>%
  summarize(across(gHM, list(min = min, mean = mean, max = max, median = median, sd = sd)))

cpad_ghm <- full_join(cpad_super, cpad_ghm_summary) %>%
  mutate(
    gHM_range = gHM_max - gHM_min,
    gHM_cv = gHM_sd / gHM_mean
  ) %>%
  mutate(gHM_sd = case_when(
    !is.na(gHM_sd) ~ gHM_sd,
    is.na(gHM_sd) ~ 0
  )) %>%
  mutate(gHM_cv = case_when(
    !is.na(gHM_cv) ~ gHM_cv,
    is.na(gHM_cv) ~ 0
  ))

st_write(cpad_ghm, here::here("data", "processed_data", "cpad_ghm.shp"), append = FALSE)

rm(ghm_masked, cpad_ghm_values, cpad_ghm_summary, cpad_ghm)

# Human Footprint Index ---------------------------------------------------

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

hfp_cropped <- crop(hfp, cpad_super)
hfp_masked <- mask(hfp_cropped, cpad_super)
hfp_masked_inv = mask(hfp_cropped, cpad_super, inverse = TRUE)

hfp_masked <- resample(hfp_masked, cpad_raster, metho = "cubic")
hfp_masked_inv = resample(hfp_masked_inv, cpad_raster, metho = "cubic")

rm(hfp_cropped)

cpad_hfp_values <- terra::extract(x = hfp_masked, y = cpad_super)

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

inside_hfp = terra::extract(hfp_masked, cpad_super, xy=TRUE) %>% 
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

outside_hfp = terra::extract(hfp_masked_inv, ca, xy=TRUE) %>% 
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

hfp = list(inside_hfp, outside_hfp) %>% 
  reduce(rbind) %>% 
  st_set_crs("EPSG:4326")

st_write(cpad_hfp, here::here("data", "processed_data", "cpad_hfp.shp"), append = FALSE)
st_write(hfp, here::here("data", "processed_data", "in_out_hfp.shp"), append = FALSE)

rm(hfp_masked, hfp_masked_inv, cpad_hfp_values, cpad_hfp_summary, cpad_hfp)

# Biodiversity ------------------------------------------------------------

bio = st_read(here::here("data", "raw_data", "ds2739.gdb")) %>% 
  sf::st_transform("EPSG:4326") %>% 
  select(NativeCount, RareCount) %>% 
  janitor::clean_names()

bio_raster <- rasterize(bio, hfp, field = "native_count")
rare_raster <- rasterize(bio, hfp, field = "rare_count")

rm(hfp, bio)

bio_cropped <- crop(bio_raster, cpad_super)
bio_masked <- mask(bio_cropped, cpad_super)
bio_masked_inv = mask(bio_cropped, cpad_super, inverse = TRUE)

rare_cropped <- crop(rare_raster, cpad_super)
rare_masked <- mask(rare_cropped, cpad_super)
rare_masked_inv = mask(rare_cropped, cpad_super, inverse = TRUE)

bio_masked <- resample(bio_masked, cpad_raster, metho = "cubic")
bio_masked_inv = resample(bio_masked_inv, cpad_raster, metho = "cubic")

rare_masked <- resample(rare_masked, cpad_raster, metho = "cubic")
rare_masked_inv = resample(rare_masked_inv, cpad_raster, metho = "cubic")

rm(bio_cropped, bio_raster, rare_cropped, rare_raster)

cpad_bio_values <- terra::extract(x = bio_masked, y = cpad_super, ID=TRUE)
cpad_rare_values <- terra::extract(x = rare_masked, y = cpad_super, ID=TRUE)

cpad_bio_summary <- cpad_bio_values %>%
  rename(bio = native_count) %>% ### CHECK
  group_by(ID) %>%
  summarize(across(bio, list(min = min, mean = mean, max = max, median = median, sd = sd))) %>%
  mutate(bio_sd = case_when(
    !is.na(bio_sd) ~ bio_sd,
    is.na(bio_sd) ~ 0
  )) 

cpad_rare_summary <- cpad_rare_values%>%
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

inside_bio = terra::extract(bio_masked, cpad_super, xy=TRUE) %>% 
  filter(!is.na(native_count)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

outside_bio = terra::extract(bio_masked_inv, ca, xy=TRUE) %>% 
  filter(!is.na(native_count)) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

inside_rare = terra::extract(rare_masked, cpad_super, xy=TRUE) %>% 
  filter(!is.na(rare_count)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

outside_rare = terra::extract(rare_masked_inv, ca, xy=TRUE) %>% 
  filter(!is.na(rare_count)) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

bio = list(inside_bio, outside_bio) %>% 
  reduce(rbind) %>% 
  st_set_crs("EPSG:4326")

rare = list(inside_rare, outside_rare) %>% 
  reduce(rbind) %>% 
  st_set_crs("EPSG:4326")

st_write(cpad_bio, here::here("data", "processed_data", "cpad_bio.shp"), append = FALSE)
st_write(bio, here::here("data", "processed_data", "in_out_bio.shp"), append = FALSE)
st_write(rare, here::here("data", "processed_data", "in_out_rare.shp"), append = FALSE)

rm(bio_masked, bio_masked_inv, cpad_bio_values, cpad_bio_summary, cpad_bio, inside_bio, outside_bio, inside_rare, outside_rare)

# Strava ------------------------------------------------------------------

strava_coarse <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_coarse_cropped <- crop(strava_coarse, cpad_super)
strava_coarse_masked <- mask(strava_coarse_cropped, cpad_super)

rm(strava_coarse, strava_coarse_cropped)

strava_coarse_masked <- resample(strava_coarse_masked, cpad_raster, method = "cubic")

block_id_raster <- rasterize(census_block, strava_coarse_masked, field = "GEOID")
block_id_masked <- mask(block_id_raster, cpad_super)

rm(block_id_raster)

block_size <- cellSize(block_id_masked, unit = "m")
block_size_masked <- mask(block_size, cpad_super)

rm(block_size)

cpad_strava_coarse_values <- terra::extract(x = strava_coarse_masked, y = cpad_super)

rm(strava_coarse)

block_id_cpad <- terra::extract(x = block_id_masked, y = cpad_super)

rm(block_id_masked)

block_area_cpad <- terra::extract(x = block_size_masked, y = cpad_super) %>%
  select(-ID)

rm(block_size_masked)

block_cpad <- cbind(block_id_cpad, block_area_cpad) %>%
  full_join(block) %>%
  filter(!is.na(ID)) %>%
  group_by(ID, GEOID, value) %>%
  summarize(area = sum(area))

rm(block, block_id_cpad, block_area_cpad)

strava_block <- full_join(cpad_strava_coarse_values, block_cpad) %>%
  distinct()

rm(block_cpad, cpad_strava_coarse_values)

strava_block <- strava_block %>%
  filter(!is.na(strava_coarse_1))

strava_block_sum <- strava_block %>%
  mutate(value = case_when(
    value == 0 ~ 1,
    TRUE ~ value,
  )) %>%
  group_by(ID, GEOID, value, area) %>%
  summarize(
    mean_1 = mean(strava_coarse_1, na.rm = TRUE),
    mean_2 = mean(strava_coarse_2, na.rm = TRUE),
    mean_3 = mean(strava_coarse_3, na.rm = TRUE),
    max_1 = max(strava_coarse_1, na.rm = TRUE),
    max_2 = max(strava_coarse_2, na.rm = TRUE),
    max_3 = max(strava_coarse_3, na.rm = TRUE),
    min_1 = min(strava_coarse_1, na.rm = TRUE),
    min_2 = min(strava_coarse_2, na.rm = TRUE),
    min_3 = min(strava_coarse_3, na.rm = TRUE)
  ) %>%
  distinct() %>%
  mutate(
    mean_1 = case_when(
      mean_1 < 0 ~ 0,
      mean_1 > 255 ~ 255,
      TRUE ~ mean_1
    ),
    mean_2 = case_when(
      mean_2 < 0 ~ 0,
      mean_2 > 255 ~ 255,
      TRUE ~ mean_2
    ),
    mean_3 = case_when(
      mean_3 < 0 ~ 0,
      mean_3 > 255 ~ 255,
      TRUE ~ mean_3
    ),
    max_1 = case_when(
      max_1 < 0 ~ 0,
      max_1 > 255 ~ 255,
      TRUE ~ max_1
    ),
    max_2 = case_when(
      max_2 < 0 ~ 0,
      max_2 > 255 ~ 255,
      TRUE ~ max_2
    ),
    max_3 = case_when(
      max_3 < 0 ~ 0,
      max_3 > 255 ~ 255,
      TRUE ~ max_3
    ),
    min_1 = case_when(
      min_1 < 0 ~ 0,
      min_1 > 255 ~ 255,
      TRUE ~ min_1
    ),
    min_2 = case_when(
      min_2 < 0 ~ 0,
      min_2 > 255 ~ 255,
      TRUE ~ min_2
    ),
    min_3 = case_when(
      min_3 < 0 ~ 0,
      min_3 > 255 ~ 255,
      TRUE ~ min_3
    )
  )

rm(strava_block)

strava_block_sum <- strava_block_sum %>%
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
  mutate(
    hex_mean_int_pop = hex_mean_int / value,
    hex_max_int_pop = hex_max_int / value,
    hex_min_int_pop = hex_min_int / value
  ) %>%
  select(GEOID, ID, value, area, hex_mean_int_pop, hex_max_int_pop, hex_min_int_pop)

cpad_strava <- full_join(cpad_super, strava_block_sum) %>%
  mutate(percent_pa = area / shape_area) %>%
  group_by(ID) %>%
  mutate(
    hex_pop_area = weighted.mean(hex_mean_int_pop, percent_pa, na.rm = TRUE),
    max_hex_pop_area = weighted.mean(hex_max_int_pop, percent_pa, na.rm = TRUE),
    min_hex_pop_area = weighted.mean(hex_min_int_pop, percent_pa, na.rm = TRUE)
  ) %>%
  select(-GEOID, -ID, -value, -area, -hex_mean_int_pop, -hex_max_int_pop, -hex_min_int_pop, -percent_pa) %>%
  st_drop_geometry() %>%
  distinct() %>%
  full_join(cpad_super)

rm(strava_block_sum)

st_write(cpad_strava, here::here("data", "processed_data", "cpad_strava.shp"), append = FALSE)

# Safegraph ---------------------------------------------------------------

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
    cv_count = sd_count / mean_count
  ) %>%
  ungroup()

safegraph_block <- safegraph_sum %>%
  left_join(census_block) %>%
  st_as_sf()

# %>%
#   select(-cbg)

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