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

ca_census <- st_read(here::here("data", "raw_data", "EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb/")) %>%
  janitor::clean_names() %>%
  st_transform(crs = st_crs(cpad_super)) %>%
  filter(state_name == "California") %>%
  select(id)

ca_census = ca_census %>%
  mutate(census_area = st_area(ca_census))

cpad_census = st_intersection(ca_census, cpad_super) %>%
  rename(cbg = id)

rm(ca_census)

# cpad_census <- st_read(here::here("data", "processed_data", "cpad_census.shp")) %>%
#   rename(cbg = id)

list_of_files <- list.files(
  path = here::here("data", "raw_data", "safegraph"),
  recursive = TRUE,
  pattern = "\\.txt$",
  full.names = TRUE
)

safegraph <- read_csv(list_of_files, id = "file_name") %>%
  mutate(file_name = str_replace(file_name, "C:/Users/acaug/Documents/School/PhD/CA-protected-areas/data/raw_data/safegraph/", ""))%>% 
  mutate(file_name = str_replace(file_name, "_cbg_daily.txt", "")) %>% 
  mutate(date = as.Date(file_name)) %>% 
  select(-file_name) %>% 
  rename(cbg = cpad_cbg) %>% 
  mutate(cbg = as.factor(cbg))%>% 
  full_join(cpad_census) %>%
  distinct() %>%
  filter(!is.na(device_count)) %>%
  filter(!is.na(suid_nm)) %>%
  st_drop_geometry() %>%
  select(combo_r, shape_r, suid_nm, date, device_count) %>%
  rename(suid_nma = suid_nm)

rm(list_of_files, cpad_census)

safegraph_pa_sum <- safegraph %>%
  mutate(percent_pa = combo_r / shape_r) %>%
  select(-combo_r, -shape_r) %>% 
  group_by(suid_nma, date) %>%
  summarize(
    sg = weighted.mean(device_count, percent_pa, na.rm = TRUE)
  ) %>%
  left_join(cpad_super) %>%
  st_drop_geometry()

rm(safegraph, cpad_super)

write_csv(safegraph_pa_sum, here::here("data", "processed_data", "temporal_analysis.csv"))
