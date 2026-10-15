library(tidyverse)
library(sf)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4269") %>%
  janitor::clean_names() %>%
  st_make_valid()

ca_counties = st_read(here::here("data", "raw_data", "cb_2018_us_county_500k.shp")) %>%
  sf::st_transform("EPSG:4269") %>%
  janitor::clean_names() %>% 
  filter(statefp == "06") %>% 
  st_make_valid()

cpad_counties = st_join(ca_counties, cpad_super) %>% 
  st_drop_geometry() %>% 
  select(geoid) %>% 
  distinct() 

write_csv(cpad_counties, here::here("analysis", "county_geoid.csv"))
