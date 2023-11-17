library(tidyverse)
library(sf)
library(data.table)
rm(list = ls())

sf_use_s2(TRUE)

# species list + taxonomy
# provides species' common names and taxa (birds vs. mammals)
species_list <- fread("data/species_list.csv")

# animal location data
# relevant fields:
# individual_id - ID for the animal wearing the tag
# study_id - ID for the dataset (includes multiple individuals)
# taxon_canonical_name - scientific name of species (may include multiple studies)
# lon - longitude
# lat - latitude
# timestamp

d <- fread("data/final_evt_for_map.csv") %>%
  select(individual_id, study_id, taxon_canonical_name, lon, lat, timestamp) %>%
  left_join(., species_list, by = c("taxon_canonical_name" = "scientific_name")) # join with species list

# convert to sf object
d_sf <- st_as_sf(d, coords = c("lon", "lat"), crs = "EPSG:4326")

# read in CPAD data
cpad_buffer <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) %>% 
  filter(suid_nma != 16162) %>% 
  st_buffer(5000) %>% 
  sf::st_transform("EPSG:4326") %>% 
  select(suid_nma, park_name, shape_area, access_typ)

# goal: determine which studies we should prioritize contacting
# to do so...
# 1. find the locations that are in or around protected areas (5 km buffer?)
#     - should probably exclude the BLM property that covers the entire state

d_cpad = st_join(d_sf, cpad_buffer, join = st_intersects)

# 2. map the number of species, individuals, locations in each PA

species_cpad = d_cpad %>% 
  group_by(suid_nma, taxon_canonical_name, common_name) %>% 
  st_combine()

# 3. summarize the number of species, individuals, locations by access type and PA size
# 4. output summary table of studies with data in or around PAs
#     - include species name (scientific and common), number of individuals, and number of locations