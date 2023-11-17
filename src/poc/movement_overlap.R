library(tidyverse)
library(sf)
library(data.table)
library(EnvStats)
library(patchwork)
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

rm(d)

# read in CPAD data
cpad_buffer <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) %>% 
  filter(suid_nma != 16162) %>% #remove BLM property that covers all of CA
  st_buffer(5000) %>% 
  sf::st_transform("EPSG:4326") %>% 
  select(suid_nma, park_name, shape_area, access_typ) %>% 
  mutate(log_size = log10(shape_area)) %>% 
  mutate(size_bin = case_when(
    log_size <= 4 ~ 1,
    log_size > 4 & log_size < 5 ~ 2,
    log_size >= 5 ~ 3
  ))

# goal: determine which studies we should prioritize contacting
# to do so...
# 1. find the locations that are in or around protected areas (5 km buffer?)
#     - should probably exclude the BLM property that covers the entire state

d_intersect = d_sf[cpad_buffer, , op=st_intersects]
d_cpad = st_join(d_intersect, cpad_buffer, left=TRUE)

rm(cpad_buffer)
rm(d_sf)
rm(d_intersect)

# 2. map the number of species, individuals, locations in each PA

species_cpad = d_cpad %>% 
  group_by(suid_nma, taxon_canonical_name, common_name, access_typ, shape_area, size_bin) %>% 
  summarize(geometry = st_combine(geometry))

# 3. summarize the number of species, individuals, locations by access type and PA size

species_access_sum = species_cpad %>% 
  st_drop_geometry() %>% 
  group_by(access_typ, suid_nma) %>% 
  summarize(species_count = n()) 

ind_access_sum = d_cpad %>% 
  st_drop_geometry() %>% 
  group_by(access_typ, suid_nma) %>% 
  summarize(species_count = n())

p1 = ggplot(species_access_sum) +
  geom_boxplot(aes(access_typ, species_count)) +
  theme_bw() +
  labs(x = "Species Count",
       y = "Access Type") +
  stat_n_text(aes(access_typ, species_count))

p2 = ggplot(ind_access_sum) +
  geom_boxplot(aes(access_typ, species_count)) +
  theme_bw() +
  scale_y_log10() +
  labs(x = "Individual Count",
       y = "Access Type") +
  stat_n_text(aes(access_typ, species_count))

plot1 = p1 + p2
plot1 

ggsave(plot1, file = paste0("species_access_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

species_size_sum = species_cpad %>% 
  st_drop_geometry() %>% 
  group_by(suid_nma) %>% 
  summarize(species_count = n(),
            shape_area = shape_area)

ind_size_sum = d_cpad %>% 
  st_drop_geometry() %>% 
  group_by(suid_nma) %>% 
  summarize(species_count = n(),
            shape_area = shape_area) %>% 
  distinct()

p1 = ggplot(species_size_sum) +
  geom_point(aes(species_count, shape_area)) +
  theme_bw() +
  labs(x = "Species Count",
       y = "Size of Protected Area km^2") +
  scale_y_log10()

p2 = ggplot(ind_size_sum) +
  geom_point(aes(species_count, shape_area)) +
  theme_bw() +
  labs(x = "Individual Count",
       y = "Size of Protected Area km^2")+
  scale_y_log10() +
  scale_x_log10()

plot1 = p1 + p2
plot1 

ggsave(plot1, file = paste0("species_size_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# 4. output summary table of studies with data in or around PAs
#     - include species name (scientific and common), number of individuals, and number of locations

sum = d_cpad %>% 
  st_drop_geometry() %>% 
  group_by(suid_nma, taxon_canonical_name, common_name) %>% 
  summarize(ind_loc_count = n())

species = sum %>% 
  group_by(taxon_canonical_name, common_name) %>% 
  summarize(ind_count = sum(ind_loc_count))

locs = sum %>% 
  group_by(suid_nma) %>% 
  summarize(loc_count = sum(ind_loc_count))

sum_final = sum %>% 
  full_join(species) %>% 
  full_join(locs)

write_csv(sum_final, here::here("analysis", "movement_data_summary.csv"))
