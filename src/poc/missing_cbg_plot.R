library(tidyverse)
library(sf)
library(terra)
library(tidycensus)
library(patchwork)

# census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

sf_use_s2(FALSE)

census_blockgroup <- get_decennial(
  state = "CA",
  geography = "cbg",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>%
  mutate(census_blockgroup_area = units::drop_units(st_area(geometry))) %>%
  select(-variable, -NAME, -value) %>%
  sf::st_transform("EPSG:4326") %>%
  st_buffer(dist = 0) %>%
  mutate(cbg = GEOID) %>%
  mutate(cbg = as.factor(cbg))

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

missing = safegraph %>% 
  group_by(cbg) %>% 
  summarize(value = mean(device_count)) %>% 
  full_join(census_blockgroup) %>% 
  st_as_sf() %>% 
  filter(is.na(value))

not_missing = safegraph %>% 
  group_by(cbg) %>% 
  summarize(value = mean(device_count)) %>% 
  full_join(census_blockgroup) %>% 
  st_as_sf() %>% 
  filter(!is.na(value))
 
p1 = ggplot(data = not_missing) +
  geom_sf(fill = "blue") +
  theme_bw() +
  labs(title = "CBG with safegraph")

p2 = ggplot(data = missing) +
  geom_sf(fill = "red") +
  theme_bw() +
  labs(title = "CBG without safegraph")

plot = p1 + p2

ggsave(plot, file = paste0("census_sum.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
