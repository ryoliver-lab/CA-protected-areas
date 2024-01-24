library(tidyverse)
library(sf)

sf_use_s2(FALSE)

# read data ---------------------------------------------------------------
cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

cpad_census <- st_read(here::here("data", "processed_data", "cpad_census.shp")) %>% 
  rename(cbg = id)

ca_census <- st_read(here::here("data", "raw_data", "EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb/")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = st_crs(cpad_census)) %>% 
  filter(state_name == "California") %>% 
  select(id) %>% 
  rename(cbg = id)

list_of_files <- list.files(path = here::here("data", "raw_data", "counties-dates-2-10-22-reformatted", "cali"),
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

safegraph <- read_csv(list_of_files, id = "file_name") %>% 
  select(-file_name) %>% 
  mutate(year = lubridate::year(date)) 

safegraph_mean = safegraph  %>% 
  group_by(cbg, year) %>% 
  summarise(mean_count = mean(count))

# combine census and safegraph --------------------------------------------

safegraph_sf = left_join(safegraph_mean, cpad_census) %>% 
  st_as_sf()

safegraph_full_sf = left_join(safegraph, ca_census) %>% 
  st_as_sf()

rm(cpad_census, ca_census, list_of_files, safegraph, safegraph_mean)

# st_write(safegraph_sf, here::here("data", "processed_data", "safegraph.shp"), append = FALSE)
# st_write(safegraph_full_sf, here::here("data", "processed_data", "safegraph_full.shp"), append = FALSE)

yearly_park_count = safegraph_sf %>% 
  group_by(suid_nm, year) %>% 
  summarize(park_count = weighted.mean(mean_count, combo_r, na.rm = TRUE),
            geometry = st_union(geometry)) #average of count weighted by combo_r

p1 = ggplot(yearly_park_count) +
  geom_sf(aes(fill = park_count, color = park_count)) +
  theme_bw(base_size = 20) +
  labs(fill = "Mobile Device Counts",
       color = "Mobile Device Counts") +
  facet_wrap(~year) +
  scale_fill_viridis_c(trans = scales::pseudo_log_trans(sigma = 0.001)) +
  scale_color_viridis_c(trans = scales::pseudo_log_trans(sigma = 0.001))

ggsave(p1, file = paste0("safegraph.pdf"), path = here::here("analysis", "figs"), height = 10, width = 15)

rm(p1)

yearly_wider = yearly_park_count %>% 
  pivot_wider(names_from = "year", values_from = "park_count") %>% 
  rename(year1 = "2019",
         year2 = "2020")

p2 = ggplot(yearly_wider) +
  geom_point(aes(year1, year2)) +
  theme_bw() +
  labs(x = "2019",
       y = "2020") +
  scale_x_log10() +
  scale_y_log10()

ggsave(p2, file = paste0("safegraph_year.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# time series -------------------------------------------------------------

state_mean = safegraph_full_sf %>% 
  st_drop_geometry() %>% 
  group_by(date,year) %>% 
  summarise(mean_count = mean(count, na.rm = TRUE)) %>% 
  mutate(loc= "All California")

census_inside = st_intersection(safegraph_full_sf, cpad_super)%>% 
  st_drop_geometry() %>% 
  group_by(date,year) %>% 
  summarise(mean_count = mean(count, na.rm = TRUE))%>% 
  mutate(loc = "Inside Protected Areas")
census_outside = st_sym_difference(safegraph_full_sf, cpad_super)%>% 
  st_drop_geometry() %>% 
  group_by(date,year) %>% 
  summarise(mean_count = mean(count, na.rm = TRUE))%>% 
  mutate(loc = "Outside Protected Areas")

census_mean = list(state_mean, census_inside) %>% 
  reduce(full_join)

ggplot(data = census_mean) +
  geom_line(aes(date, mean_count, color = loc)) +
  facet_wrap(~year, scales = "free_x") +
  theme_bw() +
  labs(x = "Date", y = "Mobile Device Counts",
       color = "Location") +
  scale_color_viridis_d()
