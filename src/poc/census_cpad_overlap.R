# set up ------------------------------------------------------------------
library(tidyverse)
library(sf)
library(units)
library(patchwork)
library(EnvStats)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  janitor::clean_names() %>%
  mutate(ID = row_number())

ca_census <- st_read(here::here("data", "raw_data", "EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb/")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = st_crs(cpad_super)) %>% 
  filter(state_name == "California") %>% 
  select(id)

ca_census = ca_census %>% 
  mutate(census_area = st_area(ca_census))

# Num areas by size -------------------------------------------------------

cpad_census = st_intersection(ca_census, cpad_super)

p = ggplot(cpad_super) +
  geom_histogram(aes(log10(shape_area)), bins = 10) +
  theme_bw()
p

hist_dat = ggplot_build(p)$data[[1]]

# bin 1: 0 to log(3.5)
# bin 2: log(3.5) to log(4)
# bin 3: log(4) to max

cpad_census = cpad_census %>% 
  mutate(log_size = log10(shape_area)) %>% 
  mutate(size_bin = case_when(
    log_size <= 4 ~ 1,
    log_size > 4 & log_size < 5 ~ 2,
    log_size >= 5 ~ 3
  ))

size_summary = cpad_census %>% 
  st_drop_geometry() %>% 
  group_by(suid_nma, size_bin, access_typ) %>% 
  summarize(count = n()) 

p1 = ggplot(size_summary) +
  geom_boxplot(aes(as.factor(size_bin), log(count), fill = access_typ)) +
  theme_bw() +
  labs(x = "PA Size Class", 
       y = "Log10 of Count",
       fill = "Access Type") +
  stat_n_text(aes(as.factor(size_bin), log(count))) +
  scale_x_discrete(breaks = c("1", "2", "3"), labels = c("0 to 10 km^2", "10 to 100 km^2", "100 to 57,468,293 km^2"))
p1

ggsave(p1, file = paste0("census_count_per_size.pdf"), path = here::here("analysis", "figs"), height = 10, width = 10)

# Overlap Calcs -----------------------------------------------------------

cpad_census = cpad_census %>% 
  mutate(combo_area = st_area(cpad_census)) %>% 
  mutate(percent_in_park = drop_units(combo_area / census_area * 100))

st_write(cpad_census, here::here("data", "processed_data", "cpad_census.shp"))

average_in_park = cpad_census %>% 
  st_drop_geometry() %>% 
  group_by(park_name, suid_nma) %>% 
  summarize(mean_in_park = mean(percent_in_park),
            size_bin = size_bin) %>% 
  distinct()

write_csv(average_in_park, here::here("analysis", "average_census_percent_in_park.csv"))

p2 = ggplot(average_in_park) +
  geom_boxplot(aes(as.factor(size_bin), mean_in_park)) +
  theme_bw() +
  labs(x = "PA Size Class", 
       y = "Average Percent Census Block in Park") +
  stat_n_text(aes(as.factor(size_bin), mean_in_park)) + 
  scale_x_discrete(breaks = c("1", "2", "3"), labels = c("0 to 10 km^2", "10 to 100 km^2", "100 to 57,468,293 km^2"))
p2

ggsave(p2, file = paste0("average_percent_census_block_in_park.pdf"), path = here::here("analysis", "figs"), height = 10, width = 10)

# overlap plots -----------------------------------------------------------

small_pa = cpad_census %>% 
  slice_min(order_by = shape_area, n = 5)
large_pa = cpad_census %>% 
  slice_max(order_by = shape_area, n = 896) 

small_census = ca_census %>% 
  filter(id %in% small_pa$id)
large_census = ca_census %>% 
  filter(id %in% large_pa$id)

#large PAs
death_valley = large_pa %>% 
  filter(park_name == "Death Valley National Park")
los_padres = large_pa %>% 
  filter(park_name == "Los Padres National Forest")
shasta_trinity = large_pa %>% 
  filter(park_name == "Shasta-Trinity National Forest")
inyo = large_pa %>% 
  filter(park_name == "Inyo National Forest")
death_valley_census = large_census %>% 
  filter(id %in% death_valley$id)
los_padres_census = large_census %>% 
  filter(id %in% los_padres$id)
shasta_trinity_census = large_census %>% 
  filter(id %in% shasta_trinity$id)
inyo_census = large_census %>% 
  filter(id %in% inyo$id)

p1 = ggplot() +
  geom_sf(data= death_valley_census, aes(fill = id)) +
  geom_sf(data = death_valley, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Death Valley National Park")
p2 = ggplot() +
  geom_sf(data= los_padres_census, aes(fill = id)) +
  geom_sf(data = los_padres, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Los Padres National Forest")
p3 = ggplot() +
  geom_sf(data= shasta_trinity_census, aes(fill = id)) +
  geom_sf(data = shasta_trinity, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Shasta-Trinity National Forest")
p4 = ggplot() +
  geom_sf(data= inyo_census, aes(fill = id)) +
  geom_sf(data = inyo, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Inyo National Forest")

plot1 = (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

ggsave(plot1, file = paste0("large_pa_census.pdf"), path = here::here("analysis", "figs"), height = 10, width = 10)

#small pas
scout = small_pa %>% 
  filter(park_name == "Scout Park")
third_street = small_pa %>% 
  filter(park_name == "Third Street & Prospect Avenue Park")
crescent = small_pa %>% 
  filter(park_name == "Crescent Place Triangle")
corona = small_pa %>% 
  filter(park_name == "Corona del Mar Pocket Park")
ventura = small_pa %>% 
  filter(park_name == "Ventura holding 58")
scout_census = small_census %>% 
  filter(id %in% scout$id)
third_street_census = small_census %>% 
  filter(id %in% third_street$id)
crescent_census = small_census %>% 
  filter(id %in% crescent$id)
corona_census = small_census %>% 
  filter(id %in% corona$id)
ventura_census = small_census %>% 
  filter(id %in% ventura$id)

p1 = ggplot() +
  geom_sf(data= scout_census, aes(fill = id)) +
  geom_sf(data = scout, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Scout Park")
p2 = ggplot() +
  geom_sf(data= third_street_census, aes(fill = id)) +
  geom_sf(data = third_street, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Third Street & Prospect Avenue Park")
p3 = ggplot() +
  geom_sf(data= crescent_census, aes(fill = id)) +
  geom_sf(data = crescent, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Crescent Place Triangle")
p4 =ggplot() +
  geom_sf(data= corona_census, aes(fill = id)) +
  geom_sf(data = corona, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Corona del Mar Pocket Park")
p5 = ggplot() +
  geom_sf(data= ventura_census, aes(fill = id)) +
  geom_sf(data = ventura, aes(fill = id), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Ventura holding 58")

plot2 = (p1 + p2 + p3) / (p4 + p5) + plot_annotation(tag_levels = "A")

ggsave(plot2, file = paste0("small_pa_census.pdf"), path = here::here("analysis", "figs"), height = 10, width = 10)

#Average proportion of area that census block group overlaps with the protected area? 
