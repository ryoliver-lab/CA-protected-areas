library(tidyverse)
library(sf)
library(EnvStats)
library(patchwork)
library(tidycensus)

#census_api_key("440a21138779ca663e2d17789486ad0389a85339")
options(tigris_use_cache = TRUE)

sf_use_s2(FALSE)

# read raw data -----------------------------------------------------------

cpad_extend <- st_read(here::here("data", "processed_data", "cpad_extended.shp")) %>% 
  select(-LRO, -freq_LRO, -LC, -freq_LC, -gHM_median, -gHM_min, -gHM_max, -gHM_range, -HFP_median, -HFP_min, -HFP_max, -HFP_range) %>% 
  mutate(gHM_cv = gHM_sd / gHM_mean,
         HFP_cv = HFP_sd / HFP_mean)%>%
  sf::st_transform("EPSG:4326")

cpad_strava_coarse <- st_read(here::here("data", "processed_data", "cpad_strava_coarse.shp")) %>%
  sf::st_transform("EPSG:4326")%>% 
  mutate(hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L))%>% 
  mutate(hex_min_int = strtoi(str_remove(hex_min, "#"), 16L))%>% 
  mutate(hex_max_int = strtoi(str_remove(hex_max, "#"), 16L)) %>% 
  mutate(hex_range_int = hex_max_int - hex_min_int) %>% 
  select(-hex_min_int, -hex_max_int, -hex_max, -hex_min, -hex_mean)

cpad_safegraph = st_read(here::here("data", "processed_data", "safegraph.shp")) %>% 
  filter(!is.na(park_nm))%>%
  sf::st_transform("EPSG:4326") 

census <- get_acs(
  state = "CA",
  geography = "tract",
  variables = "B01003_001",
  geometry = TRUE,
  year = 2020
) %>% 
  mutate(census_area = st_area(geometry)) %>% 
  select(-variable, -moe)%>%
  sf::st_transform("EPSG:4326")%>% 
  st_buffer(dist = 0)

# Add new Census data -----------------------------------------------------

cpad_safegraph_strava = st_join(cpad_safegraph, cpad_strava_coarse) %>% 
  st_buffer(dist = 0)

rm(cpad_safegraph, cpad_strava_coarse)

cpad_safegraph_strava_track = st_intersection(census, cpad_safegraph_strava)

rm(cpad_safegraph_strava, census)

# Population weighted means -----------------------------------------------

human_move_cpad = cpad_safegraph_strava_track %>% 
  select(-layer.y, -layer.x, -ID_1, -acres.y, -label_name, 
         -gap4_acres, -gap3_acres, -gap_tot_ac, -gap1_acres, 
         -gap2_acres, -mng_ag_id, -mng_ag_lev, -acres.x, 
         -yr_est.y, -yr_est.x, -gp_tt_c, -gp3_crs, -gp4_crs,
         -gp1_crs, -gp2_crs, -cbg, censs_r, -accss_t, -access_typ,
         -size_bn,  -prcnt__, -park_rl, -park_name, -shape_leng, -park_nm,
         -park_url, -labl_nm, -label_name, -mng_g_d, -mng_gnc, -mng_g_l, 
         -mng_g_t, -log_siz, -agncy_w, -agncy_web, -combo_r, -suid_nm,
         -mng_agncy, -mng_ag_typ, -shp_lng, -shape_r, -ID, -cv_count) %>% 
  janitor::clean_names() %>% 
  mutate(census_area = units::drop_units(census_area)) %>% 
  mutate(combine_area = units::drop_units(st_area(geometry))) %>% 
  mutate(percent_pa = combine_area / shape_area) %>% 
  mutate(percent_block = combine_area / censs_r) %>%
  mutate(census_weighted_count = mean_count * percent_block) %>% 
  group_by(suid_nma, year) %>% 
  mutate(park_weighted_pop = weighted.mean(estimate, percent_pa, na.rm = TRUE)) %>% 
  group_by(suid_nma, year) %>% 
  mutate(park_weighted_count = weighted.mean(census_weighted_count, percent_pa, na.rm = TRUE)) %>% 
  select(-geoid, -name, -estimate, -census_area, -mean_count, -max_count, 
         -min_count, -censs_r, -sd_count, -combine_area, -percent_pa, -percent_block,
         -census_weighted_count) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  mutate(w_count = park_weighted_count / park_weighted_pop,
         w_strava = hex_mean_int / park_weighted_pop,
         w_strava_r = hex_range_int / park_weighted_pop) %>% 
  select(-hex_range_int, -hex_mean_int, -park_weighted_count,
         -park_weighted_pop)

rm(cpad_safegraph_strava_track)

cpad = full_join(human_move_cpad, cpad_extend, 
                 by = "suid_nma", relationship ="many-to-many") %>% 
  distinct() %>%   
  st_as_sf() %>% 
  select(-shape_area.x)%>% 
  rename(shape_area = shape_area.y)%>% 
  mutate(sg_year = year) %>% 
  select(-year) %>% 
  mutate(pa_id = suid_nma) %>% 
  select(-suid_nma)  %>% 
  mutate(sg_count = w_count) %>% 
  select(-w_count) %>% 
  mutate(strava = w_strava) %>% 
  select(-w_strava)%>% 
  mutate(strava_r = w_strava_r) %>% 
  select(-w_strava_r)%>% 
  mutate(access = access_typ) %>% 
  select(-access_typ)%>% 
  mutate(park_nm = park_name) %>% 
  select(-park_name)%>% 
  mutate(park_link = park_url) %>% 
  select(-park_url)

rm(human_move_cpad, cpad_extend)

st_write(cpad, here::here("data", "processed_data", "cpad_human.shp"), append= FALSE, overwrite=TRUE)
