library(tidyverse)
library(ggridges)
library(sf)
library(terra)
library(patchwork)

sf_use_s2(FALSE)

# cali --------------------------------------------------------------------

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) %>% 
  filter(park_name != "BLM")

# GHM ---------------------------------------------------------------------

cpad_ghm <- rast(here::here("data", "processed_data", "gHM_masked.tif"))  %>%
  project("EPSG:4326")
cpad_ghm = terra::extract(cpad_ghm, cpad_super, xy=TRUE) %>% 
  filter(!is.na(gHM)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")
  
ghm <- rast(here::here("data", "raw_data", "gHM.tif")) %>%
  project("EPSG:4326")

ghm_cropped = crop(ghm, cpad_super)

ghm_masked = mask(ghm_cropped, cpad_super, inverse = TRUE)
ghm_masked = mask(ghm_masked, ca)

ghm_masked = terra::extract(ghm_masked, ca, xy=TRUE) %>% 
  filter(!is.na(gHM)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

ghm_ca = mask(ghm_cropped, ca)
ghm_ca = terra::extract(ghm_ca, ca, xy=TRUE) %>% 
  filter(!is.na(gHM)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "All California")

rm(ghm, ghm_cropped)

ghm = list(ghm_ca, ghm_masked, cpad_ghm) %>% 
  reduce(rbind) %>% 
  st_set_crs("EPSG:4326")

rm(ghm_ca, ghm_masked, cpad_ghm)

# HFP ---------------------------------------------------------------------

cpad_hfp <- rast(here::here("data", "processed_data", "HFP2009_masked.tif")) %>%
  project("EPSG:4326")
cpad_hfp = terra::extract(cpad_hfp, cpad_super, xy=TRUE) %>% 
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

hfp <- rast(here::here("data", "raw_data", "HFP2009.tif")) %>%
  project("EPSG:4326")

hfp_cropped = crop(hfp, cpad_super)

hfp_masked = mask(hfp_cropped, cpad_super, inverse = TRUE)
hfp_masked = mask(hfp_masked, ca)
hfp_masked = terra::extract(hfp_masked, ca, xy=TRUE) %>% 
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

hfp_ca = mask(hfp_cropped, ca)
hfp_ca = terra::extract(hfp_ca, ca, xy=TRUE) %>% 
  filter(!is.na(HFP2009)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "All California")

rm(hfp, hfp_cropped)

hfp = list(hfp_ca, hfp_masked, cpad_hfp) %>% 
  reduce(rbind)%>% 
  st_set_crs("EPSG:4326")

rm(hfp_ca, hfp_masked, cpad_hfp)

# Strava ---------------------------------------------------------------------

cpad_strava <- rast(here::here("data", "raw_data", "strava_coarse_masked.tif")) %>%
  project("EPSG:4326")
cpad_strava = terra::extract(cpad_strava, cpad_super, xy=TRUE) %>% 
  filter(!is.na(strava_coarse_1)) %>% 
  mutate(hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255)) %>% 
  mutate(hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L)) %>% 
  select(hex_mean, hex_mean_int, x, y, ID) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Inside Protected Areas")

strava <- rast(here::here("data", "raw_data", "strava_coarse.tif")) %>%
  project("EPSG:4326")

strava_cropped = crop(strava, cpad_super)

strava_masked = mask(strava_cropped, cpad_super, inverse = TRUE)
strava_masked = mask(strava_masked, ca)
strava_masked = terra::extract(strava_masked, ca, xy=TRUE) %>% 
  filter(!is.na(strava_coarse_1)) %>% 
  mutate(hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255)) %>% 
  mutate(hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L)) %>% 
  select(hex_mean, hex_mean_int, x, y, ID) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "Outside Protected Areas")

strava_ca = mask(strava_cropped, ca)
strava_ca = terra::extract(strava_ca, ca, xy=TRUE) %>% 
  filter(!is.na(strava_coarse_1)) %>% 
  mutate(hex_mean = rgb(strava_coarse_1, strava_coarse_2, strava_coarse_3, maxColorValue = 255)) %>% 
  mutate(hex_mean_int = strtoi(str_remove(hex_mean, "#"), 16L)) %>% 
  select(hex_mean, hex_mean_int, x, y, ID) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  mutate(loc = "All California")

rm(strava, strava_cropped, ca)

strava = list(strava_ca, strava_masked, cpad_strava) %>% 
  reduce(rbind)%>% 
  st_set_crs("EPSG:4326")

rm(strava_ca, strava_masked, cpad_strava)

# distribution maps -------------------------------------------------------

p1 = ggplot() +
  geom_density_ridges(data = ghm, aes(x = gHM, y = loc, fill = loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 2-) +
  labs(x = "Global Human Modification",
       y = "",
       fill = "Location")+
  scale_fill_viridis_d() 

p2 = ggplot() +
  geom_density_ridges(data = hfp, aes(x = HFP2009, y = loc, fill = loc), alpha = 0.5, scale = 3) +
  theme_bw() +
  labs(x = "Human Footprint Index",
       y = "",
       fill = "Location")+
  scale_fill_viridis_d() 

p3 = ggplot() +
  geom_density_ridges(data = strava, aes(x = hex_mean_int, y = loc, fill = loc), alpha = 0.5, scale = 3) +
  theme_bw() +
  labs(x = "Strava Heatmap",
       y = "",
       fill = "Location")+
  scale_fill_viridis_d()

plot = (p1 / p2 / p3) + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("mod_distribution.pdf"), path = here::here("analysis", "figs"), height = 12, width = 15)

# all PAs -----------------------------------------------------------------

p1 = ggplot() +
  geom_sf(data= ghm %>% filter(loc == "All California"), aes(fill = gHM, color = gHM), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_fill_viridis_c(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(color = "Global Modification",
       fill = "Global Modification")

p2 = ggplot() +
  geom_sf(data= hfp %>% filter(loc == "All California"), aes(fill = HFP2009, color = HFP2009), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Human Footprint",
       fill = "Human Footprint")

p3 = ggplot() +
  geom_sf(data= strava %>% filter(loc == "All California"), aes(fill = hex_mean_int, color = hex_mean_int), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Strava Heatmap",
       fill = "Strava Heatmap")

plot = p1 / p2 / p3
plot

ggsave(plot, file = paste0("mod_all_ca.pdf"), path = here::here("analysis", "figs"), height = 15, width = 12)

p1 = ggplot() +
  geom_sf(data= ghm %>% filter(loc == "Inside Protected Areas"), aes(fill = gHM, color = gHM), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_fill_viridis_c(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(color = "Global Modification",
       fill = "Global Modification")

p2 = ggplot() +
  geom_sf(data= hfp %>% filter(loc == "Inside Protected Areas"), aes(fill = HFP2009, color = HFP2009), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Human Footprint",
       fill = "Human Footprint")

p3 = ggplot() +
  geom_sf(data= strava %>% filter(loc == "Inside Protected Areas"), aes(fill = hex_mean_int, color = hex_mean_int), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Strava Heatmap",
       fill = "Strava Heatmap")

plot = p1 / p2 / p3
plot

ggsave(plot, file = paste0("mod_all_pa.pdf"), path = here::here("analysis", "figs"), height = 15, width = 12)

# Individual PAs ----------------------------------------------------------

large_pa = cpad_super %>% 
  filter(park_name == "Death Valley National Park" | park_name == "Los Padres National Forest" |
           park_name == "Shasta-Trinity National Forest" | park_name == "Inyo National Forest")

rm(cpad_super)

large_pa_ghm = ghm[large_pa, ,]
large_pa_hfp = hfp[large_pa, ,]
large_pa_strava = strava[large_pa, ,]

p1 = ggplot() +
  geom_sf(data= large_pa_ghm, aes(fill = gHM, color = gHM), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c() +
  scale_fill_viridis_c()+
  labs(color = "Global Modification",
       fill = "Global Modification")

p2 = ggplot() +
  geom_sf(data= large_pa_hfp, aes(fill = HFP2009, color = HFP2009), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name)) 
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Human Footprint",
       fill = "Human Footprint")

p3 = ggplot() +
  geom_sf(data= large_pa_strava, aes(fill = hex_mean_int, color = hex_mean_int), size = 0.5) +
  # geom_sf(data = large_pa, linewidth = 0.05, fill = "transparent") +
  theme_bw() +
  # geom_text(data = large_pa, aes(label = park_name))
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Strava Heatmap",
       fill = "Strava Heatmap")

plot = p1 / p2 / p3
plot

ggsave(plot, file = paste0("large_pa_mod.pdf"), path = here::here("analysis", "figs"), height = 15, width = 12)
