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
  mutate(ID = row_number())

ca = st_read(here::here("data", "raw_data", "CA_State_TIGER2016.shp")) %>%
  sf::st_transform("EPSG:4326")

# GHM ---------------------------------------------------------------------

cpad_ghm <- rast(here::here("data", "processed_data", "gHM_masked.tif"))  %>%
  project("EPSG:4326")
cpad_ghm = terra::extract(cpad_ghm, ca, xy=TRUE) %>% 
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
cpad_hfp = terra::extract(cpad_hfp, ca, xy=TRUE) %>% 
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

rm(hfp, hfp_cropped, ca)

hfp = list(hfp_ca, hfp_masked, cpad_hfp) %>% 
  reduce(rbind)%>% 
  st_set_crs("EPSG:4326")

rm(hfp_ca, hfp_masked, cpad_hfp)

# distribution maps -------------------------------------------------------

p1 = ggplot() +
  geom_density_ridges(data = ghm, aes(x = gHM, y = loc, fill = loc), alpha = 0.5, scale = 3) +
  theme_bw() +
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

plot = p1 / p2 + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("mod_distribution.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# Individual PAs ----------------------------------------------------------

large_pa = cpad_super %>% 
  filter(park_name == "Death Valley National Park" | park_name == "Los Padres National Forest" |
           park_name == "Shasta-Trinity National Forest" | park_name == "Inyo National Forest")

rm(cpad_super)

large_pa_ghm = ghm[large_pa, ,]
large_pa_hfp = hfp[large_pa, ,]

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

plot = p1 / p2 
plot

ggsave(plot, file = paste0("large_pa_mod.pdf"), path = here::here("analysis", "figs"), height = 15, width = 12)
