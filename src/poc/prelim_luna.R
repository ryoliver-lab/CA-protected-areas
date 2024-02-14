library(tidyverse)
library(sf)
library(patchwork)
library(here)

sf_use_s2(FALSE)

cpad_census <- st_read(here("data", "processed_data", "cpad_census.shp")) %>% 
  sf::st_transform("EPSG:4269")

# start with Santa Barbara County
ca_counties <- st_read(here("data", "processed_data", "cb_2018_us_county_500k.shp")) %>% 
  janitor::clean_names()

sb_county <- ca_counties %>% 
  filter(name == "Santa Barbara") %>% 
  select(name)

# check: st_crs(sb_county) == st_crs(cpad_census)

cpad_census_sb <- st_intersection(sb_county, cpad_census)

ggplot() + 
  geom_sf(data = sb_county, # outline of SB county,
          color = "black") +
  geom_sf(data = cpad_census_sb, # filled by access type 
          aes(fill = accss_t)) +
  labs(title = "Access Types in SB County") +
  theme_classic()
  
  # create function to explore access type in California Counties
  access_type_county_plot <- function(county_name) {
    
    county_df <- ca_counties %>% 
      filter(name == county_name) %>% 
      select(name)
    
    cpad_census <- st_intersection(county_df, cpad_census)
    
    county_plot <- ggplot() + 
      geom_sf(data = county_df, # outline of SB county,
              color = "black") +
      geom_sf(data = cpad_census, # filled by access type 
              aes(fill = accss_t)) +
      labs(title = paste0("Access Types in ", county_name),
           fill = "Access Type") +
    theme_classic()
    
    county_plot
  }

access_type_county_plot("Los Angeles")

# ...............................................
# CPAD Holdings by County
# ...............................................

sf_use_s2(FALSE)

cpad_holdings <- st_read(here::here("data", "raw_data", "CPAD_2023a_Holdings.shp")) %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) %>% 
  sf::st_transform("EPSG:4269")

# check: st_crs(sb_county) == st_crs(cpad_holdings)

cpad_holdings_sb <- st_intersection(sb_county, cpad_holdings)

ggplot() + 
  geom_sf(data = sb_county, # outline of SB county,
          color = "black") +
  geom_sf(data = cpad_holdings_sb, # filled by access type 
          aes(fill = access_typ)) +
  labs(title = "Access Types of Holdings in SB County") +
theme_classic()

# cpad_holdings_sb %>% 
#   st_drop_geometry() %>% 
#   select(gap1_acres, gap2_acres, gap3_acres, gap4_acres) %>% 
#   pivot_longer(cols = 1:4,
#                names_to = "group",
#                values_to = "acres") %>% 
#   ggplot() +
#   geom_boxplot(aes(x = group, y = acres))
