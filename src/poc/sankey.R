library(tidyverse)
library(sf)
library(networkD3)

sf_use_s2(FALSE)

cpad_super <- st_read(here::here("data", "raw_data", "CPAD_2023a_SuperUnits.shp")) %>%
  sf::st_transform("EPSG:4326") %>%
  janitor::clean_names() %>%
  mutate(ID = row_number()) %>%
  st_drop_geometry() %>%
  group_by(suid_nma) %>%
  mutate(max = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
  mutate(gap = case_when(
    gap1_acres == max ~ "GAP 1",
    gap2_acres == max ~ "GAP 2",
    gap3_acres == max ~ "GAP 3",
    gap4_acres == max ~ "GAP 4"
  )) %>%
  select(suid_nma, park_name, access_typ, mng_ag_lev, gap) %>%
  ungroup()

sum1 <- cpad_super %>%
  group_by(access_typ, mng_ag_lev) %>%
  summarise(n = n())
names(sum1) <- c("source", "target", "n")
sum2 <- cpad_super %>%
  group_by(gap, access_typ) %>%
  summarise(n = n())
names(sum2) <- c("source", "target", "n")
sum <- rbind(sum1, sum2)

node_names <- unique(c(as.character(cpad_super$access_typ), as.character(cpad_super$mng_ag_lev), as.character(cpad_super$gap)))
nodes <- data.frame(name = node_names)
nodes$group <- c("a", "b", "b", "c", "d", "f", "f", "d", "d", "d", "f", "f", "f", "f", "c", "b", "a", "a")

links <- data.frame(
  source = match(sum$source, node_names) - 1,
  target = match(sum$target, node_names) - 1,
  value = sum$n
)

my_color <- 'd3.scaleOrdinal() .domain(["a", "b", "c", "d", "f"]) .range(["green", "yellow", "red", "white", "grey"])' # a color for each group in nodes group

p <- sankeyNetwork(
  Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value", 
  fontSize = 12, nodeWidth = 30, colourScale = my_color, NodeGroup = "group"
)

p
