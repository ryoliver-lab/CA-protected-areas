library(tidyverse)
library(sf)
library(patchwork)

cpad_extent = st_read(here::here("data", "processed_data", "cpad_extended.shp"))

# Plots ------------------------------------------------------

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_min, fill = g_hm_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_mean, fill = g_hm_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_max, fill = g_hm_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_median, fill = g_hm_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_range, fill = g_hm_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = g_hm_sd, fill = g_hm_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_min, fill = hfp_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_mean, fill = hfp_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_max, fill = hfp_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_median, fill = hfp_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_range, fill = hfp_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_sd, fill = hfp_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = freq_lro)) +
  facet_wrap(~ lro) +
  theme_bw()

ggsave(p1, file = paste0("plot3.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = freq_lc)) +
  facet_wrap(~ lc) +
  theme_bw()

ggsave(p1, file = paste0("plot3.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)