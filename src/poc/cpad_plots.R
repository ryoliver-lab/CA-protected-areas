library(tidyverse)
library(sf)
library(patchwork)

cpad_extent = st_read(here::here("data", "processed_data", "cpad_extended.shp"))

access_sum = read_csv(here::here("analysis", "access_summary.csv"))
layer_sum = read_csv(here::here("analysis", "mgn_type_summary.csv"))

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

# Summary Plots -----------------------------------------------------------

p7 <- ggplot(data = cpad_values1) +
  geom_boxplot(aes(x = access_typ, y = gHM)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Access Level"
  )

p8 <- ggplot(data = cpad_values1) +
  geom_boxplot(aes(x = layer, y = gHM), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Agency Type"
  )

plot3 <- p7 + p8

ggsave(plot3, file = paste0("plot4.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p7 <- ggplot(data = cpad_values2) +
  geom_boxplot(aes(x = access_typ, y = HFP2009)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Access Level"
  )

p8 <- ggplot(data = cpad_values2) +
  geom_boxplot(aes(x = layer, y = HFP2009), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Agency Type"
  )

plot3 <- p7 + p8

ggsave(plot3, file = paste0("plot4.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p7 = ggplot(data = access_sum3) +
  geom_col(aes(x = access_typ, y = freq_LRO, fill = as.factor(LRO))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p8 = ggplot(data = layer_sum3) +
  geom_col(aes(x = layer, y = freq_LRO, fill = as.factor(LRO))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot3 <- p7 + p8


ggsave(plot3, file = paste0("plot4.3.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p7 = ggplot(data = access_sum4) +
  geom_col(aes(x = access_typ, y = freq_LC, fill = as.factor(LC))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p8 = ggplot(data = layer_sum4) +
  geom_col(aes(x = layer, y = freq_LC, fill = as.factor(LC))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot3 <- p7 + p8


ggsave(plot3, file = paste0("plot4.4.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)