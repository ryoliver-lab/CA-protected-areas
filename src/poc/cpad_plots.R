library(tidyverse)
library(sf)
library(EnvStats)
library(patchwork)

cpad_extend <- st_read(here::here("data", "processed_data", "cpad_extended.shp"))

access_sum <- read_csv(here::here("analysis", "access_summary.csv"))
layer_sum <- read_csv(here::here("analysis", "mgn_type_summary.csv"))

cpad_sub <- cpad_extend %>%
  st_drop_geometry() %>%
  select(-freq_LRO, -freq_LC, -LC, -LRO) %>%
  distinct() %>%
  mutate(log_size = log10(shape_area)) %>%
  mutate(size_bin = case_when(
    log_size <= 4 ~ 1,
    log_size > 4 & log_size < 5 ~ 2,
    log_size >= 5 ~ 3
  ))

# Plots ------------------------------------------------------

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = gHM_min, fill = gHM_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = gHM_mean, fill = gHM_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = gHM_max, fill = gHM_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = gHM_median, fill = gHM_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = gHM_range, fill = gHM_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = gHM_sd, fill = gHM_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_min, fill = HFP_min)) +
  theme_bw()

p2 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_mean, fill = HFP_mean)) +
  theme_bw()

p3 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_max, fill = HFP_max)) +
  theme_bw()

p4 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_median, fill = HFP_median)) +
  theme_bw()

p5 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_range, fill = HFP_range)) +
  theme_bw()

p6 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = hfp_sd, fill = HFP_sd)) +
  theme_bw()

plot1 <- p2 + p6

plot2 <- p1 + p4 + p3 + p5

ggsave(plot1, file = paste0("plot1.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)
ggsave(plot2, file = paste0("plot2.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = freq_lro)) +
  facet_wrap(~lro) +
  theme_bw()

ggsave(p1, file = paste0("plot3.1.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p1 <- ggplot(data = cpad_extend) +
  geom_sf(aes(color = freq_lc)) +
  facet_wrap(~lc) +
  theme_bw()

ggsave(p1, file = paste0("plot3.2.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

# Summary Plots -----------------------------------------------------------

p7 <- ggplot(data = cpad_sub) +
  geom_boxplot(aes(x = access_typ, y = gHM_mean)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Access Level"
  ) +
  stat_n_text(aes(x = access_typ, y = gHM_mean))

p8 <- ggplot(data = cpad_sub) +
  geom_boxplot(aes(x = layer, y = gHM_mean), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Modification Index",
    x = "Agency Type"
  ) +
  stat_n_text(aes(x = layer, y = gHM_mean))

plot3 <- p7 + p8

ggsave(plot3, file = paste0("ghm_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 20)

p7 <- ggplot(data = cpad_sub) +
  geom_boxplot(aes(x = access_typ, y = HFP_mean)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Footprint Index",
    x = "Access Level"
  ) +
  stat_n_text(aes(x = access_typ, y = HFP_mean))

p8 <- ggplot(data = cpad_sub) +
  geom_boxplot(aes(x = layer, y = HFP_mean), outlier.shape = NA) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Human Footprint Index",
    x = "Agency Type"
  ) +
  stat_n_text(aes(x = layer, y = HFP_mean))

plot3 <- p7 + p8

ggsave(plot3, file = paste0("hfp_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 20)

p7 <- ggplot(data = access_sum) +
  geom_col(aes(x = access_typ, y = freq_LRO, fill = as.factor(LRO))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    x = "Access Type",
    y = "Summed Frequency",
    fill = "Category"
  )

p8 <- ggplot(data = layer_sum) +
  geom_col(aes(x = layer, y = freq_LRO, fill = as.factor(LRO))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    x = "Agency Type",
    y = "Summed Frequency",
    fill = "Category"
  )

plot3 <- p7 + p8

ggsave(plot3, file = paste0("landuse_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p7 <- ggplot(data = access_sum) +
  geom_col(aes(x = access_typ, y = freq_LC, fill = as.factor(LC))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    x = "Access Type",
    y = "Summed Frequency",
    fill = "Category"
  )

p8 <- ggplot(data = layer_sum) +
  geom_col(aes(x = layer, y = freq_LC, fill = as.factor(LC))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    x = "Agency Type",
    y = "Summed Frequency",
    fill = "Category"
  )

plot3 <- p7 + p8


ggsave(plot3, file = paste0("landcover_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)


# by size plots -----------------------------------------------------------

p7 <- ggplot(data = cpad_sub) +
  geom_boxplot(aes(x = access_typ, y = shape_area/1000)) +
  theme_bw() +
  coord_flip() +
  labs(
    y = "Protected Area Size km^2",
    x = "Access Level"
  ) +
  stat_n_text(aes(x = access_typ, y = shape_area/1000)) +
  scale_y_log10()

ggsave(p7, file = paste0("access_size_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 12)

p7 <- ggplot(data = cpad_sub) +
  geom_point(aes(x = shape_area/1000, y = scale(gHM_mean))) +
  theme_bw() +
  labs(
    y = "Normalized Global Human Modification",
    x = "Protected Area Size km^2"
  ) +
  scale_x_log10()

p8 <- ggplot(data = cpad_sub) +
  geom_point(aes(x = shape_area/1000, y = scale(HFP_mean))) +
  theme_bw() +
  labs(
    y = "Noramlized Human Footprint Index",
    x = "Protected Area Size km^2"
  ) +
  scale_x_log10()

plot3 <- p7 + p8

ggsave(plot3, file = paste0("mod_size_summary.pdf"), path = here::here("analysis", "figs"), height = 10, width = 20)
