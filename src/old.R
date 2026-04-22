# # JSD Test ----------------------------------------------------------------
# 
# strava_inside <- strava %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   filter(!is.na(gray_scale)) %>%
#   pull(gray_scale)
# 
# strava_outside <- strava %>%
#   filter(loc == "Outside Protected Areas") %>%
#   filter(!is.na(gray_scale)) %>%
#   pull(gray_scale)
# 
# breaks <- seq(min(c(strava_inside, strava_outside)), max(c(strava_inside, strava_outside)), length.out = 100)
# p1 <- hist(strava_inside, breaks = breaks, plot = FALSE)$counts / length(strava_inside)
# p2 <- hist(strava_outside, breaks = breaks, plot = FALSE)$counts / length(strava_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") # 0.1871658 bounded between 0 = same and 1
# # GAP 1/2  0.1092982
# # GAP 3/4 0.2204127
# 
# hfp_inside <- hfp %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   filter(!is.na(HFP2009)) %>%
#   pull(HFP2009)
# 
# hfp_outside <- hfp %>%
#   filter(loc == "Outside Protected Areas") %>%
#   filter(!is.na(HFP2009)) %>%
#   pull(HFP2009)
# 
# breaks <- seq(min(c(hfp_inside, hfp_outside)), max(c(hfp_inside, hfp_outside)), length.out = 100)
# p1 <- hist(hfp_inside, breaks = breaks, plot = FALSE)$counts / length(hfp_inside)
# p2 <- hist(hfp_outside, breaks = breaks, plot = FALSE)$counts / length(hfp_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") # 0.2390168  bounded between 0 = same and 1
# # GAP 1/2 0.11747
# # GAP 3/4 0.289343
# 
# bio_inside <- bio %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   pull(ntv_cnt)
# 
# bio_outside <- bio %>%
#   filter(loc == "Outside Protected Areas") %>%
#   pull(ntv_cnt)
# 
# breaks <- seq(min(c(bio_inside, bio_outside)), max(c(bio_inside, bio_outside)), length.out = 100)
# p1 <- hist(bio_inside, breaks = breaks, plot = FALSE)$counts / length(bio_inside)
# p2 <- hist(bio_outside, breaks = breaks, plot = FALSE)$counts / length(bio_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") #  0.07686548
# # GAP 1/2 0.1081752
# # GAP 3/4 0.07450112
# 
# rare_inside <- rare %>%
#   filter(loc == "Inside Protected Areas") %>%
#   # filter(gap_status %in% c("GAP 1", "GAP 2")) %>%
#   # filter(gap_status %in% c("GAP 3", "GAP 4")) %>%
#   filter(rare_count > 0) %>%
#   pull(rare_count)
# 
# rare_outside <- rare %>%
#   filter(loc == "Outside Protected Areas") %>%
#   filter(rare_count > 0) %>%
#   pull(rare_count)
# 
# breaks <- seq(min(c(rare_inside, rare_outside)), max(c(rare_inside, rare_outside)), length.out = 100)
# p1 <- hist(rare_inside, breaks = breaks, plot = FALSE)$counts / length(rare_inside)
# p2 <- hist(rare_outside, breaks = breaks, plot = FALSE)$counts / length(rare_outside)
# 
# JSD(rbind(p1, p2), unit = "log2") # 0.1141226
# # GAP 1/2  0.1284492
# # GAP 3/4   0.1099759

# Distributions -----------------------------------------------------------

strava <- strava %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

hfp <- hfp %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

bio <- bio %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

rare <- rare %>%
  mutate(gap_loc = case_when(
    loc == "Outside Protected Areas" ~ "Outside",
    gap_status == "GAP 1" ~ "GAP 1",
    gap_status == "GAP 2" ~ "GAP 2",
    gap_status == "GAP 3" ~ "GAP 3",
    gap_status == "GAP 4" ~ "GAP 4"
  ))

# rsvg_png(here::here("icons", "building-regular-full.svg"), file = here::here("icons", "building-regular-full.png"))
# rsvg_png(here::here("icons", "kiwi-bird-solid-full.svg"), file = here::here("icons", "kiwi-bird-solid-full.png"))
# rsvg_png(here::here("icons", "person-running-solid-full.svg"), file = here::here("icons", "person-running-solid-full.png"))
# rsvg_png(here::here("icons", "seedling-solid-full.svg"), file = here::here("icons", "seedling-solid-full.png"))

HFI_png <- here::here("icons", "building-regular-full.png")
strava_png <- here::here("icons", "person-running-solid-full.png")
native_png <- here::here("icons", "seedling-solid-full.png")
rare_png <- here::here("icons", "kiwi-bird-solid-full.png")

p1 <- ggplot(hfp) +
  geom_density_ridges(aes(x = HFP2009, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Landscape Modification ", "<img src='", HFI_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = c(viridis::viridis(5, end = .9)[1:4], "#fde725")) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    legend.position = "none",
    axis.title.x = element_markdown()
  )

p2 <- ggplot(bio) +
  geom_density_ridges(aes(x = ntv_cnt, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Native Species Count ", "<img src='", native_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = c(viridis::viridis(5, end = .9)[1:4], "#fde725")) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    legend.position = "none",
    axis.title.x = element_markdown()
  )

p3 <- ggplot(rare) +
  geom_density_ridges(aes(x = rare_count, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Rare Species Count ", "<img src='", rare_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = c(viridis::viridis(5, end = .9)[1:4], "#fde725")) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    legend.position = "none",
    axis.title.x = element_markdown()
  )

p6 <- ggplot(strava) +
  geom_density_ridges(aes(x = gray_scale, y = gap_loc, fill = gap_loc), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Human Mobility ", "<img src='", strava_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = c(viridis::viridis(5, end = .9)[1:4], "#fde725")) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

strava_combine_sub <- strava %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))

hfp_combine_sub <- hfp %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))

bio_combine_sub <- bio %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))


rare_combine_sub <- rare %>%
  filter(mng_agncy %in% c(
    "United States National Park Service",
    "The Nature Conservancy",
    "United States Forest Service",
    "California Department of Fish and Wildlife",
    "California Department of Parks and Recreation"
  )) %>%
  mutate(mng_agncy = case_when(
    mng_agncy == "United States National Park Service" ~ "NPS",
    mng_agncy == "The Nature Conservancy" ~ "TNC",
    mng_agncy == "United States Forest Service" ~ "USFS",
    mng_agncy == "California Department of Fish and Wildlife" ~ "CDFW",
    mng_agncy == "California Department of Parks and Recreation" ~ "CDPR"
  ))

p7 <- ggplot(hfp_combine_sub) +
  geom_density_ridges(aes(x = HFP2009, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Landscape Modification ", "<img src='", HFI_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    legend.position = "none",
    axis.title.x = element_markdown()
  )

p8 <- ggplot(bio_combine_sub) +
  geom_density_ridges(aes(x = ntv_cnt, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Native Species Count ", "<img src='", native_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    legend.position = "none",
    axis.title.x = element_markdown()
  )

p9 <- ggplot(rare_combine_sub) +
  geom_density_ridges(aes(x = rare_count, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Rare Species Count ", "<img src='", rare_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    legend.position = "none",
    axis.title.x = element_markdown()
  )

p10 <- ggplot(strava_combine_sub) +
  geom_density_ridges(aes(x = gray_scale, y = mng_agncy, fill = mng_agncy), alpha = 0.5, scale = 3) +
  theme_bw(base_size = 20) +
  labs(
    x = paste0("Human Mobility ", "<img src='", strava_png, "' width='24'/>"),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    legend.position = "none",
    axis.title.x = element_markdown()
  )

layout <- "
AABF
AACG
AADH
AAEI
"

plot <- p11 + p2 + p3 + p1 + p6 + p8 + p9 + p7 + p10 + plot_annotation(tag_levels = "A") + plot_layout(design = layout)

ggsave(plot = plot, filename = paste0("fig1.pdf"), path = here::here("analysis", "figs"), height = 15, width = 30, device = cairo_pdf)
