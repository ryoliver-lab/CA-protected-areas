# temporal_data <- read_csv(here::here("data", "processed_data", "temporal_analysis.csv")) %>%
#   rowwise() %>%
#   mutate(gap = max(gap1_acres, gap2_acres, gap3_acres, gap4_acres)) %>%
#   mutate(gap_status = case_when(
#     gap == gap1_acres ~ "GAP 1",
#     gap == gap2_acres ~ "GAP 2",
#     gap == gap3_acres ~ "GAP 3",
#     gap == gap4_acres ~ "GAP 4"
#   )) %>%
#   select(suid_nma, date, sg, park_name, mng_ag_lev, mng_agncy, gap_status) %>%
#   filter(!is.na(sg)) %>%
#   group_by(suid_nma) %>%
#   mutate(count = n()) %>%
#   filter(suid_nma %in% human$suid_nma) %>%
#   mutate(year = year(date))
# Temporal Analysis -------------------------------------------------------

temporal_data <- temporal_data %>%
  mutate(date_j = yday(date)) %>%
  mutate(date_j = case_when(
    year == 2020 ~ date_j + 365,
    year == 2021 ~ date_j + 731,
    year == 2019 ~ date_j
  )) %>%
  mutate(
    month = month(date),
    day = mday(date)
  ) %>%
  mutate(day_plus_month = day + month * 100) %>%
  mutate(season = case_when(
    day_plus_month %in% 320:620 ~ "Spring",
    day_plus_month %in% 621:922 ~ "Summer",
    day_plus_month %in% 923:1221 ~ "Fall",
    TRUE ~ "Winter"
  )) %>%
  select(-month, -day, -day_plus_month) %>%
  mutate(season = fct_relevel(season, "Winter", "Spring", "Summer", "Fall"))

temporal_bins <- temporal_data %>%
  group_by(suid_nma) %>%
  mutate(
    max = max(sg, na.rm = TRUE),
    min = min(sg, na.rm = TRUE),
    quartile_1 = quantile(sg, probs = 0.25),
    quartile_2 = quantile(sg, probs = 0.5),
    quartile_3 = quantile(sg, probs = 0.75)
  ) %>%
  ungroup() %>%
  mutate(bin = case_when(
    sg < quartile_1 ~ "1st Quartile",
    sg > quartile_3 ~ "4th Quartile",
    sg >= quartile_1 & sg < quartile_2 ~ "2nd Quartile",
    sg >= quartile_2 & sg <= quartile_3 ~ "3rd Quartile"
  ))

temp_date <- temporal_bins %>%
  group_by(date_j, bin) %>%
  mutate(date_count = n()) %>%
  ungroup() %>%
  select(date, bin, date_count) %>%
  distinct()

temp_date2 <- temporal_bins %>%
  group_by(date_j, bin, mng_ag_lev) %>%
  mutate(date_count = n()) %>%
  ungroup() %>%
  select(date, bin, date_count, mng_ag_lev) %>%
  distinct()

temp_date3 <- temporal_bins %>%
  group_by(date_j, bin, mng_agncy) %>%
  mutate(date_count = n()) %>%
  ungroup() %>%
  select(date, bin, date_count, mng_agncy) %>%
  distinct()

temp_date4 <- temporal_bins %>%
  group_by(date_j, bin, gap_status) %>%
  mutate(date_count = n()) %>%
  ungroup() %>%
  select(date, bin, date_count, gap_status) %>%
  distinct()

# p1 <- ggplot(temp_date) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c() + # trans = scales::pseudo_log_trans(sigma = 0.001)
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Tourism Quartiles",
#     fill = "Count"
#   ) +
#   scale_x_date()

seasons <- temporal_bins %>%
  filter(year == 2019) %>%
  group_by(season, suid_nma) %>%
  summarize(season_med = median(sg)) %>%
  group_by(suid_nma) %>%
  slice_max(order_by = season_med, n = 1)

season_sum <- seasons %>%
  group_by(season) %>%
  mutate(season_count = n()) %>%
  select(season_count) %>%
  distinct()

p2 <- ggplot(data = season_sum) +
  geom_bar(aes(x = (season_count / sum(season_sum$season_count) * 100), y = "", fill = season), position = "stack", stat = "identity") +
  theme_bw(base_size = 16) +
  labs(
    x = "Percent Parks with Peak Usage in a Season",
    y = "",
    fill = ""
  ) +
  scale_fill_viridis_d(end = 0.9) +
  theme(legend.position = "bottom")

temp_med_mad <- temporal_data %>%
  filter(year == 2019) %>%
  group_by(suid_nma, mng_agncy, park_name) %>%
  summarize(
    med_sg = median(sg),
    mad_sg = mad(sg)
  ) %>%
  full_join(seasons)

p3 <- ggplot() +
  geom_point(data = temp_med_mad %>% filter(med_sg < 1000), aes(med_sg, mad_sg, color = season)) +
  # geom_point(data = CDFW_med_mad, aes(med_sg, mad_sg, color = "California Department of Fish and Wildlife")) +
  # geom_point(data = NPS_med_mad, aes(med_sg, mad_sg, color = "United States National Park Service")) +
  # geom_point(data = TNC_med_mad, aes(med_sg, mad_sg, color = "The Nature Conservancy")) +
  # geom_point(data = UC_med_mad, aes(med_sg, mad_sg, color = "United States Forest Service")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(end = 0.9) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    x = "Median Safegraph",
    y = "Median Absolute Deviance",
    color = ""
  )

plot <- (p2 + p3) + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("fig4.pdf"), path = here::here("analysis", "figs"), height = 10, width = 15)

# p1 = ggplot(temp_date2 %>% filter(mng_ag_lev == "City")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "City"
#   ) +
#   scale_x_date()
#
# p2 = ggplot(temp_date2 %>% filter(mng_ag_lev == "County")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "County"
#
#   ) +
#   scale_x_date()
#
# p3 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Federal")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "Federal"
#   ) +
#   scale_x_date()
#
# p4 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Home Owners Association")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "Home Owners Association"
#   ) +
#   scale_x_date()
#
# p5 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Joint")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "Joint"
#   ) +
#   scale_x_date()
#
# p6 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Non Profit")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "Non Profit"
#   ) +
#   scale_x_date()
#
# p7 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Private")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "Private"
#   ) +
#   scale_x_date()
#
# p8 = ggplot(temp_date2 %>% filter(mng_ag_lev == "Special District")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "Special District"
#   ) +
#   scale_x_date()
#
# p9 = ggplot(temp_date2 %>% filter(mng_ag_lev == "State")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "State"
#   ) +
#   scale_x_date()
#
# p = (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9)
#
# ggsave(p, file = paste0("figS8.pdf"), path = here::here("analysis", "figs"), height = 15, width = 20)
#
# p1 = ggplot(temp_date3 %>% filter(mng_agncy == "United States National Park Service")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "National Park Service"
#   ) +
#   scale_x_date()
#
# p2 = ggplot(temp_date3 %>% filter(mng_agncy == "The Nature Conservancy")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "The Nature Conservancy"
#
#   ) +
#   scale_x_date()
#
# p3 = ggplot(temp_date3 %>% filter(mng_agncy == "California Department of Fish and Wildlife")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "California Fish and Wildlife"
#   ) +
#   scale_x_date()
#
# p4 = ggplot(temp_date3 %>% filter(mng_agncy == "United States Forest Service")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "United States Forest Service"
#   ) +
#   scale_x_date()
#
# p5 = ggplot(temp_date3 %>% filter(mng_agncy == "California Department of Parks and Recreation")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "California Parks and Rec"
#   ) +
#   scale_x_date()
#
# p = (p1 + p2) /(p3 + p4 + p5)
#
# ggsave(p, file = paste0("figS9.pdf"), path = here::here("analysis", "figs"), height = 15, width = 20)
#
# p1 = ggplot(temp_date4 %>% filter(gap_status == "GAP 1")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "GAP 1"
#   ) +
#   scale_x_date()
#
# p2 = ggplot(temp_date4 %>% filter(gap_status == "GAP 2")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "GAP 2"
#
#   ) +
#   scale_x_date()
#
# p3 = ggplot(temp_date4 %>% filter(gap_status == "GAP 3")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "GAP 3"
#   ) +
#   scale_x_date()
#
# p4 = ggplot(temp_date4 %>% filter(gap_status == "GAP 4")) +
#   geom_tile(aes(date, bin, fill = date_count)) +
#   geom_vline(aes(xintercept = as.Date("2020-03-15")), color = "blue") + # covid lockdown
#   geom_vline(aes(xintercept = as.Date("2020-04-24")), color = "blue") + # earliest reopen
#   scale_fill_viridis_c(na.value = "#440154") +
#   theme_bw(base_size= 16) +
#   labs(
#     x = "Date",
#     y = "Safegraph Quartiles",
#     fill = "Count",
#     title = "GAP 4"
#   ) +
#   scale_x_date()
#
# p = (p1 + p2) /(p3 + p4)
#
# ggsave(p, file = paste0("figS10.pdf"), path = here::here("analysis", "figs"), height = 15, width = 20)
