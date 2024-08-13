# PCA Regression --------------------------------------------------

## Diagnoses

fit1 <- nnet::multinom(class ~ access_typ + mng_ag_lev + gap1_acres + gap2_acres +
                         gap3_acres + gap4_acres + shape_area, data = class_data)

summary(fit1)

Anova(fit1)

tbl_regression(fit1, exp = TRUE)

tidy_fit <- tidy(fit1, conf.int = TRUE, exponentiate = TRUE)

write_csv(tidy_fit, here::here("analysis", "multinom_params.csv"))

probs <- fitted(fit1) %>%
  as.data.frame() %>%
  cbind(class_data)

write_csv(probs, here::here("analysis", "multinom_pred.csv"))

slopes <- avg_slopes(fit1, type = "probs", by = "class") %>%
  mutate(contrast = case_when(
    contrast == "mean(Open Access) - mean(No Public Access)" ~ "No Public Access to Open Access",
    contrast == "mean(Restricted Access) - mean(No Public Access)" ~ "No Public Access to Restricted Access",
    contrast == "mean(Unknown Access) - mean(No Public Access)" ~ "No Public Access to Unknown",
    contrast == "mean(County) - mean(City)" ~ "City to County",
    contrast == "mean(Federal) - mean(City)" ~ "City to Federal",
    contrast == "mean(Home Owners Association) - mean(City)" ~ "City to Home Owners Association",
    contrast == "mean(Non Profit) - mean(City)" ~ "City to Non Profit",
    contrast == "mean(Private) - mean(City)" ~ "City to Private",
    contrast == "mean(Special District) - mean(City)" ~ "City to Special District",
    contrast == "mean(State) - mean(City)" ~ "City to State",
    TRUE ~ contrast
  )) %>%
  mutate(term = case_when(
    term == "gap1_acres" ~ "GAP 1 Acres",
    term == "gap2_acres" ~ "GAP 2 Acres",
    term == "gap3_acres" ~ "GAP 3 Acres",
    term == "gap4_acres" ~ "GAP 4 Acres",
    term == "shape_area" ~ "Shape Area",
    TRUE ~ term
  ))

p1 <- ggplot(slopes %>% filter(term == "access_typ"), aes(contrast, estimate)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point(aes(color = class), size = 2) +
  facet_wrap(~group) +
  theme_bw() +
  scale_colour_viridis_d() +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "",
    y = "Average Change in Odds",
    color = "Comparison Class"
  )

p2 <- ggplot(slopes %>% filter(term == "mng_ag_lev"), aes(contrast, estimate)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point(aes(color = class), size = 2) +
  facet_wrap(~group) +
  theme_bw() +
  scale_colour_viridis_d() +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "",
    y = "Average Change in Odds",
    color = "Comparison Class"
  )

p3 <- ggplot(slopes %>% filter(term %in% c("GAP 1 Acres", "GAP 2 Acres", "GAP 3 Acres", "GAP 4 Acres", "Shape Area")), aes(term, estimate)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point(aes(color = class), size = 2) +
  facet_wrap(~group) +
  theme_bw() +
  scale_colour_viridis_d() +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "",
    y = "Average Change in Odds",
    color = "Comparison Class"
  )

plot <- p1 / p2 / p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("fig2.pdf"), path = here::here("analysis", "figs"), height = 20, width = 15)
