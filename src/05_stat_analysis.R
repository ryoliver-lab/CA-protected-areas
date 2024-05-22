library(tidyverse)
library(nnet)
library(gtsummary)
library(broom)
# To plot predicted values
library(ggeffects)
# To get average marginal effect
library(marginaleffects)
# For the Wald Test
library(car)
library(mgcv)
library(tidymodels)
library(patchwork)

class_data <- read_csv(here::here("analysis", "nmds.csv")) %>%
  select(
    class, suid_nma, access_typ, mng_ag_lev, gap1_acres, gap2_acres,
    gap3_acres, gap4_acres, gHM_mean, HFP_mean, sg, hx_pp_r, park_name, shape_area
  ) %>%
  drop_na() %>%
  filter(mng_ag_lev != "Tribal") %>%
  filter(mng_ag_lev != "Joint") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(class = fct_relevel(class, c(
    "high mod / med mobility", "low mod / high safegraph",
    "low mod / high strava"
  )))

classes <- class_data %>%
  select(suid_nma, class) %>%
  distinct()

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  drop_na() %>%
  full_join(classes) %>%
  filter(!is.na(hex_int))

# multinomial regression --------------------------------------------------

fit1 <- nnet::multinom(class ~ access_typ + mng_ag_lev + gap1_acres + gap2_acres +
  gap3_acres + gap4_acres + shape_area, data = class_data)

summary(fit1)

Anova(fit1)

class_data2 = class_data %>% 
  group_by(class) %>% 
  sample_n(903)

fit2 = nnet::multinom(class ~ access_typ + mng_ag_lev + gap1_acres + gap2_acres +
                        gap3_acres + gap4_acres + shape_area, data = class_data2)

summary(fit2)

Anova(fit2)

tbl_regression(fit2, exp = TRUE)

tidy_fit <- tidy(fit2, conf.int = TRUE, exponentiate = TRUE)

write_csv(tidy_fit, here::here("analysis", "multinom_params2.csv"))

predict_class <- predict(fit2, class_data2, type = "class") %>%
  as.data.frame() %>%
  cbind(class_data2)

predict_prob <- predict(fit2, class_data2, type = "prob") %>%
  as.data.frame() %>%
  cbind(predict_class) %>%
  mutate(correct = case_when(
    . == class ~ 1,
    . != class ~ 0
  ))

result <- predict_prob %>%
  group_by(class) %>%
  summarize(
    n = n(),
    num_correct = sum(correct)
  ) %>%
  mutate(percent_correct = num_correct / n) %>%
  full_join(predict_prob)

names(result) <- c(
  "class", "n", "num_correct", "percent_correct",
  "high mod / med mobility", "low mod / high safegraph", "low mod / high strava", "prediction",
  "suid_nma", "access_typ", "mng_ag_lev", "gap1_acres",
  "gap2_acres", "gap3_acres", "gap4_acres", "gHM_mean",
  "HFP_mean", "sg", "hx_pp_r", "park_name",
  "shape_area", "correct"
)

accuracy(result, class, prediction)
precision(result, class, prediction)
sensitivity(result, class, prediction) # true positive rate
specificity(result, class, prediction) # true negative rate

roc_auc(predict_prob, class, c(
  "high mod / med mobility",
  "low mod / high safegraph",
  "low mod / high strava"
))

a <- autoplot(roc_curve(predict_prob, class, c(
  "high mod / med mobility",
  "low mod / high safegraph",
  "low mod / high strava"
))) +
  theme_bw()

ggsave(a, file = paste0("roc.pdf"), path = here::here("analysis", "figs"), height = 10, width = 15)

write_csv(result, here::here("analysis", "multinom_pred1.csv"))

p1 <- ggeffect(fit2, terms = "access_typ") %>%
  plot() +
  labs(
    y = "Probability of Being in Class",
    x = "Access Type",
    title = ""
  )

p2 <- ggeffect(fit2, terms = "mng_ag_lev") %>%
  plot() +
  labs(
    y = "Probability of Being in Class",
    x = "Management Level",
    title = ""
  )

p3 <- ggeffect(fit2, terms = "gap1_acres") %>%
  plot() +
  scale_x_log10() +
  labs(
    y = "Probability of Being in Class",
    x = "GAP 1 Acres",
    title = ""
  )

p4 <- ggeffect(fit2, terms = "gap2_acres") %>%
  plot() +
  scale_x_log10() +
  labs(
    y = "Probability of Being in Class",
    x = "GAP 2 Acres",
    title = ""
  )

p5 <- ggeffect(fit2, terms = "gap3_acres") %>%
  plot() +
  scale_x_log10() +
  labs(
    y = "Probability of Being in Class",
    x = "GAP 3 Acres",
    title = ""
  )

p6 <- ggeffect(fit2, terms = "gap4_acres") %>%
  plot() +
  scale_x_log10() +
  labs(
    y = "Probability of Being in Class",
    x = "GAP 4 Acres",
    title = ""
  )

p7 <- ggeffect(fit2, terms = "shape_area") %>%
  plot() +
  scale_x_log10() +
  labs(
    y = "Probability of Being in Class",
    x = "Park Area",
    title = ""
  )

plot <- p1 / p2
# ggsave(plot, file = paste0("accessvmanage.pdf"), path = here::here("analysis", "figs"), height = 15, width = 15)

plot <- p3 / p4 / p5 / p6 / p7
# ggsave(plot, file = paste0("sizeeffect.pdf"), path = here::here("analysis", "figs"), height = 20, width = 12)

count <- result %>%
  group_by(class, prediction) %>%
  summarize(n = n())

# Spatial Analysis --------------------------------------------------------

gam1 <- gam(hex_int ~ s(gHM) + s(HFP2009) + s(y, x, bs ="gp", m = 1), data = spatial_data)

summary(gam1)
AIC(gam1)

plot(gam1)

class_models <- spatial_data %>%
  filter(count > 10) %>%
  group_by(class) %>%
  do(model = gam(hex_int ~ s(gHM) + s(HFP2009) + s(y, x, bs ="gp", m = 1), data = .))

park_models <- spatial_data %>%
  filter(count > 10) %>%
  group_by(suid_nma, class) %>%
  do(model = gam(hex_int ~ s(gHM, k = 2) + s(HFP2009, k = 2) + s(y, x, k = 2, bs ="gp", m = 1), data = .))

# Constancy: 1 – H(x)/log(n), where H(x) is information entropy or  1/sd

entropy <- function(target) {
  freq <- table(target) / length(target)
  # vectorize
  vec <- as.data.frame(freq)[, 2]
  # drop 0 to avoid NaN resulting from log2
  vec <- vec[vec > 0]
  # compute entropy
  -sum(vec * log2(vec))
}

cross.entropy <- function(p, phat) {
  x <- 0
  for (i in 1:length(p)) {
    x <- x + (p[i] * log(phat[i]))
  }
  return(-x)
}

const <- 1 - entropy(spatial_data$hex_int) / log(nrow(spatial_data))

# Consistency: [H(x) + H(y) – H(XY)]/log(n) where y is spatial position

single_pt_haversine = function(lat, lng) {

  r = 6371 # Earth's radius (km). Have r = 3956 if you want miles

  # 'Single-point' Haversine formula
  a = sin(lat/2)**2 + cos(lat) * sin(lng/2)**2
  d = 2 * r * asin(sqrt(a)) 

  return(d)
}

spatial_data = spatial_data %>% 
  mutate(haversine = single_pt_haversine(y, x))

# need to fix
contin <- (entropy(spatial_data$hex_int) + entropy(spatial_data$haversine) - cross.entropy(spatial_data$hex_int, spatial_data$haversine)) / log(nrow(spatial_data))

class_entropy <- spatial_data %>%
  group_by(class) %>%
  summarise(
    entropy = entropy(hex_int),
    inv_sd = 1 / sd(hex_int),
    const = 1 - entropy(hex_int) / log(n()),
    contin = (entropy(hex_int) + entropy(haversine) - cross.entropy(hex_int, haversine)) / log(nrow(spatial_data))
    
  )

park_entropy <- spatial_data %>%
  group_by(suid_nma, class) %>%
  summarise(
    entropy = entropy(hex_int),
    inv_sd = 1 / sd(hex_int),
    const = 1 - entropy(hex_int) / log(n()),
    contin = (entropy(hex_int) + entropy(haversine) - cross.entropy(hex_int, haversine)) / log(nrow(spatial_data))
  )
