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
    "low mod / med mobility", "low mod / high mobility",
    "high mod / low mobility", "high mod / med mobility"
  )))

classes <- class_data %>%
  select(suid_nma, class) %>%
  distinct()

spatial_data <- read_csv(here::here("data", "processed_data", "spatial_analysis.csv")) %>%
  drop_na() %>%
  group_by(suid_nma) %>%
  mutate(count = n()) %>%
  filter(count > 2) %>%
  ungroup() %>%
  full_join(classes) %>%
  filter(!is.na(hex_int))

# multinomial regression --------------------------------------------------

fit1 <- multinom(class ~ access_typ + mng_ag_lev + gap1_acres + gap2_acres +
  gap3_acres + gap4_acres + shape_area, data = class_data)
summary(fit1)

Anova(fit1)

tbl_regression(fit1, exp = TRUE)

tidy_fit <- tidy(fit1, conf.int = TRUE, exponentiate = TRUE)

write_csv(tidy_fit, here::here("analysis", "multinom_params1.csv"))

predict_class <- predict(fit1, class_data, type = "class") %>%
  as.data.frame() %>%
  cbind(class_data)

predict_prob <- predict(fit1, class_data, type = "prob") %>%
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

write_csv(result, here::here("analysis", "multinom_pred1.csv"))

ggeffect(fit1, terms = "access_typ") %>%
  plot()

ggeffect(fit1, terms = "mng_ag_lev") %>%
  plot()

ggeffect(fit1, terms = "gap1_acres") %>%
  plot()

ggeffect(fit1, terms = "gap2_acres") %>%
  plot()

ggeffect(fit1, terms = "gap3_acres") %>%
  plot()

ggeffect(fit1, terms = "gap4_acres") %>%
  plot()

ggeffect(fit1, terms = "shape_area") %>%
  plot() +
  scale_x_log10()

# Spatial Analysis --------------------------------------------------------

gam1 <- gam(hex_int ~ s(gHM) + s(HFP2009) + s(y, x), data = spatial_data)

summary(gam1)
AIC(gam1)

plot(gam1)

class_models <- spatial_data %>%
  filter(count > 10) %>%
  group_by(class) %>%
  do(model = gam(hex_int ~ s(gHM) + s(HFP2009) + s(y, x), data = .))

park_models <- spatial_data %>%
  filter(count > 10) %>%
  group_by(suid_nma, class) %>%
  do(model = gam(hex_int ~ s(gHM, k = 9) + s(HFP2009, k = 9) + s(y, x, k = 9), data = .))

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

class_entropy <- spatial_data %>%
  group_by(class) %>% 
  summarise(entropy = entropy(hex_int),
            inv_sd = 1 / sd(hex_int),
            const =  1 - entropy(hex_int) / log(n()))

park_entropy <- spatial_data %>%
  group_by(suid_nma, class) %>% 
  summarise(entropy = entropy(hex_int),
            inv_sd = 1 / sd(hex_int),
            const =  1 - entropy(hex_int) / log(n()))

1 / sd(spatial_data$hex_int)

entropy(spatial_data$hex_int)

# Consistency: [H(x) + H(y) – H(XY)]/log(n) where y is spatial position

# need to fix
contin <- (entropy(spatial_data$hex_int) + entropy(spatial) - cross.entropy(spatial_data$hex_int, spatial)) / log(nrow(spatial_data))
