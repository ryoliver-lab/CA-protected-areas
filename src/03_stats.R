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

class_data = read_csv(here::here("analysis", "nmds.csv")) %>% 
  select(class, suid_nma, access_typ, mng_ag_lev, gap1_acres, gap2_acres,
         gap3_acres, gap4_acres, gHM_mean, HFP_mean, sg, hx_pp_r, park_name) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(class = fct_relevel(class, c("low mod / med mobility", "low mod / high mobility", 
                                      "high mod / low mobility", "high mod / med mobility")))

# multinomial regression --------------------------------------------------

fit1 = multinom(class ~ access_typ + mng_ag_lev + gap1_acres + gap2_acres +
                  gap3_acres + gap4_acres, data = class_data)
summary(fit1)

tbl_regression(fit1, exp = TRUE)

tidy_fit = tidy(fit1)

write_csv(tidy_fit, here::here("analysis", "multinom_params1.csv"))

predict_class = predict(fit1, class_data, type = "class") %>% 
  as.data.frame() %>% 
  cbind(class_data)

predict_prob = predict(fit1, class_data, type = "prob") %>% 
  as.data.frame() %>% 
  cbind(predict_class) %>% 
  mutate(correct = case_when(
    . == class ~ 1,
    . != class ~ 0
  ))

result = predict_prob %>% 
  group_by(class) %>% 
  summarize(
    n = n(),
    num_correct = sum(correct)
  ) %>% 
  mutate(percent_correct = num_correct/n) %>% 
  full_join(predict_prob)

write_csv(result, here::here("analysis", "multinom_pred1.csv"))
