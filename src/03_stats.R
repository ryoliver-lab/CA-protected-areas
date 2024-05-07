library(tidyverse)
library(nnet)
library(gtsummary)
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
  mutate_if(is.character,as.factor)

# multinomial regression --------------------------------------------------

fit1 = multinom(class ~ access_typ + mng_ag_lev + gap1_acres + gap2_acres +
                  gap3_acres + gap4_acres, data = class_data)
summary(fit1)

tbl_regression(fit1, exp = TRUE)
