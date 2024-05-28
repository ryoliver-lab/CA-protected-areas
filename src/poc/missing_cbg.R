cpad_cbg = cpad_census %>% 
  st_drop_geometry() %>% 
  filter(!is.na(suid_nm)) %>% 
  select(cbg) %>% 
  mutate(cpad_cbg = cbg) %>% 
  distinct()

sg_cbg = safegraph %>% 
  select(cbg) %>% 
  mutate(sg_cbg = cbg) %>% 
  distinct()

cbg = full_join(cpad_cbg, sg_cbg) %>% 
  distinct()

missing_cbg = cbg %>% 
  select(cpad_cbg) %>% 
  filter(!is.na(cpad_cbg)) %>% 
  arrange(cpad_cbg)

write_csv(missing_cbg, here::here("data", "cpad_cbg.csv"))
