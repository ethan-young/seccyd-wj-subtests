
wj_data_long %>% 
  pivot_longer(cols = c(-id, -starts_with("assess")), names_to = "subtest", values_to = "score") %>% 
  ggplot(aes(x = fct_reorder(assessment, assessment_order), y = score)) +
  geom_boxplot() +
  facet_wrap(~subtest)

wj_data_long %>% 
  #group_by(id) %>% 
  #summarise(across(c(-starts_with("assess")), ~mean(.x, na.rm = T))) %>% 
  pivot_longer(cols = c(-id, -starts_with("assess")), names_to = "subtest", values_to = "score") %>% 
  ggplot(aes(x = assessment_order, y = score)) + 
  geom_line(aes(group = id), alpha = .5, color = "gray") +
  geom_point(alpha = .5) +
  stat_smooth(method = "lm") +
  facet_wrap(~subtest)

wj_data_long %>% 
  group_by(id) %>%
  summarise(across(c(-starts_with("assess")), ~mean(.x, na.rm = T))) %>% 
  select(-id) %>% 
  corr_table()

wj_data_long %>% 
  mutate(general = across(.cols = c(-id, -starts_with("assess"))) %>% rowMeans(na.rm = T)) %>% 
  pivot_longer(c(-id, -assessment, -assessment_order), names_to = "test", values_to = "score") %>% 
  ggplot(aes(x = test, y = score)) +
  geom_boxplot() +
  facet_wrap(~assessment, ncol = 1)

wj_data_long %>% 
  mutate(general = across(.cols = c(-id, -starts_with("assess"))) %>% rowMeans(na.rm = T)) %>% 
  pivot_longer(c(-id, -assessment, -assessment_order), names_to = "test", values_to = "score") %>% 
  filter(test == "general") %>% 
  pivot_wider(id_cols = id, names_from = assessment, values_from = score, names_prefix = "general_") %>% 
  select(-id) %>% 
  corr_table()

wj_data_long %>% 
  mutate(general = across(.cols = c(-id, -starts_with("assess"))) %>% rowMeans(na.rm = T)) %>% 
  pivot_longer(c(-id, -assessment, -assessment_order), names_to = "test", values_to = "score") %>% 
  ggplot(aes(x = score)) +
  geom_histogram() +
  facet_grid(rows = vars(assessment), cols = vars(test))
