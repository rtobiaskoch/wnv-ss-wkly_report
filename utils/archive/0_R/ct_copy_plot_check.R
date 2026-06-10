

p_ct_copy = database_update %>%
  filter(year > 2023) %>%
  filter(!is.na(test_code)) %>%
  mutate(log_copies = log(copies)) %>%
ggplot(aes(x = cq, y = log_copies, color = factor(test_code), group= week)) + 
  geom_smooth(method = "lm", se = F, alpha = 0.5, color = "grey80") +
  geom_point(aes(text = csu_id, shape = factor(week)), alpha = 0.6) + 
  facet_wrap(~year, scales = "free_y") +
  geom_hline(yintercept = log(copy_threshold), linetype = "dashed", color = "red") +
  theme_classic() +
  theme(legend.position = "none")

ggplotly(p_ct_copy)

