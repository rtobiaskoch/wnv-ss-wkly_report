library(tidyverse)
library(googledrive)
library(googlesheets4)
library(RColorBrewer)

gsheet_pull_prompt("1_input/RMRP_database.csv", 
                   key = key_birds,
                   sheet = "data")
birds_database = read.csv("1_input/RMRP_database.csv")

bird_wk_sum = birds_database %>%
  filter(target_name == "WNV") %>% 
  group_by(week, test_code) %>%
  count() %>%
  group_by(week) %>%
  mutate(tl = sum(n)) %>%
  ungroup() %>%
  complete(week, test_code) %>%
  mutate(pct = n/tl) %>%
  mutate(test_code = factor(test_code))

bird_wk_sum[is.na(bird_wk_sum)] <- 0

ggplot(bird_wk_sum, aes(x = week, color = test_code, fill = test_code)) +
  geom_col(aes(y = n), alpha = 0.7) +
#  geom_line(aes(y = pct*20)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_classic()

ggplot(bird_wk_sum, aes(x = week, y = pct, fill = test_code)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic()
