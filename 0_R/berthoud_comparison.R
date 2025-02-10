list2env(readRDS(config_params_file),           envir = .GlobalEnv)


real_data_zone = read.csv(fn_data_output) %>%
  filter(zone == "BE") %>%
  mutate(grp = "real")

#set number of hypothetical traps
n = 5

set.seed(1)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- C L E A N -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Function to sample n rows while ensuring test_code == 1 is included
sampler <- function(data, n) {
  test_code_rows <- data %>% filter(test_code > 0)
  other_rows <- data %>% filter(test_code == 0) %>% sample_n(n - nrow(test_code_rows))
  bind_rows(test_code_rows, other_rows)
}


hypo_data0 = check_read_fun(fn_database_update, wk = week_filter_yr) %>%
  filter(zone == "BE") %>%
  filter(week > 23) %>% #removw 23 because extra cdc traps werent provided
  filter(method == "L")

hypo_trap = hypo_data0 %>% 
  group_by(year, week, zone, trap_id) %>%
  summarise(test_code = sum(test_code)) %>%
  group_modify(~sampler(.x, n = n)) %>%
  ungroup()

hypo_data = hypo_data0 %>% 
  semi_join(hypo_trap, by = c("year", "week", "zone", "trap_id")) %>%
  mutate(grp = paste(year, week, zone, spp, sep = "-"))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- A B U N D A N C E -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
hypo_abund = hypo_data %>%
  group_by(year, week, zone, spp) %>%
  summarize(mosq_L = sum(total)) %>%
  mutate(abund = mosq_L/n) %>%
  select(-mosq_L)

    
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------- P I R : SPP -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
  
  mle = pIR(test_code ~ total|grp, data = hypo_data, pt.method = "firth")
  
hypo_pir =  as.data.frame(mle) %>%
    separate(grp,
             into = c("year", "week", "zone", "spp"),
             sep = "-") %>%
    transmute(year = as.integer(year),
              week = as.integer(week),
              zone = zone,
              spp = spp,
              pir = round(P,4),
              pir_lci = round(Lower,4),
              pir_uci = round(Upper,4)
    )




hypo_data_zone0 = full_join(hypo_abund, hypo_pir, by = c("year", "week", "zone", "spp")) %>%
  mutate(vi = pir*abund,
         vi_uci = pir_uci*abund,
         vi_lci = pir_lci*abund)

sum_col = c("abund", "vi", "vi_lci", "vi_uci")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------- A B U N D : A L L -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

hypo_spp_all0 = hypo_data_zone0 %>%
  mutate(spp =  "All") %>%
  group_by(year, week, zone, spp) %>%
  summarise(spp = "All",
            across(all_of(sum_col), sum))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------- P I R : A L L  -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
hypo_spp_all_pir = hypo_data_zone0 %>%
  group_by(year, week, zone) %>%
  summarize(spp = "All",
            pir = sum(vi)/sum(abund),
            pir_lci = sum(vi_lci)/sum(abund),
            pir_uci = sum(vi_uci)/sum(abund))

hypo_spp_all = left_join(hypo_spp_all0, hypo_spp_all_pir, by = grp_vars)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------- R B I N D  -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
hypo_data_zone = rbind(hypo_data_zone0, hypo_spp_all) %>%
  mutate(grp = "hypothetical")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------J O I N   R E A L   &    H Y P O   -------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
matching_columns <- intersect(names(real_data_zone), names(hypo_data_zone))


hypo_data_test = rbind(hypo_data_zone, real_data_zone) %>%
  select(all_of(matching_columns)) %>%
  #mutate(vi_uci = if_else(vi == 0, 0, vi_uci)) %>%
  group_by(year, week, zone) %>%
  filter(week > 27) %>%
  mutate(week = factor(week),
         spp = factor(spp, levels = c("Pipiens", "Tarsalis", "All"))) %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>%
  ungroup() %>%
  select(week, spp, grp, vi, vi_lci, vi_uci) %>%
  arrange(week, spp, grp)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------P L O T   -------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

pd = position_dodge(0.3)

p_df_ci_spp = ggplot(hypo_data_test, aes(x = week, y = vi, color = grp)) +
  geom_errorbar(aes(ymin = vi_lci, ymax = vi_uci), size = 0.8, width = .2, position = pd) +
  geom_point(size = 2, position = pd) +
  geom_hline(yintercept = 0) +
  facet_grid(spp~.) +
  theme_minimal() +
  # scale_color_manual(values = c("Pipiens" = "darkred",
  #                               "Tarsalis" = "red",
  #                               "All" = "#ffa590")) +
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "red") +
  #scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  #coord_cartesian(ylim = c(0, 1)) +
  ggtitle("VI 95% CI") +
  labs(y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))


p_df_ci_spp +gridExtra::tableGrob(hypo_data_test)
