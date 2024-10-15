list2env(readRDS("data_input/config_params.RDS"),           envir = .GlobalEnv)

#get number of expected active traps
active_trap0 = read.csv(fn_trap_active) %>%
  filter(active == 1) %>%
  group_by(zone, method) %>% 
  count()

trap_p_wk = read.csv(fn_trap_p_wk)

p_trap_QC = trap_p_wk %>%
  select(-func_GT_wk, -trap_L_func, -method) %>%
  filter(week == week_filter) %>%
  pivot_longer(cols = -c(year, week, zone, active),
               names_to = "status",
               values_to = "n")



  ggplot(aes(status, n)) +
  geom_col() +
  facet_wrap(~zone) +
  geom_text(aes(label = n), color = "white", vjust = -0.5) +
  theme_classic()
p_trap_QC
