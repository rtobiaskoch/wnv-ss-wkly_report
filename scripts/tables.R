source('scripts/config.R')

data_input = check_read_fun(fn_database_update)

data_zone_wk = check_read_fun(fn_data_output)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#TABLE 1A
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

t1a_abund = data_zone_wk %>%
  select(zone, spp, abund) %>%
  pivot_wider(names_from = spp, values_from = abund)




#total indiv examined
#number pools examined
total_examined = data_input %>%
  group_by(year,week,zone,spp) %>% 
  summarise(total = sum(total)) %>%
  pivot_wider(names_from = "spp", values_from = "total")

#number pools examined
pools = data_input %>%
  group_by(year,week,zone,spp) %>% 
  count() %>%
  pivot_wider(names_from = "spp", values_from = "n")
