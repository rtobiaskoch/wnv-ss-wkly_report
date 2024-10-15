list2env(readRDS("data_input/config_params.RDS"),           envir = .GlobalEnv)

database = read.csv(fn_database_update)

t = database %>% 
  distinct(trap_id, year, week, zone)%>% 
  group_by(year, zone, trap_id) %>% 
  count() %>%  #get weeks
  pivot_wider(names_from = year, 
              values_from = n, #number of weeks trap had mosquitoes for each year
              values_fill = 0)


inactive = t %>% 
  filter(`2023` == 0 & `2024` == 0) 

active = t %>%
  filter(`2023`  > 0 & `2024` > 0) 

write.csv(inactive, fn_inactive_trap, row.names = F)

