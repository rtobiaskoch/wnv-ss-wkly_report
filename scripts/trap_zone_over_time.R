list2env(readRDS("data_input/config_params.RDS"),           envir = .GlobalEnv)

database = read.csv(fn_database_update)

trap_zone_yr = database %>% 
  filter(method == "L") %>%
  filter(year != 2014 & week != 23) %>% #drop 2014 bc only wk 30 showing up and week 23 bc many zones don't have much data
  distinct(trap_id, year, week, zone)%>% # remove the pools to get traps
  group_by(year, zone, week) %>%#remove trap id to get count of traps per zone per week per year
  count() %>%  #get # traps
  arrange(year, zone, week) |>
  ungroup()

max_trap_zone_yr = trap_zone_yr %>%
  group_by(zone, year) %>%
  summarize(max = max(n),
            min = min(n),
            mean = mean(n),
            median = median(n)) |> 
  arrange(zone, year)

write.csv(max_trap_zone_yr, fn_max_trap_yr, row.names = F)


trap_zone_yr_week = trap_zone_yr %>%
  pivot_wider(names_from = week, 
              values_from = n, #number of weeks trap had mosquitoes for each year
              values_fill = 0,
              names_prefix = "w")

