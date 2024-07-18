source("scripts/config.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get active active trap data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gsheet_pull(trap_active_key, "data", fn_trap_active)
active_trap0 = read.csv(fn_trap_active) #remove BC & BE because their traps are infrequent


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get active malfunction trap data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gsheet_pull(trap_malfunction_key, "data", fn_trap_malfunction)
malfunction_trap = read.csv(fn_trap_malfunction)

malfunction_trap_test = malfunction_trap %>%
  filter(year == year_filter,
         week == week_filter)

if(nrow(malfunction_trap_test) == 0) {
  print(paste("Alert! there are no malfunctioned traps for the year and week filter. Sounds too good to be true. 
        Check and update your malfunction trap on gdrive our where ever it may be hosted in your world."))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FC ZONE TRAP NUMBERS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#chagne week filter for expand_grid to get all comination of year and week
week_filter_yr


  # Your code with group_by and summarize
  #get number of traps per night
  
  active_trap = active_trap0 %>% filter(active == 1)
  
  #expand the traps to match the week and year we are interested in 
  active_trap = tidyr::expand_grid(
                trap_id = active_trap$trap_id,
                year = year_filter,
                week = week_filter) %>%
   left_join(active_trap0, by = "trap_id") # add back in the zone, method and active status
  
  
  #FC ROUTINE TRAP NUMBERS
  trap_p_wk_status = active_trap %>%
    left_join(malfunction_trap, by = c("trap_id", "year", "week")) %>%
    mutate(malfunction = if_else(!is.na(malfunction),malfunction, 0)) %>%
    arrange(desc(active), malfunction)
  
  suppressMessages({  
  #get summary of functional traps for the week by zone by subtracting malfunctioning from active
  func_trap_L = trap_p_wk_status %>% 
    group_by(year, week, zone, method) %>% 
    summarize(active = sum(active),
              malfunction = sum(malfunction)) %>%
    mutate(trap_L_func = active - malfunction) %>% 
    filter(method == "L")
})
  
suppressMessages({  
fc_func_trap_L = func_trap_L %>% 
  filter(zone %in% fc_zones) %>%
  group_by(year, week, method) %>%
    summarise(
      zone = "FC",
      active = sum(active, na.rm = TRUE),
      malfunction = sum(malfunction, na.rm = TRUE),
      trap_L_func = sum(trap_L_func, na.rm = TRUE)
    )

func_trap_L2 = rbind(func_trap_L, fc_func_trap_L) %>%
  mutate(zone = factor(zone, levels = zone_lvls)) %>%
  arrange(zone)
  
})

write.csv(func_trap_L2, fn_func_trap, row.names = F)
