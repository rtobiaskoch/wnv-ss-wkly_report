source("scripts/config.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get active active trap data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gsheet_pull(trap_active_key, "data", fn_trap_active)
active_trap = read.csv(fn_trap_active) #remove BC & BE because their traps are infrequent

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get active malfunction trap data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gsheet_pull(trap_malfunction_key, "data", fn_trap_malfunction)
malfunction_trap = read.csv(fn_trap_malfunction)

malfunction_trap_test = malfunction_trap %>%
  filter(year == year_filter,
         week == week_filter)

if(nrow(malfunction_trap_test) == 0) {
  paste("Alert! there are no malfunctioned traps for the year and week filter. Sounds too good to be true. 
        Check and update your malfunction trap on gdrive our where ever it may be hosted in your world.")
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FC ZONE TRAP NUMBERS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  
  #FC ROUTINE TRAP NUMBERS
  trap_p_wk_status = active_trap %>%
    mutate(year = year_filter,
           week = week_filter) %>%
    left_join(malfunction_trap, by = c("trap_id", "year", "week")) %>%
    mutate(malfunction = if_else(!is.na(malfunction),malfunction, 0)) %>%
    arrange(desc(active), malfunction)
  
  #get summary of functional traps for the week by zone by subtracting malfunctioning from active
  func_trap_L = trap_p_wk_status %>% 
    group_by(zone, method) %>% 
    summarize(active = sum(active),
              malfunction = sum(malfunction)) %>%
    mutate(trap_L_func = active - malfunction) %>% 
    filter(method == "L")

fc_func_trap_L = func_trap_L %>% 
  filter(zone %in% fc_zones) %>%
  group_by(method) %>%
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
