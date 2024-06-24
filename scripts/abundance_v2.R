source("scripts/config.R")

#ABUNDANCE
#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = check_read_fun(fn_database_update)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#get traps to merge weekly data to.
#this is done because traps with 0 are not reported. 
gsheet_pull(trap_gsheet_key, "data", fn_trap)
trap_data = read.csv(fn_trap)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get active malfunction trap data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gsheet_pull(trap_active_key, "data", fn_trap_active)
active_trap = read.csv(fn_trap_active) #remove BC & BE because their traps are infrequent


gsheet_pull(trap_malfunction_key, "data", fn_trap_malfunction)
malfunction_trap = read.csv(fn_trap_malfunction)

  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FC ZONE TRAP NUMBERS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
# Your code with group_by and summarize
#get number of traps per night
  
#FC ROUTINE TRAP NUMBERS
trap_p_wk_status = trap_data %>%
  select(trap_id, zone) %>%
   mutate(year = year_filter,
          week = week_filter) %>% #may need to write something to account for multiple weeks
  left_join(active_trap, by = "trap_id") %>%
  mutate(malfunction = if_else(trap_id %in% malfunction_trap$trap_id, 1, 0)) %>%
  mutate(method = if_else(str_detect(trap_id, regex("GR", ignore_case = T)), "G", "L")) %>%
  arrange(desc(active), malfunction)

expected_trap_L = trap_p_wk_status %>%
   group_by(zone, method) %>% 
   summarize(active = sum(active),
             malfunction = sum(malfunction)) %>%
  mutate(n_trap = active - malfunction) %>%
  filter(method == "L") %>%
  filter(zone %in% c(fc_zones, "LV"))

trap_p_wk_routine = trap_p_wk_status %>%
  filter(active == 1 & malfunction == 0) %>%
  filter(zone %in% c(fc_zones, "LV")) %>% # filtering for FC zones because we know the routines traps for certain
  group_by(year, zone, week, method) %>% #get number of traps per week per zone
  summarise(n = n()) %>%
  ungroup()%>%
  pivot_wider(names_from = method, values_from = n, 
                names_prefix = "trap_",values_fill = 0) %>%
  mutate(n_trap_routine = trap_L + trap_G) #change name from n_trap so it can merge with the weekly data

})

suppressMessages({
#NON WEEKLY TRAP NUMBERS
  trap_p_wk0 = data_input %>%
  distinct(year,trap_date, week, zone, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
  group_by(year, zone, week, method) %>% #get number of traps per week per zone
  summarise(n = n()) %>%
  ungroup()%>%
  pivot_wider(names_from = method, values_from = n, 
              names_prefix = "trap_",values_fill = 0) %>%
  mutate(n_trap = trap_L + trap_G)

})

trap_check = trap_p_wk0 %>%
  left_join(trap_p_wk_routine %>% select(zone, n_trap_routine), by = "zone") %>%
  mutate(trap_0 = n_trap - n_trap) %>%
  mutate(routine_GT_wk = n_trap_routine >= n_trap) %>% #is the routine greater than the weekly? if not there is an error
  mutate(routine_GT_wk = if_else(is.na(routine_GT_wk), T, routine_GT_wk))
  
if(any(trap_check$routine_GT_wk == F)) {
  
  print("check your trap data. 
        The number of traps in this weeks data is greater than the expected routine traps.")

  } else {
  
  suppressMessages({
  
    
    trap_p_wk = trap_p_wk_routine %>%
      rquery::natural_join(trap_p_wk0, 
                           by = "zone",
                           jointype = "FULL")
    
    #get number of mosquitoes per night per trap
    m_p_wk = data_input %>%
      group_by(across(all_of(c(grp_vars, "method")))) %>% #get number of mosquitoes per week per zone per species
      summarize(mosq = sum(total)) %>%
      ungroup()%>%
      pivot_wider(names_from = method, values_from = mosq, 
                  names_prefix = "mosq_", values_fill = 0) %>%
      mutate(mosq = mosq_L + mosq_G)
    
    #get abundance per trap
    abund_zone_wk = left_join(trap_p_wk, m_p_wk, by = c("year","zone", "week")) %>%
      mutate(abund = round(mosq_L/trap_L,2))
  })
  
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    #for all of fort collins
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    suppressMessages({
      # Your code with group_by and summarize
      #get number of traps per night
      #need to recalculate n_traps because you can add up all zones because you get duplicates with spp
      
      fc_trap_p_wk = trap_p_wk_routine %>%
        filter(zone %in% fc_zones) %>%
        group_by(year, week) %>%
        summarize(zone = "FC",
                  trap_L = sum(trap_L),
                  trap_G = sum(trap_G),
                  n_trap = sum(n_trap))
        
        
      
      #get number of mosquitoes per night per trap
      fc_m_p_wk = m_p_wk %>%
        filter(zone %in% fc_zones) %>%
        group_by(year, week, spp) %>%
        summarise(zone = "FC", 
                  across(where(is.numeric), ~sum(.x, na.rm = TRUE)))
      
      #get abundance per trap
      fc_abund_zone_wk = left_join(fc_trap_p_wk, fc_m_p_wk, by = c("year","zone", "week")) %>%
        mutate(abund = round(mosq_L/trap_L,2))
    })
    
    
    abund_zone_wk2 = rbind(abund_zone_wk, fc_abund_zone_wk) %>%
      mutate(zone = factor(zone, levels = zone_lvls)) %>%
      arrange(zone)
  
  write.csv(abund_zone_wk2, paste0(fn_abund_out, ".csv"), row.names = F)
  
  
  
} # end of else




