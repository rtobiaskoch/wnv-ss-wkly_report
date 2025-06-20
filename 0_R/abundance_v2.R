
#ABUNDANCE

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#below is potential modification to the check_read_fun.R
data_input = check_read_fun(fn_database_update, wk = week_filter_yr)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------G E T   F U N C T I O N A L   T R A P S ------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#read in weeks functional traps. derived from get_func_trap.R

func_trap_L = read.csv(fn_func_trap)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>#GET WEEKS TRAP NUMBERS
#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

suppressMessages({
  trap_p_wk0 = data_input %>%
    distinct(year,trap_date, week, zone, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
    group_by(year, zone, week, method) %>% #get number of traps per week per zone
    summarise(n = n()) %>%
    ungroup()%>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "trap_",values_fill = 0) %>%
    mutate(n_trap = trap_L + trap_G)
})
  #GET WEEKS TRAP NUMBERS: FC
  #>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  suppressMessages({
    fc_trap_wk0 = trap_p_wk0 %>% 
      filter(zone %in% fc_zones) %>%
      group_by(year,week) %>%
      summarise(
        zone = "FC",
        trap_G = sum(trap_G, na.rm = TRUE),
        trap_L = sum(trap_L, na.rm = TRUE),
        n_trap = sum(n_trap, na.rm = TRUE)
      )
  } )
  
  #>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #>combine data
  #>#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
  trap_p_wk = rbind(trap_p_wk0, fc_trap_wk0) %>%
    left_join(func_trap_L, by = c("year", "week", "zone")) %>% #merge with the routine functional trap list
    mutate(trap_L_0 = active - trap_L_func - malfunction) %>%
    mutate(func_GT_wk = trap_L_func <= trap_L) %>% #is the routine greater than the weekly? if not there is an error
    mutate(func_GT_wk = if_else(is.na(func_GT_wk), T, func_GT_wk)) %>%
    mutate(trap_L_func = if_else(trap_L_func == 0, trap_L, trap_L_func)) %>% #for WC and BC that don't have active traps
    mutate(zone = factor(zone, levels = zone_lvls)) %>%
    arrange(zone)

#account for boulder with no obvious traps
  BC_trap = trap_p_wk %>%
    filter(zone == "BC") %>%
    mutate(method = if_else(is.na(method), "L", method),
           active = if_else(is.na(active), trap_L, active),
           malfunction = if_else(is.na(malfunction), 0, malfunction),
           trap_L_func = if_else(is.na(trap_L_func), trap_L, trap_L_func),
           trap_L_0    = if_else(is.na(trap_L_0), active-trap_L, trap_L_0)
           )
  
  trap_p_wk = trap_p_wk %>%
    filter(zone != "BC") %>%
    rbind(BC_trap)
  
  write.csv(trap_p_wk, fn_trap_p_wk, row.names = F)
  
  
#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>check trap numbers
#>#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

trap_check_FC_LV =   trap_p_wk %>%
  filter(zone %in% c(fc_zones, "LV"))

#if(any(trap_check_FC_LV$func_GT_wk == F)) {
if(F){
  
  stop("check your trap data. 
        The number of traps in this weeks data is greater than the expected routine traps.")

  } else {
  
  suppressMessages({
  
     
    #ALL ZONES
    #get number of mosquitoes per night per trap
    m_p_wk0 = data_input %>%
      group_by(across(all_of(c(grp_vars, "method")))) %>% #get number of mosquitoes per week per zone per species
      summarize(mosq = sum(total)) %>%
      ungroup()%>%
      pivot_wider(names_from = method, values_from = mosq, 
                  names_prefix = "mosq_", values_fill = 0) %>%
      mutate(mosq = mosq_L + mosq_G)
    
    
    #FC
    #get number of mosquitoes per night per trap
    fc_m_p_wk = m_p_wk0 %>%
      filter(zone %in% fc_zones) %>%
      group_by(year, week, spp) %>%
      summarise(zone = "FC", 
                across(where(is.numeric), ~sum(.x, na.rm = TRUE)))
    
    m_p_wk = rbind(m_p_wk0, fc_m_p_wk)
    
    #get abundance per trap
   abund_zone_wk_with_G = left_join(trap_p_wk, m_p_wk, by = c("year","zone", "week")) %>%
      mutate(abund = round(mosq_L/trap_L_func,2)) #WHERE CALC HAPPENS
    
    abund_zone_wk = abund_zone_wk_with_G %>%
      select(year, week, zone, spp, mosq, mosq_L, trap_L_func, abund) %>%
      mutate(zone = factor(zone, levels = zone_lvls)) %>%
      arrange(zone)
    
  })
  
  
  write.csv(abund_zone_wk, paste0(fn_abund_out, ".csv"), row.names = F)
  
  
} # end of else




