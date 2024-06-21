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

inactive_trap = read.csv(fn_inactive_trap) %>%
  filter(zone != "BC") #remove BC & BE because their traps are infrequent
  
active_trap = trap_data %>%
  anti_join(inactive_trap, by = "trap_id")


gsheet_pull(trap_malfunction_key, "data", fn_trap_malfunction)
malfunction_trap = read.csv(fn_trap_malfunction)

  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#for each zone
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


suppressMessages({
# Your code with group_by and summarize
#get number of traps per night
trap_p_wk = data_input %>%
  distinct(year,trap_date, week, zone, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
  group_by(year, zone, week, method) %>% #get number of traps per week per zone
  summarise(n = n()) %>%
  ungroup()%>%
  pivot_wider(names_from = method, values_from = n, 
                names_prefix = "trap_",values_fill = 0) %>%
  mutate(n_trap = trap_L + trap_G)


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
  fc_trap_p_wk = data_input %>%
    filter(zone %in% fc_zones) %>%
    distinct(year,trap_date, week, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
                                                        #and remove zones to get all FC
    group_by(year, week, method) %>% #get number of total traps by counting instances. remove zone to get all FC
    summarise(zone = "FC", #create zone for merging
              n = n()) %>%
    ungroup()%>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "trap_",values_fill = 0) %>%
    mutate(n_trap = trap_L + trap_G)
  
  
  #get number of mosquitoes per night per trap
  fc_m_p_wk = m_p_wk %>%
    group_by(year, week, spp) %>%
    summarise(zone = "FC", 
              across(where(is.numeric), ~sum(.x, na.rm = TRUE)))
  
  #get abundance per trap
  fc_abund_zone_wk = left_join(fc_trap_p_wk, fc_m_p_wk, by = c("year","zone", "week")) %>%
    mutate(abund = round(mosq_L/trap_L,2))
})


abund_zone_wk2 = rbind(abund_zone_wk, fc_abund_zone_wk)

write.csv(abund_zone_wk2, paste0(fn_abund_out, ".csv"), row.names = F)


