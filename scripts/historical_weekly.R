source("scripts/config.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get current year calculations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
week_filter = seq(1, week_filter, by =1)

data_input = check_read_fun(fn_database_update)

suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  trap_p_wk = data_input %>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    distinct(trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools
    group_by(zone, week) %>% #get number of traps per week per zone
    summarise(n_trap = n()) %>%
    ungroup()
  
  #get number of mosquitoes per night per trap
  m_p_wk = data_input%>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    group_by(week, zone) %>% #get number of mosquitoes per week per zone per species
    summarize(mosq = sum(total)) %>%
    ungroup()# %>%
  
  #get abundance per trap
  abund_zone_wk = left_join(trap_p_wk, m_p_wk, by = hx_grp_vars) %>%
    mutate(abund = round(mosq/n_trap,1)) %>%
    select(-n_trap,-mosq) %>%
    pivot_wider(names_from = zone, 
                values_from = abund,
                names_prefix = "abund_")
})

#Historical PIR and VI


data_list = data_input %>%
  # anti_join(gravid_only, by = hx_grp_vars) %>% # remove the wks with only gravid trap
  mutate(grp = paste(week,zone, sep ="-"))


mle = pIR(test_code ~ total|grp, data = data_list, pt.method = "firth")


df_pir = as.data.frame(mle) %>%
  separate(grp,
           into = hx_grp_vars,
           sep = "-") %>%
  mutate(week = as.integer(week)) %>%
  rename(pir = P,
         pir_lci = Lower,
         pir_uci = Upper) %>%
  select(-pir_lci, -pir_uci) %>%
  mutate(pir = round(pir,4)*1000) %>%
  pivot_wider(names_from = "zone", 
              values_from = "pir", 
              names_prefix = "pir_")


current_wk = abund_zone_wk %>%
  left_join(df_pir, by = "week") %>%
  mutate(abund_FC = (abund_NW + abund_NE + abund_SE+ abund_SW)/4,
         pir_FC = (pir_NW + pir_NE + pir_SE+ pir_SW)/4
  )







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get historical calculations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



year_filter = seq(year_filter-5, year_filter-1, by = 1)
week_filter = 20:40

data_input = check_read_fun(fn_database_update)

suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  trap_p_wk = data_input %>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    distinct(trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools
    group_by(zone, week) %>% #get number of traps per week per zone
    summarise(n_trap = n()) %>%
    ungroup()
  
  #get number of mosquitoes per night per trap
  m_p_wk = data_input%>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    group_by(week, zone) %>% #get number of mosquitoes per week per zone per species
    summarize(mosq = sum(total)) %>%
    ungroup()# %>%
  
  #get abundance per trap
  abund_zone_wk = left_join(trap_p_wk, m_p_wk, by = hx_grp_vars) %>%
    mutate(abund = round(mosq/n_trap,1)) %>%
    select(-n_trap,-mosq) %>%
    pivot_wider(names_from = zone, 
                values_from = abund,
                names_prefix = "hx_abund_")
})

#Historical PIR and VI


data_list = data_input %>%
 # anti_join(gravid_only, by = hx_grp_vars) %>% # remove the wks with only gravid trap
  mutate(grp = paste(week,zone, sep ="-"))


mle = pIR(test_code ~ total|grp, data = data_list, pt.method = "firth")


df_pir = as.data.frame(mle) %>%
  separate(grp,
           into = hx_grp_vars,
           sep = "-") %>%
  mutate(week = as.integer(week)) %>%
  rename(pir = P,
         pir_lci = Lower,
         pir_uci = Upper) %>%
  select(-pir_lci, -pir_uci) %>%
  mutate(pir = round(pir,4)*1000) %>%
  pivot_wider(names_from = "zone", 
              values_from = "pir", 
              names_prefix = "hx_pir_")


#Abundance with current wk


hx_abund_wk = abund_zone_wk %>%
  left_join(current_wk, by = "week") %>%
  select(-starts_with("pir"), -ends_with('NA')) %>%
  mutate(hx_abund_FC = (hx_abund_NW + hx_abund_NE + hx_abund_SE+ hx_abund_SW)/4) %>%
  arrange(week) %>%
  select(c( "week",
            "abund_NW", "hx_abund_NW", 
            "abund_NE", "hx_abund_NE", 
            "abund_SE", "hx_abund_SE", 
            "abund_SW", "hx_abund_SW", 
            "abund_FC", "hx_abund_FC", 
            "abund_LV", "hx_abund_LV", 
            "abund_BE", "hx_abund_BC"
  ))



#PIR with current wk
hx_pir_wk = df_pir %>%
  left_join(current_wk, by = "week") %>%
  select(-starts_with("abund"), -ends_with('NA')) %>%
  mutate(hx_pir_FC = (hx_pir_NW + hx_pir_NE + hx_pir_SE+ hx_pir_SW)/4) %>%
  arrange(week) %>%
  select(c( "week",
    "pir_NW", "hx_pir_NW", 
    "pir_NE", "hx_pir_NE", 
    "pir_SE", "hx_pir_SE", 
    "pir_SW", "hx_pir_SW", 
    "pir_FC", "hx_pir_FC", 
    "pir_LV", "hx_pir_LV", 
    "pir_BE", "hx_pir_BC"
  ))




write.csv(hx_abund_wk, "data_output/hx_abund_wk.csv", row.names = F)
write.csv(hx_pir_wk, "data_output/hx_pir_wk.csv", row.names = F)

