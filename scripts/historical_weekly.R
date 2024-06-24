source("scripts/config.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get current year by week abundance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
week_filter = seq(1, week_filter, by =1)

data_input = check_read_fun(fn_database_update)

abund_zone_wk2 = read.csv(paste0(fn_abund_out, ".csv"))

suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  trap_p_wk = data_input %>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    distinct(trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools
    group_by(zone, week) %>% #get number of traps per week per zone
    summarise(trap_L = n()) %>%
    ungroup()
  
  #get number of mosquitoes per night per trap
  m_p_wk = data_input %>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    group_by(week, zone) %>% #get number of mosquitoes per week per zone per species
    summarize(mosq_L = sum(total)) %>%
    ungroup()# %>%
  
  #get abundance per trap
  df_abund = left_join(trap_p_wk, m_p_wk, by = hx_grp_vars) %>%
    mutate(abund = round(mosq_L/trap_L,2)) %>%
    select(-trap_L,-mosq_L) %>%
    pivot_wider(names_from = zone, 
                values_from = abund,
                names_prefix = "abund_") %>%
   mutate(abund_FC = (abund_NW + abund_NE + abund_SE+ abund_SW)/4)
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get current year by week PIR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


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
  mutate(pir = round(pir,4)) %>%
  pivot_wider(names_from = "zone", 
              values_from = "pir", 
              names_prefix = "pir_") %>%
  mutate(pir_FC = (pir_NW + pir_NE + pir_SE+ pir_SW)/4)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get current year by week PIR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get VI by separating abund and PIR and 
df_vi = df_abund * df_pir 
df_vi[is.na(df_vi)] = 0
colnames(df_vi) = str_replace(colnames(df_vi), "abund", "vi")

df_vi = df_vi %>% 
  mutate(week = df_pir$week) %>%
  mutate(vi_FC = (vi_NW + vi_NE + vi_SE+ vi_SW)/4)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#combine to get current year data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# current_wk = df_abund %>%
#   left_join(df_pir, by = "week") %>%
#   left_join(df_vi, by = "week") %>%
#   mutate(abund_FC = (abund_NW + abund_NE + abund_SE+ abund_SW)/4,
#          pir_FC = (pir_NW + pir_NE + pir_SE+ pir_SW)/4,
#          vi_FC = (vi_NW + vi_NE + vi_SE+ vi_SW)/4
#   )


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GET HISTORICAL CALCULATIONS: ABUNDANCE
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
    summarise(trap_L = n()) %>%
    ungroup()
  
  #get number of mosquitoes per night per trap
  m_p_wk = data_input %>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    group_by(week, zone) %>% #get number of mosquitoes per week per zone per species
    summarize(mosq_L = sum(total)) %>%
    ungroup()# %>%
  
  #get abundance per trap
  df_abund_hx = left_join(trap_p_wk, m_p_wk, by = hx_grp_vars) %>%
    mutate(abund = round(mosq_L/trap_L,2)) %>%
    select(-trap_L,-mosq_L) %>%
    pivot_wider(names_from = zone, 
                values_from = abund,
                names_prefix = "hx_abund_") %>%
    mutate(hx_abund_FC = (hx_abund_NW + hx_abund_NE + hx_abund_SE+ hx_abund_SW)/4)
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GET HISTORICAL CALCULATIONS: PIR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

data_list = data_input %>%
 # anti_join(gravid_only, by = hx_grp_vars) %>% # remove the wks with only gravid trap
  mutate(grp = paste(week,zone, sep ="-"))


mle = pIR(test_code ~ total|grp, data = data_list, pt.method = "firth")


df_pir_hx = as.data.frame(mle) %>%
  separate(grp,
           into = hx_grp_vars,
           sep = "-") %>%
  mutate(week = as.integer(week)) %>%
  rename(pir = P,
         pir_lci = Lower,
         pir_uci = Upper) %>%
  select(-pir_lci, -pir_uci) %>%
  mutate(pir = round(pir,4)) %>%
  pivot_wider(names_from = "zone", 
              values_from = "pir", 
              names_prefix = "hx_pir_")%>%
  mutate(hx_pir_FC = (hx_pir_NW + hx_pir_NE + hx_pir_SE+ hx_pir_SW)/4)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GET HISTORICAL CALCULATIONS: VI
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get VI by separating abund and PIR and 
df_vi_hx = df_abund_hx * df_pir_hx

df_vi_hx[is.na(df_vi_hx)] = 0

colnames(df_vi_hx) = str_replace(colnames(df_vi_hx), "abund", "vi")

df_vi_hx = df_vi_hx %>% 
  mutate(week = df_pir_hx$week) %>%
  mutate(hx_vi_FC = (hx_vi_NW + hx_vi_NE + hx_vi_SE+ hx_vi_SW)/4)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: ABUNDANCE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
hx_abund_report = df_abund_hx %>%
  left_join(df_abund, by = "week") %>%
 # mutate(hx_abund_FC = (hx_abund_NW + hx_abund_NE + hx_abund_SE+ hx_abund_SW)/4) %>%
  arrange(week) %>%
  select(c( "week",
            "abund_NW", "hx_abund_NW", 
            "abund_NE", "hx_abund_NE", 
            "abund_SE", "hx_abund_SE", 
            "abund_SW", "hx_abund_SW", 
            "abund_FC", "hx_abund_FC", 
            "abund_LV", "hx_abund_LV", 
            "abund_BE", "hx_abund_BC"
  ))%>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: PIR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PIR with current wk
hx_pir_report = df_pir_hx %>%
  left_join(df_pir, by = "week") %>%
  mutate(across(-week, ~ . * 1000)) %>% #get it into the format of the report
  arrange(week) %>%
  select(c( "week",
    "pir_NW", "hx_pir_NW", 
    "pir_NE", "hx_pir_NE", 
    "pir_SE", "hx_pir_SE", 
    "pir_SW", "hx_pir_SW", 
    "pir_FC", "hx_pir_FC", 
    "pir_LV", "hx_pir_LV", 
    "pir_BE", "hx_pir_BC"
  ))%>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: vi
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#vi with current wk
hx_vi_report = df_vi_hx %>%
  left_join(df_vi, by = "week") %>%
  arrange(week) %>%
  select(c( "week",
            "vi_NW", "hx_vi_NW", 
            "vi_NE", "hx_vi_NE", 
            "vi_SE", "hx_vi_SE", 
            "vi_SW", "hx_vi_SW", 
            "vi_FC", "hx_vi_FC", 
            "vi_LV", "hx_vi_LV", 
            "vi_BE", "hx_vi_BC"
  )) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))



write.csv(hx_vi_report, "data_output/table1b_hx_vi.csv", row.names = F)
write.csv(hx_abund_report, "data_output/table2b_hx_abund.csv", row.names = F)
write.csv(hx_pir_report, "data_output/table3b_hx_pir.csv", row.names = F)


