source("scripts/config.R")


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CURRENT WEEK
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


week_filter = seq(1, week_filter, by =1)

data_input = check_read_fun(fn_database_update)

#get number of active traps. For the purposes of historical calculations not going to consider malfunctioning traps
func_trap_L = read.csv(fn_func_trap) %>%
  select(zone, active) %>%
  rename(trap_L = "active")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CURRENT WEEK: ABUNDANCE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  
  #get number of mosquitoes per night per trap
  m_p_wk0 = data_input %>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    group_by(week, zone) %>% #get number of mosquitoes per week per zone per species
    summarize(mosq_L = sum(total)) %>%
    ungroup()# %>%
  
  fc_m_p_wk = m_p_wk0 %>%
    filter(zone %in% fc_zones) %>%
    group_by(week) %>%
    summarise(zone = "FC", 
              mosq_L = sum(mosq_L))
  
  m_p_wk = rbind(m_p_wk0, fc_m_p_wk)
  
  #get abundance per trap 
  df_abund = left_join(func_trap_L, m_p_wk, by = "zone") %>%
    mutate(abund = round(mosq_L/trap_L,2)) %>%
    select(-mosq_L, -trap_L) %>%
    complete(zone, week) %>% #fill in missing weeks for the non-routine zones
    filter(!is.na(week))
    # pivot_wider(names_from = zone, 
    #             values_from = abund,
    #             names_prefix = "abund_")
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CURRENT WEEK: PIR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_list = data_input %>%
  # anti_join(gravid_only, by = hx_grp_vars) %>% # remove the wks with only gravid trap
  mutate(grp = paste(week,zone, sep ="-"))


mle = pIR(test_code ~ total|grp, data = data_list, pt.method = "firth")


df_pir0 = as.data.frame(mle) %>%
  separate(grp,
           into = hx_grp_vars,
           sep = "-") %>%
  mutate(week = as.integer(week)) %>%
  rename(pir = P,
         pir_lci = Lower,
         pir_uci = Upper) %>%
  select(-pir_lci, -pir_uci) %>%
  mutate(pir = round(pir,4)) #%>%
  # pivot_wider(names_from = "zone", 
  #             values_from = "pir", 
  #             names_prefix = "pir_") %>%
  # mutate(pir_FC = (pir_NW + pir_NE + pir_SE+ pir_SW)/4)

fc_pir = df_pir0 %>%
  filter(zone %in% fc_zones) %>%
  group_by(week) %>%
  summarise(zone = "FC", 
            pir = mean(pir))

df_pir = rbind(df_pir0, fc_pir) %>%
  complete(zone, week)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CURRENT WEEK: VI/ALL
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all_c_long = df_abund %>%
  left_join(df_pir, by = c("zone", "week")) %>%
  mutate(vi = round(abund * pir,4)) %>%
  pivot_longer(cols = c(abund, pir, vi),
               names_to = "est",
               values_to = "value") %>%
  mutate(type = "current")

df_all_c = df_abund %>%
  left_join(df_pir, by = c("zone", "week")) %>%
  mutate(vi = round(abund * pir,4)) %>%
  pivot_wider(names_from = zone, 
              values_from = c(abund, pir, vi)
              )

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GET HISTORICAL CALCULATIONS
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# HX: ABUNDANCE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

year_filter = seq(year_filter-5, year_filter-1, by = 1)
week_filter = 20:40

data_input = check_read_fun(fn_database_update)

#get number of active traps. For the purposes of historical calculations not going to consider malfunctioning traps
func_trap_L = read.csv(fn_func_trap) %>%
  select(zone, active) %>%
  rename(trap_L = "active")

suppressMessages({
#get non 0 traps for non-routine areas like BC and WC
trap_p_wk = data_input %>%
  filter(method == "L") %>%
  filter(zone %in% non_routine_zones) %>%
  distinct(year,trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
  group_by(year, zone, week) %>% #get number of traps per week per zone
  summarise(trap_L = n()) %>%
  ungroup() %>%
  group_by(zone, week) %>% #get the average number of traps otherwise it will count too many
  summarize(trap_L = mean(trap_L)) %>% 
  ungroup()
})

suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  
  #get number of mosquitoes per night per trap
  m_p_wk0 = data_input %>%
    filter(method != "G") %>% #remove gravid traps for abundance calculation
    group_by(year,week, zone) %>% #get number of mosquitoes per week per zone per species
    summarize(mosq_L = sum(total)) %>%
    ungroup() %>%
    group_by(week, zone) %>% #need average for all years
    summarise(mosq_L = mean(mosq_L)) %>%
    ungroup()# %>%
})


suppressMessages({ 
  fc_m_p_wk = m_p_wk0 %>%
    filter(zone %in% fc_zones) %>%
    group_by(week) %>% #get number of mosquitoes per week per zone per species
    summarize(zone = "FC",
              mosq_L = mean(mosq_L)) %>%
    ungroup()# %>%
  
  m_p_wk = rbind(m_p_wk0, fc_m_p_wk)
})  


suppressMessages({ 
  #get abundance per trap 
  df_abund = left_join(func_trap_L, m_p_wk, by = "zone")
  
  df_abund = 
    rquery::natural_join(trap_p_wk, df_abund, jointype = "FULL",
                         by = c("week", "zone")) %>% #get BC and WC trap_L
    mutate(abund = round(mosq_L/trap_L,2)) %>%
    complete(zone, week) %>% #fill in missing weeks for the non-routine zones
    filter(!is.na(week)) %>%
    select(-mosq_L, -trap_L)
  # pivot_wider(names_from = zone, 
  #             values_from = abund,
  #             names_prefix = "abund_")
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# HX: PIR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_list = data_input %>%
  # anti_join(gravid_only, by = hx_grp_vars) %>% # remove the wks with only gravid trap
  mutate(grp = paste(week,zone, sep ="-"))


mle = pIR(test_code ~ total|grp, data = data_list, pt.method = "firth")


df_pir0 = as.data.frame(mle) %>%
  separate(grp,
           into = hx_grp_vars,
           sep = "-") %>%
  transmute(zone = zone,
            week = as.integer(week),
            pir = round(P,4))
# pivot_wider(names_from = "zone", 
#             values_from = "pir", 
#             names_prefix = "pir_") %>%
# mutate(pir_FC = (pir_NW + pir_NE + pir_SE+ pir_SW)/4)

fc_pir = df_pir0 %>%
  filter(zone %in% fc_zones) %>%
  group_by(week) %>%
  summarise(zone = "FC", 
            pir = round(mean(pir),4))

df_pir = rbind(df_pir0, fc_pir) %>%
  complete(zone, week)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# HX: VI/ALL
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all_hx_long = df_abund %>%
  left_join(df_pir, by = c("zone", "week")) %>%
  mutate(vi = round(abund * pir,4))  %>%
  pivot_longer(cols = c(abund, pir, vi),
               names_to = "est",
               values_to = "value") %>%
  mutate(type = "hx")

df_all_hx = df_abund %>%
  left_join(df_pir, by = c("zone", "week")) %>%
  mutate(vi = round(abund * pir,4))  %>%
  pivot_wider(names_from = zone, 
              values_from = c(abund, pir, vi),
              names_prefix = "hx_"
  )


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: ABUNDANCE
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all = df_all_hx %>% left_join(df_all_c, by = "week")

hx_abund_report = df_all %>%
  arrange(week) %>%
  select(c( "week",
            "abund_NW", "abund_hx_NW", 
            "abund_NE", "abund_hx_NE", 
            "abund_SE", "abund_hx_SE", 
            "abund_SW", "abund_hx_SW", 
            "abund_FC", "abund_hx_FC", 
            "abund_LV", "abund_hx_LV", 
            "abund_BE", "abund_hx_BC"
  ))%>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: PIR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PIR with current wk
hx_pir_report = df_all %>%
  mutate(across(-week, ~ . * 1000)) %>% #get it into the format of the report
  arrange(week) %>%
  select(c( "week",
    "pir_NW", "pir_hx_NW", 
    "pir_NE", "pir_hx_NE", 
    "pir_SE", "pir_hx_SE", 
    "pir_SW", "pir_hx_SW", 
    "pir_FC", "pir_hx_FC", 
    "pir_LV", "pir_hx_LV", 
    "pir_BE", "pir_hx_BC"
  ))%>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: vi
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#vi with current wk
hx_vi_report = df_all %>%
  arrange(week) %>%
  select(c( "week",
            "vi_NW", "vi_hx_NW", 
            "vi_NE", "vi_hx_NE", 
            "vi_SE", "vi_hx_SE", 
            "vi_SW", "vi_hx_SW", 
            "vi_FC", "vi_hx_FC", 
            "vi_LV", "vi_hx_LV", 
            "vi_BE", "vi_hx_BC"
  )) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))


write.csv(hx_vi_report, "data_output/table1b_hx_vi.csv", row.names = F)
write.csv(hx_abund_report, "data_output/table2b_hx_abund.csv", row.names = F)
write.csv(hx_pir_report, "data_output/table3b_hx_pir.csv", row.names = F)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: VIZUALIZATION
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df_all_long = rbind(df_all_c_long, df_all_hx_long) %>%
  mutate(value = if_else(est == "pir", value * 3000, value),
         value = if_else(est == "vi", value * 200, value),
         type = factor(type, levels = c("hx", "current")),
         zone = factor(zone, levels = zone_lvls))

p_df_all_long = ggplot(df_all_long, aes(x = week, y = value, 
                                        color = type, fill = type,group = type)) +
  geom_area(alpha = 0.3) +
  facet_grid(zone ~ est) +
  theme_classic() +
  scale_color_manual(values = c("hx" = "grey50",
                                "current" = "red")) +
  scale_fill_manual(values = c("hx" = "grey50",
                                "current" = "red"))
    
p_df_all_long   

ggsave("data_output/plots/hx_plot.png", p_df_all_long)
    
    
    
