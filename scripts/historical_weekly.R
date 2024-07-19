source("scripts/config.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CURRENT YEAR: WIDER
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


current_year0 =  check_read_fun(fn_data_output, wk = week_filter_yr)


current_year_wide = current_year0 %>%
  filter(spp == "All") %>%
  mutate(week = factor(week)) %>%
  select(week, zone, abund, pir, vi) %>%
  pivot_wider(names_from = zone, 
              values_from = c(abund, pir, vi))

current_year_long = current_year0 %>%
  select(grp_vars, abund, pir, pir_lci, pir_uci, vi, vi_lci, vi_uci) %>%
  pivot_longer(cols = -c(grp_vars),
               names_to = "est",
               values_to = "value") %>%
  mutate(type = "current")

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GET HISTORICAL CALCULATIONS
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#GET DATA
year_filter = year_filter_hx
week_filter = 23:37

data_input = check_read_fun(fn_database_update)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------------- H X: A  B U N D A N C E ------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#--------------------------------T R A P S ----------------------------------------------
#GET KNOWN ROUTINE TRAPS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get number of active traps. For the purposes of historical calculations not going to consider malfunctioning traps


gsheet_pull(routine_trap_tl_key, sheet = "data", out_fn = "data_input/routine_trap_yr.csv")
routine_traps = read.csv("data_input/routine_trap_yr.csv") %>%
  filter(year %in% year_filter)

#GET NON-ROUTINE TRAPS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get non 0 traps for non-routine areas like BC
suppressMessages({
   non_routine = data_input %>%
    filter(method == "L") %>%
    filter(zone %in% non_routine_zones) %>%
    distinct(year,trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
    group_by(year, zone, week) %>% #get number of traps per week per zone
    summarise(trap_L = n()) %>%
    ungroup() %>%
    group_by(year, zone) %>% #get the average number of traps otherwise it will count too many
    summarize(trap_L = mean(trap_L)) %>%
    ungroup()
})

trap_data = rbind(non_routine, routine_traps)


#--------------------------------M O Z Z I E S ----------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  
  #get number of mosquitoes per zone per week per year
  m_p_wk0 = data_input %>%
    ungroup() %>%
    filter(method == "L") %>% #remove gravid traps for abundance calculation
    group_by(year,week,zone,spp) %>% #calc number of mosquitoes per week per zone
    summarize(mosq_L = sum(total)) %>%
    ungroup()
})

#HX: GET MOSQUITOES FOR CITYWIDE FC
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({ 
  fc_m_p_wk = m_p_wk0 %>%
    filter(zone %in% fc_zones) %>% # keep only FC zones
    group_by(year,week,spp) %>% #get number of mosquitoes per week per zone per species
    summarize(zone = "FC",
              mosq_L = sum(mosq_L)) %>% # changed from the mean which would be incorrect 
                                        #because I compare it to total number of traps per zone not the average
    ungroup() # %>%
  
  m_p_wk = rbind(m_p_wk0, fc_m_p_wk)
})  


suppressMessages({ 
  #get abundance per trap 
  df_abund = left_join(m_p_wk, trap_data, by = c("year", "zone")) %>%
    mutate(abund = round(mosq_L/trap_L,2)) %>%
    complete(zone, week) %>% #fill in missing weeks for the non-routine zones
    filter(!is.na(week)) #%>%
    #select(-mosq_L, -trap_L)
  # pivot_wider(names_from = zone, 
  #             values_from = abund,
  #             names_prefix = "abund_")
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- C A L C   P I R:  A L L  --------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#create a grouping variable for mle
data_input = data_input %>%
  arrange(across(all_of(grp_vars))) %>% #dont split by method because PIR includes gravid traps
  mutate(grp = paste(year,week,zone,spp, sep ="-"))

#run pIR
mle = pIR(test_code ~ total|grp, data = data_input, pt.method = "firth")


#create pIR dataframe
df_pir0 = as.data.frame(mle) %>%
  separate(grp,
           into = c("year", "week", "zone", "spp"),
           sep = "-") %>%
  transmute(year = as.integer(year),
            week = as.integer(week),
            zone = zone,
            spp = spp,
            pir = round(P,4),
            pir_lci = round(Lower,4),
            pir_uci = round(Upper,4)
  )

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- C A L C   P I R:  F C -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#calculate FC row for sum
suppressMessages({
  fc_pir0 = data_input %>%
    filter(zone %in% fc_zones) %>% #keep only fc zones
    mutate(zone = "FC") %>% #change zone to be FC
    mutate(grp = paste(year,week,zone,spp, sep ="-"))
  
  mle = pIR(test_code ~ total|grp, data = fc_pir0, pt.method = "firth")
  
  fc_pir0 =  as.data.frame(mle) %>%
    separate(grp,
             into = c("year", "week", "zone", "spp"),
             sep = "-") %>%
    transmute(year = as.integer(year),
              week = as.integer(week),
              zone = zone,
              spp = spp,
              pir = round(P,4),
              pir_lci = round(Lower,4),
              pir_uci = round(Upper,4)
    )
    })
  
  

df_pir = rbind(df_pir0, fc_pir0)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------- C O M B I N E   D A T A -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_zone_wk0 = df_abund %>%
  left_join(df_pir, by = grp_vars) %>%
  mutate(vi = round(abund * pir,4),
         vi_lci = round(abund * pir_lci,4),
         vi_uci = round(abund * pir_uci,4)) %>%
  mutate(year = factor(year),
         week = factor(week),
         zone = factor(zone, levels = zone_lvls),
         spp = factor(spp, levels = c("Pipiens", "Tarsalis", "All"))) %>%
  arrange(zone, spp)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- S U M    A L L   S P P ----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
sum_col = c("mosq_L", 
            "abund", 
            "vi", "vi_lci", "vi_uci")

distinct_col = c("trap_L")

suppressMessages({
  data_zone_wk_spp_all0 = data_zone_wk0 %>%
    mutate(spp =  "All") %>%
    group_by(year, week, zone, spp) %>%
    summarise(spp = "All",
              across(all_of(sum_col), sum),
              across(all_of(distinct_col), ~max(.))
    )
  
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- C A L C   P I R   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>å

suppressMessages({
  pir_all_spp = data_zone_wk0 %>%
    group_by(year, week, zone) %>%
    summarize(spp = "All",
              pir = sum(vi)/sum(abund),
              pir_lci = sum(vi_lci)/sum(abund),
              pir_uci = sum(vi_uci)/sum(abund))
  
})



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- J O I N   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_zone_wk_spp_all = left_join(data_zone_wk_spp_all0, pir_all_spp, by = grp_vars)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- C O M B I N E   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_zone_wk = rbind(data_zone_wk0, data_zone_wk_spp_all) %>% 
  arrange(year,week, zone , spp)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----------------------H X :   V I / A L L ---------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all_hx_long = data_zone_wk %>%
  select(-mosq_L, -trap_L) %>%
  pivot_longer(cols = -c(grp_vars),
               names_to = "est",
               values_to = "value") %>%
  mutate(type = "hx")

df_hx_wide = data_zone_wk %>%
  select(year, week, zone, spp, abund, pir, vi) %>%
  group_by(zone, week, spp) %>%
  summarise(abund = mean(abund), 
            pir = mean(pir), 
            vi = mean(vi)) %>%
  filter(spp == "All") %>%
  select(-spp) %>%
  pivot_wider(names_from = zone, 
              values_from = c(abund, pir, vi),
              names_prefix = "hx_")

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: ABUNDANCE
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all = df_hx_wide %>% left_join(current_year_wide, by = "week") %>% ungroup()

hx_abund_report = df_all %>%
  arrange(week) %>%
  select(c( "week",
            "abund_NW", "abund_hx_NW", 
            "abund_NE", "abund_hx_NE", 
            "abund_SE", "abund_hx_SE", 
            "abund_SW", "abund_hx_SW", 
            "abund_FC", "abund_hx_FC", 
            "abund_LV", "abund_hx_LV", 
            "abund_BE", "abund_hx_BE",
            "abund_BC", "abund_hx_BC"
  )) %>%
  mutate(across(.cols = -week, ~ ifelse(is.na(.), "", .)))


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
    "pir_BE", "pir_hx_BE",
    "pir_BC", "pir_hx_BC"
  ))%>%
  mutate(across(.cols = -week, ~ ifelse(is.na(.), "", .)))

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
            "vi_BE", "vi_hx_BE",
            "vi_BC", "vi_hx_BC"
  )) %>%
  mutate(across(.cols = -week, ~ ifelse(is.na(.), "", .)))


write.csv(hx_vi_report, "data_output/table1b_hx_vi.csv", row.names = F)
write.csv(hx_abund_report, "data_output/table2b_hx_abund.csv", row.names = F)
write.csv(hx_pir_report, "data_output/table3b_hx_pir.csv", row.names = F)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#REPORT: VIZUALIZATION
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df_all_long = rbind(current_year_long, df_all_hx_long) %>%
   mutate(type = factor(type, levels = c("hx", "current")),
          zone = factor(zone, levels = zone_lvls)) %>%
   pivot_wider(names_from = est, values_from = value) %>%
   group_by(zone, week, spp, type) %>%
   summarise(abund = mean(abund), 
            pir = mean(pir), 
            vi = mean(vi)) %>%
  filter(spp == "All")

p_df_all_fun = function(df, value, text) {
  
  ggplot(df, aes(x = week, y = {{value}}, 
             color = type, fill = type, group = type)) +
  geom_area(position = "dodge", alpha = 0.3) +
  facet_grid(zone ~ .) +
  theme_classic() +
  ggtitle(text) +
  scale_color_manual(values = c("hx" = "grey50",
                                "current" = "red")) +
  scale_fill_manual(values = c("hx" = "grey50",
                               "current" = "red"))
}   

p_abund = p_df_all_fun(df_all_long, abund, "Abundance")
p_abund 

p_pir = p_df_all_fun(df_all_long, pir, "Pooled Infection Rate")
p_pir 

p_vi = p_df_all_fun(df_all_long, vi, "Vector Index")
p_vi

p_hx_current = p_abund + p_pir + p_vi + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
p_hx_current

ggsave("data_output/plots/hx_plot.png", p_hx_current, height = 8, width = 10, units = "in")



