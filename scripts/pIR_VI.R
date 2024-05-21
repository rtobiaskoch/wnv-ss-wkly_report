#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CALCULATE POOLED INFECTIVITY RATE BY WEEK
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
source("scripts/config.R")

#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = check_read_fun(all_data_fn)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


abund_zone_wk = check_read_fun(paste0(abund_out_fn, ".csv"))

#get list of weeks with only gravid traps
gravid_only = data_input %>%
  group_by(year, week, zone, method,spp) %>%
  count() %>%
  pivot_wider(names_from = method, 
              values_from = n,
              values_fill = 0) %>%
  filter(G > 0 & L == 0) # keep list of weeks where only a gravid trap was in the zone
  
write.csv(gravid_only, "data_output/gravid_only_week_zone.csv" , row.names = F)

#split to map pIR over week pools
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # data_list = data_input %>%
  #   anti_join(gravid_only, by = c("year", "week", "zone", "spp")) %>% # remove the wks with only gravid trap
  #   arrange(year, week, zone, spp) %>%
  #   group_by(zone, year, week, spp) %>%
  #   group_split()
  
  
  data_list = data_input %>%
    anti_join(gravid_only, by = c("year", "week", "zone", "spp")) %>% # remove the wks with only gravid trap
    arrange(year, week, zone, spp) %>%
    mutate(grp = paste(year,week,zone,spp, sep ="-"))
  
  
  mle = pIR(test_code ~ total|grp, data = data_list, pt.method = "firth")
  
 df_pir = as.data.frame(mle) %>%
   separate(grp,
            into = c("year", "week", "zone", "spp"),
            sep = "-") %>%
   mutate(year = as.integer(year),
          week = as.integer(week)) %>%
   rename(pir = P,
          pir_lci = Lower,
          pir_uci = Upper)
  
  data_zone_wk = data_input %>%
    anti_join(gravid_only, by = c("year", "week", "zone", "spp")) %>%
    distinct(zone, year, week, spp) %>%
    left_join(abund_zone_wk, by = c("year", "week", "zone", "spp")) %>%
    left_join(df_pir, by = c("year", "week", "zone", "spp")) %>%
   mutate(vi = round(abund * pir,4),
          vi_lci = round(abund * pir_lci,4),
          vi_uci = round(abund * pir_uci,4))

  

  write.csv(data_zone_wk, data_output_fn,row.names = F)
  



