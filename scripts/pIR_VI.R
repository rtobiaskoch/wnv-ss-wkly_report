#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------C O N F I G --------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       
source("scripts/config.R")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------R E A D  D A T A ----------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
data_input = check_read_fun(fn_database_update)
abund_zone_wk = check_read_fun(paste0(fn_abund_out, ".csv"))
pools = check_read_fun(fn_pools_mid)
  

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
  data_zone_wk = pools %>%
    left_join(abund_zone_wk, by = grp_vars) %>%
    left_join(df_pir, by = grp_vars) %>%
    mutate(vi = round(abund * pir,4),
          vi_lci = round(abund * pir_lci,4),
          vi_uci = round(abund * pir_uci,4)) %>%
    mutate(year = factor(year),
           week = factor(week),
           zone = factor(zone, levels = zone_lvls),
           spp = factor(spp, levels = c("Pipiens", "Tarsalis"))) %>%
    arrange(zone, spp)

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #--------------- C A L C    A L L   S P P ----------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #TBD
  

  write.csv(data_zone_wk, fn_data_output,row.names = F)
  



