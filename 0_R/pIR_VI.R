#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------C O N F I G --------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       
#list2env(readRDS(config_params_file),           envir = .GlobalEnv)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------R E A D  D A T A ----------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
data_input = database_update
abund_zone_wk = check_read_fun(fn_abund_out, wk = week_filter_yr)
#pools = check_read_fun(fn_pools_mid, wk = week_filter_yr)
  

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- C A L C   P I R:  A L L   Z O N E S --------------
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
  data_zone_wk0 = abund %>%
    left_join(pools, by = grp_vars) %>%
    left_join(df_pir, by = grp_vars) %>%
    mutate(vi = round(abund * pir,4),
          vi_lci = round(abund * pir_lci,4),
          vi_uci = round(abund * pir_uci,4)) %>%
    mutate(year = factor(year),
           week = factor(week),
           zone = factor(zone, levels = zone_lvls),
           spp = factor(spp, levels = c("Pipiens", "Tarsalis"))) %>%
    arrange(zone, spp) %>%
   mutate(across(everything(), ~ replace(.x, is.na(.x), 0)))
 

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #--------------- S U M    A L L   S P P ----------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  sum_col = c("pools_G", "pools_L", "n_pools",
              "pos_pools_G", "pos_pools_L", "n_pos_pools", 
             # "mosq", 
              "mosq_L", 
              "abund", "vi", "vi_lci", "vi_uci")
 
  distinct_col = c("trap_L")
  
  grp_vars_sym <- syms(c(grp_vars))
  
  suppressMessages({
    data_zone_wk_spp_all0 <- data_zone_wk0 %>%
      mutate(spp = "All") %>%
      group_by(!!!grp_vars_sym) %>%
      summarise(
        spp = "All",
        across(all_of(sum_col), ~ sum(.x, na.rm = TRUE)),
        across(all_of(distinct_col), ~ max(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(across(everything(), ~ replace(.x, is.na(.x), 0)))
  
  })
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #--------------- C A L C   P I R   A L L   S P P ---------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>å

  suppressMessages({
    pir_all_spp = data_zone_wk0 %>%
      mutate(spp = "All") %>%
      group_by(!!!grp_vars_sym) %>%
      summarize(spp = "All",
                pir = sum(vi, na.rm = T)/sum(abund, na.rm=T),
                pir_lci = sum(vi_lci, na.rm = T)/sum(abund, na.rm = T),
                pir_uci = sum(vi_uci, na.rm = T)/sum(abund, na.rm = T)) %>%
      mutate(across(everything(), ~ replace(.x, is.na(.x), 0)))
    
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
  
  
  write.csv(data_zone_wk, fn_data_output,row.names = F)



