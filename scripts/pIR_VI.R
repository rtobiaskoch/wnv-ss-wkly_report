#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CALCULATE POOLED INFECTIVITY RATE BY WEEK
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
source("scripts/config.R")

#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = check_read_fun(all_data_fn)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


abund_zone_wk = check_read_fun(paste0(abund_out_fn, ".csv"))


#split to map pIR over week pools
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  data_list = data_input %>%
    group_by(zone, year, week, spp) %>%
    group_split()
  
  #iterat
  mle = purrr::map(data_list, ~ pIR(test_code ~ total, data = ., pt.method = "mle"))
  
  #extract values from pIR 
  pir = sapply(mle,"[[",1)
  lci =sapply(mle,"[[",2)
  uci =sapply(mle,"[[",3)
  
  data_zone_wk = abund_zone_wk %>%
    mutate(pir = round(pir,4),
           pir_lci = round(lci,4),
           pir_uci = round(uci,4)) %>%
   mutate(vector_index = round(abund * pir,4))

  

  write.csv(data_zone_wk, data_output_fn,row.names = F)
  



