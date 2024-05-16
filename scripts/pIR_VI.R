#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CALCULATE POOLED INFECTIVITY RATE BY WEEK
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(exists("all_data_fn")){
  if(file.exists(all_data_fn)) {
    data_input = read_csv(all_data_fn, show_col_types = F)
  }else{
    "add your data to the data_input folder check the name in the config.R and run read_data.R script"
  }
  
} else{
  "all_data_fn object doesn't exist please run your config.R script"
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


abund_zone_wk = read_rds(paste0(abund_out_fn, ".RData"))

data_input = data_input %>%
  filter(year == year_filter,
         week == week_filter)

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
     )
  
  saveRDS(data_zone_wk,"data_output/data"
  
} else {
  print("your abundance.R file hasn't been run. Please run first")  

}




