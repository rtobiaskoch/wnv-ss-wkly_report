#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CALCULATE POOLED INFECTIVITY RATE BY WEEK
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#check and read in data_input
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

#checks and reads in trap_data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(exists("trap_fn")){
  if(file.exists(trap_fn)) {
    trap_coord = read_csv(trap_fn, show_col_types = F)
  }else{
    "add your data to the data_input folder check the name in the config.R and run read_data.R script"
  }
  
} else{
  "trap_fn object doesn't exist please run your config.R script"
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(any(c("zone","lat","long") %in% colnames(data_input))){ #if trap_columns zone, lat long exist in dataframe remove them to be updated
  data_update = data_input %>% 
    select(-c(trap_col)) 
   } else{ #otherwise just add them
     data_update = data_input %>% 
       mutate(trap_id = str_to_upper(trap_id)) %>% 
       left_join(trap_coord, by ="trap_id") %>%
       mutate(zone = if_else(is.na(zone),
                             str_sub(trap_id, 1,2),#if zone is missing get the general location from the trap id
                             zone) #otherwise keep the zone
       )
     
   }

write.csv(data_update, "data_input/wnv-s_all_data.csv", row.names = F)

 

unmatched_traps = data_update %>%
  filter(is.na(lat))

if(nrow(unmatched_traps > 0)) {
  write.csv(unmatched_traps, "data_output/unmatched_traps.csv")
  print("you have traps with unknown location, see data_output/unmatched_traps.csv for details")
}

rm(data_update)