list2env(readRDS("data_input/config_params.RDS"),           envir = .GlobalEnv)

#pull and read database
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gsheet_pull(database_gsheet_key, "data", fn_database_input)

database = read.csv(fn_database_input) %>% #FIX DATE
  mutate(trap_date = ymd(trap_date))
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#checks and reads in trap_data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gsheet_pull(trap_gsheet_key, "data", fn_trap)
trap_data = read.csv(fn_trap)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

database_update = rquery::natural_join(new_data, database, by = "csu_id", jointype = "FULL")



  data_update = data_input %>% 
    select(-c(trap_col)) %>%
      mutate(trap_id = str_to_upper(trap_id)) %>% 
       left_join(trap_coord, by ="trap_id") %>%
       mutate(zone = if_else(is.na(zone),
                             str_sub(trap_id, 1,2),#if zone is missing get the general location from the trap id
                             zone) #otherwise keep the zone
       )
     


fn_trap_update = str_replace(fn_database_updated, ".csv", "_trap_update.csv")
write.csv(data_update, fn_trap_update , row.names = F)

 

unmatched_traps = data_update %>%
  filter(is.na(lat)) %>%
  distinct(trap_id, zone, lat, long)

if(nrow(unmatched_traps > 0)) {
  write.csv(unmatched_traps, "data_output/unmatched_traps.csv")
  print("you have traps with unknown location, see data_output/unmatched_traps.csv for details")
}

rm(data_update)
