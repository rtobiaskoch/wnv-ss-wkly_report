source("scripts/config.R")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- R E A D   D A T A  -------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cq_data = read.csv(fn_cq_out) %>% 
  select(-well_position)

gsheet_pull(trap_gsheet_key, "data", fn_trap)
trap_data = read.csv(fn_trap) %>%
  select(-zone)

#join cq (pcr) data and the trap location data to get new data to add to the database
#keep the standards controls and other samples that arent i 
new_data0 = read.csv(fn_datasheet_clean) %>% 
  full_join(cq_data, by = "csu_id") %>%
  left_join(trap_data, by = c("trap_id")) %>%
  mutate(test_code = if_else(copies_WNV > copy_threshold, 1, 0)) %>%
  mutate(seq = NA) 



new_data = new_data0 %>%
  filter(!is.na(trap_id)) %>%
  select(all_of(database_col))
  


write.csv(new_data, fn_datasheet_clean_test, row.names = F)

  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #---------- P U L L   &   U P D A T E    G D R I V E    D A T A B A S E  ----------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  
#pull database from googledrive location and read it as a csv
gsheet_pull(database_gsheet_key, "data", fn_database_input)

database = read.csv(fn_database_input)

#make a copy of the database before update
drive_cp(file = as_id(database_gsheet_key), 
         path = "database_archive",
         name = fn_gdrive_archive,
         overwrite = T)

#will update any csu_id that matches with the new_data and adds it if it doesn't
database_update = rquery::natural_join(new_data, database, by = "csu_id", jointype = "FULL")

database_update = database_update %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>% #prevents NA from making non character variables in database throw errors
  select(all_of(database_col)) %>% #reorder them because natural_join fucked up the order
  arrange(desc(trap_date))

#save it to the working directory
write.csv(database_update, fn_database_update, row.names = F)


#save it to gdrive
googlesheets4::sheet_write(database_update,
                           ss = database_gsheet_key,
                           sheet = "data"
                          )
 # END OF ELSE FOR MISSING TRAP CHECK