source("scripts/config.R")


cq_data = read.csv(fn_cq_out) %>% 
  select(-SLEV_cq, -well_position)

gsheet_pull(trap_gsheet_key, "data", fn_trap)
trap_data = read.csv(fn_trap) %>%
  select(-zone)



new_data = read.csv(fn_vdci_clean) %>% 
  left_join(cq_data, by = "csu_id") %>%
  left_join(trap_data, by = c("trap_id")) %>%
  mutate(test_code = if_else(cq <= cq_threshold, 1, 0)) %>%
  mutate(seq = NA) 

write.csv(new_data, fn_vdci_clean_test, row.names = F)

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
