list2env(readRDS(config_params_file),           envir = .GlobalEnv)

data_input0 = read_rds(fn_weekly_input_format_mid)

data_input = data_input0 %>%
  filter(Week == week_filter&Year == year_filter) #incase samples were added from a previous week/year they still get added  to database

filtered_samples = data_input0 %>%
  filter(Week != week_filter|Year != year_filter)


if(nrow(filtered_samples) > 0) {
  write.csv(filtered_samples, "data_mid/non_week_samples.csv")
  print(paste0(filtered_samples$`CSU Pool Number (CMC Enters)`, " sample was removed and not part of week ", week_filter, " sample pool"))
}


order = colnames(data_input)

test_results = check_read_fun(fn_database_update) %>%
  select(csu_id, test_code) %>% # keep only the test_code
  rename(`CSU Pool Number (CMC Enters)` = "csu_id",
         `Test Code (CSU Enters)` = "test_code")

weekly_data_format = data_input %>%
  select(-`Test Code (CSU Enters)`, #drop because going to be added from the data_base_update data
         -Comments, `Test Result (CSU Enters)`) %>% #drop because it messes up the order for the export
  left_join(test_results, by = "CSU Pool Number (CMC Enters)") %>%
  mutate(`Test Result (CSU Enters)` = if_else(`Test Code (CSU Enters)` == 1,
                                              "Positive",
                                              "Negative" )) %>%
  mutate(`Collection Site       (Trap ID)`  = str_to_upper(`Collection Site       (Trap ID)`)) %>%
  #clean up variations in the species
  mutate(Species = case_when(str_detect(Species, regex("^tar", ignore_case = T )) ~ 'Tarsalis',
                          str_detect(Species, regex("^pip", ignore_case = T )) ~ 'Pipiens'
                            )
        ) %>%
  #clean up variations in the method of gravid light trap
  mutate(`L/G` = str_trim(`L/G`)) %>%
  mutate(`L/G` = case_when(str_detect(`L/G`, regex("^g", ignore_case = T )) ~ 'G',
                             str_detect(`L/G`, regex("^l", ignore_case = T )) ~ 'L'
                          )
        ) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate(Week = epiweek(`Trap Date`)) %>% #replace manual week input with date week
  rename(Spp = "Species",
         Method = "L/G",
         Account = "Acct")

write.csv(weekly_data_format, fn_weekly_input_format, row.names = F)
