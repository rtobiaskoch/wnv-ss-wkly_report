source("scripts/config.R")

#read in data and filter
vdci = rio::import(fn_new_vdci)
cdc = rio::import(fn_new_cdc)
test = rio::import(fn_vdci_clean_test) %>%
  select(csu_id, test_code) %>%
  rename( `CSU Pool Number (CMC Enters)` = "csu_id",
          `Test Code (CSU Enters)` = "test_code")
          

data_input = rbind(cdc, vdci) %>%
  select(-`Test Code (CSU Enters)`) %>% #drop because getting added from test
  rename(Account = "Acct",
         SPP = "Species",
         Method = "L/G")  %>%
  left_join(test, by = "CSU Pool Number (CMC Enters)") %>%
  select(weekly_input_report_format) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

write.csv(data_input, "data_output/weekly_data_input_format.csv")

  
  
