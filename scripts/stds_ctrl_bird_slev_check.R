source("scripts/config.R")

data_input = read.csv(fn_stds_ctrl_slev_bird) %>%
  mutate(slev_test_code = if_else(copies_SLEV > 100, 1, 0))

#check for positive birds
bird = data_input %>%
  filter(grepl("^RMRP", csu_id, ignore.case = T)) 

bird_pos = sum(bird$test_code, na.rm = T)+ sum(bird$slev_test_code, na.rm = T)

if(bird_pos > 0) {
  print("check data you have a positive bird sample")
  write.csv(bird, "data_output/positive_birds.csv")
}

#check for SLEV
all_samples = data_input %>%
  filter(grepl("^RMRP|^CSU|^BOU|^CDC", csu_id, ignore.case = T)) 


  if(sum(all_samples$slev_test_code) > 0) {
    print("check data you have a positive SLEV sample")
  }
  
#filter for all samples that aren't going to be added to the database
non_database_sample = data_input %>%
  filter(!grepl("^CSU|^BOU|^CDC", csu_id, ignore.case = T)) 

write.csv(non_database_sample, fn_non_database_sample)

