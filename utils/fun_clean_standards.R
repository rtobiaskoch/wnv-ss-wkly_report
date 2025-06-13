clean_standards = function(df, std_pattern = "std|pos ctrl") {
  
  #takes input data from the merging of the ct data from the pcr input 
  #and the platemap data that provides the id and associated metadata
  
  req_cols = c("sample_type", "ct_threshold", "well_position",
               "cq_WNV", "plate", "csu_id")
  
  missing_cols <- setdiff(req_cols, names(df))
  
  if (length(missing_cols) > 0) {
    cat("\n Notice. Following columns are required for this function: ", paste(missing_cols, collapse = ", "), "\n")
    cat("Check input data.")
  }
  
  df = df %>%
  filter(str_detect(pattern = std_pattern, sample_type)) %>%
  select(-ct_threshold, -well_position) %>%
  pivot_longer(cols = -c(plate, csu_id, sample_type, amp_status, year, week),
               names_to = "type",
               values_to = "value") %>%
  mutate(target = if_else(str_detect(type, "WNV"), "WNV", "SLEV")) %>%
  mutate(type = str_extract(type, "^[^_]*")) %>%
  pivot_wider(names_from = type, 
              values_from = value) %>%
  mutate(year = year, 
         week = week_filter,
         log_copies = if_else(copies == 0, 0, log10(copies))) %>%
  select(year, week, plate, target, csu_id, sample_type, cq, log_copies) 
  
  return(df)
  
}