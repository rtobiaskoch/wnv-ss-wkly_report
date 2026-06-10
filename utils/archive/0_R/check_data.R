#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(exists("all_data_fn")){ #does  the file name to read in your data exist?
  if(file.exists(all_data_fn)) {
    data_input = read_csv(all_data_fn, show_col_types = F)
  }else{
    "add your data to the data_input folder check the name in the config_weekly.R"
  }
  
} else{
  "all_data_fn object doesn't exist please run your config_weekly.R script"
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#CHECK COL NAMES
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(all(colnames(data_input) == col_datasheet)) { #col_datasheet in config_weekly.R file
  "All column names of data_input match expected names"
} else {
  
  "All column names of data_input DO NOT match expected names"
}

#CHECK CLASSES
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(all(sapply(data_input, class) == col_class_database)) {
  "All classes of variables in data_input match expected classes"
} else {
  
  "All classes of variables in data_input DO NOT match expected classes"
}









