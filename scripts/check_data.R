#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(exists("all_data_fn")){ #does  the file name to read in your data exist?
  if(file.exists(all_data_fn)) {
    data_input = read_csv(all_data_fn, show_col_types = F)
  }else{
    "add your data to the data_input folder check the name in the config.R"
  }
  
} else{
  "all_data_fn object doesn't exist please run your config.R script"
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#CHECK COL NAMES
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(all(colnames(data_input) == data_col)) { #data_col in config.R file
  "All column names of data_input match expected names"
} else {
  
  "All column names of data_input DO NOT match expected names"
}

#CHECK CLASSES
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(all(sapply(data_input, class) == class_col)) {
  "All classes of variables in data_input match expected classes"
} else {
  
  "All classes of variables in data_input DO NOT match expected classes"
}









