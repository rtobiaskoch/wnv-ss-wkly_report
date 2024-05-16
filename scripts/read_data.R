#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(exists("all_data_fn")){
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
if(all(colnames(data_input) == data_col)) {
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


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data2process_fn = list.files("data_input/data_2_process/",
                         full.names = T)

if(length(data2process_fn) != 1) {
  print("There is more or less than one file in the data_input/data_2_process folder, 
        using week_filter and year_filter to filter all_data in config.R instead")
  
 data_input = data_input %>% 
    filter(year == year_filter, #year_filter defined in config.R
           week == week_filter) #year_filter defined in config.R
  
} else {
  if(!str_detect(string = new_data_fn, pattern = ".csv")){
    print("your file is not a csv. please convert to a csv")
  } else(
    data_input = read.csv(data2process_fn) 
  )
  
}




