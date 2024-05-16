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