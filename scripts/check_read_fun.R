#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>sllz>>>>>>>
check_read_fun = function(input_file) {
  
if(file.exists(input_file)) {
  if(exists("year_filter")){
    rio::import(input_file) %>%
      filter(year %in% year_filter) %>%
      filter(week %in% week_filter)
  } else {
    "run config.R to get year and week filters"
  }

    }else{
      "add your data to the data_input folder check the name in the config.R"
    }

}

