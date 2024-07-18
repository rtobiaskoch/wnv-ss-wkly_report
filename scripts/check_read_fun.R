#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>sllz>>>>>>>
check_read_fun = function(input_file, year_filter, week_filter) {
  
if(file.exists(input_file)) {
  if(exists("year_filter")){
    rio::import(input_file) %>%
      filter(year %in% year_filter) %>%
      filter(week %in% week_filter) %>%
      mutate(zone = factor(zone, levels = zone_lvls),
              spp = factor(spp, levels = c("Pipiens", "Tarsalis"))) %>%
      arrange(zone, spp)
  } else {
   print( "run config.R to get year and week filters")
  }

    }else{
      print( "add your data to the data_input folder check the name in the config.R")
    }

}

