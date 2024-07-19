#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>sllz>>>>>>>
check_read_fun = function(input_file, yr = NULL, wk = NULL) {
  
  if(is.null(wk)) {wk = week_filter}
  if(is.null(yr)) {yr = year_filter}
  
if(file.exists(input_file)) {
  if(exists("year_filter")){
    rio::import(input_file) %>%
      filter(year %in% yr) %>%
      filter(week %in% wk) %>%
      mutate(zone = factor(zone, levels = zone_lvls),
              spp = factor(spp, levels = c("Pipiens", "Tarsalis", "All"))) %>%
      arrange(year, zone, week,spp)
  } else {
   print( "run config.R to get year and week filters")
  }

    }else{
      print( "add your data to the data_input folder check the name in the config.R")
    }

}

