#data clean 
source("scripts/config.R")

data_input = check_read_fun(all_data_fn)

#date clean check

if(is.character(data_input$trap_date)) {
  data_input = data_input %>%
    mutate(trap_date = mdy(trap_date))
}

#clean the date
data_clean = data_input %>%
      mutate(trap_date = as.Date(trap_date)) %>%
      mutate(week = epiweek(trap_date)) %>% #replace manual week input with date week
      mutate(year = as.factor(year),
         week = as.factor(week)) %>%
  #clean up variations in the species
      mutate(spp2 = case_when(str_detect(spp, regex("^tar", ignore_case = T )) ~ 'Tarsalis',
                         str_detect(spp, regex("^pip", ignore_case = T )) ~ 'Pipiens'
                            )
            ) %>%
  #clean up variations in the method of gravid light trap
  mutate(method = str_trim(method)) %>%
  mutate(method2 = case_when(grepl(data_input$method, "^gravid|^G", ignore.case = T ) ~ 'G',
                             grepl(data_input$method, "^l/g|^light|^L", ignore.case = T ) ~ 'L'
                             )
         )

if(any(!is.na(data_clean$spp2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(spp = spp2) %>%
    select(-spp2)
} else {
  data_clean()
  print("there are unknown species in the spp")
}

if(any(!is.na(data_clean$method2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(method = method) %>%
    select(-method2)
} else {
  print("there are unknown trap methods in the method")
}
  
skimr::skim(data_clean)
  
  
  clean_fn = str_replace(all_data_fn, ".csv", "_cleaned.csv")
  write.csv(data_clean,  clean_fn, row.names = F)
  