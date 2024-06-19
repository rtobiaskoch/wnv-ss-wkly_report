source("scripts/config.R")

#read in data and filter
vdci = rio::import(fn_new_vdci)

if(any(colnames(vdci) != input_data_col)) {
  vdci = vdci %>%
    rename(!!!rename_col) %>%
    select(all_of(input_data_col)) %>%
    filter(!is.na(trap_date))
  
}

cdc = rio::import(fn_new_cdc)

if(any(colnames(cdc) != input_data_col)) {
  cdc = cdc %>%
    rename(!!!rename_col) %>%
    select(all_of(input_data_col)) %>%
    filter(!is.na(trap_date))
}


data_input = rbind(cdc, vdci)


#clean data
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
  mutate(trap_id = str_to_upper(trap_id)) %>%
  #clean up variations in the species
  mutate(spp2 = case_when(str_detect(spp, regex("^tar", ignore_case = T )) ~ 'Tarsalis',
                          str_detect(spp, regex("^pip", ignore_case = T )) ~ 'Pipiens'
  )
  ) %>%
  #clean up variations in the method of gravid light trap
  mutate(method = str_trim(method)) %>%
  mutate(method2 =case_when(str_detect(method, regex("^g", ignore_case = T )) ~ 'G',
                            str_detect(method, regex("^l", ignore_case = T )) ~ 'L'
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
    mutate(method = method2) %>%
    select(-method2)
} else {
  print("there are unknown trap methods in the method")
}

skimr::skim(data_clean)


write.csv(data_clean,  fn_vdci_clean, row.names = F)

