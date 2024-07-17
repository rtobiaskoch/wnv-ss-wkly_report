source("scripts/config.R")

#datasheets (mosquito pools) read

#data from vdci, cdc, and bc

#get list of file paths for all files in the fn_datasheet_input
t = list.files(path = fn_datasheet_input,
               full.names = T,
               ignore.case = T)  

#read all files and bind it into one dataframe
data_input = t %>% 
  map(~read_excel(.x, col_names = T)) %>%
  bind_rows() %>%
  filter(!is.na(`Trap Date`)&!is.na(`Zone`)&!is.na(`Total`)) #remove empty ids becasue VDCI keeps sending us empty ids

#save this after binding for the Weekly Input for the Report. Maintaining the original stupid format
#save as rds becasue csv fudges up the original colnames
write_rds(data_input, fn_weekly_input_format_mid)


data_input = data_input %>%
  rename(!!!rename_col)


#check for samples from a previous week
data_input = data_input0 %>%
  filter(Week == week_filter&Year == year_filter) #incase samples were added from a previous week/year they still get added  to database

filtered_samples = data_input0 %>%
  filter(Week != week_filter|Year != year_filter)


if(nrow(filtered_samples) > 0) {
  write.csv(filtered_samples, "data_mid/non_week_samples.csv")
  print(paste0(filtered_samples$`CSU Pool Number (CMC Enters)`, " sample was removed and not part of ", week_filter, " sample pool"))
}



if(nrow(get_dupes(data_input) > 0)) {
  stop("You have duplicates in your datasheets")
}

if(nrow(get_dupes(data_input, csu_id) > 0)) {
  stop("You have duplicate id's in your datasheets")
}


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
  mutate(method2 = case_when(str_detect(method, regex("^g", ignore_case = T )) ~ 'G',
                             str_detect(method, regex("^l", ignore_case = T )) ~ 'L'
  )
  )

#data_clean[1,"trap_date"] <- NA

if(any(is.na(data_clean$trap_date))|class(data_clean$trap_date) != "Date") {
  stop("there are missing/misformated dates in the data input")
}


if(any(!is.na(data_clean$spp2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(spp = spp2) %>%
    select(-spp2)
} else {
  stop("there are unknown species in the spp")
}

if(any(!is.na(data_clean$method2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(method = method2) %>%
    select(-method2)
} else {
  stop("there are unknown trap methods in the method")
}


data_clean = data_clean %>%
  select(any_of(database_col))
#   filter(values > 0)

write.csv(data_clean,  fn_datasheet_clean, row.names = F)
