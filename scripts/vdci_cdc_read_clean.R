source("scripts/config.R")

#read in data and filter
vdci0 = rio::import(fn_new_vdci)

#tells me list of input_data_col is a different length but that is okay
suppressWarnings(
  if(any(colnames(vdci0) != input_data_col)) {
    vdci = vdci0 %>%
      rename(!!!rename_col) #%>%
    #  select(all_of(input_data_col)) %>%
     # filter(!is.na(trap_date))
    
  }
  
)


cdc0 = rio::import(fn_new_cdc)

if(any(colnames(cdc0) != input_data_col)) {
  cdc = cdc0 %>%
    rename(!!!rename_col)# %>%
   # select(all_of(input_data_col)) %>%
   # filter(!is.na(trap_date))
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


weekly_data_format = data_clean %>%
  rename(Account = "Acct",
         `Collection Site` = "trap_id",
         Total = "total",
         SPP = "spp",
         Method = "method")
         
        
#get list of changes made
#not including because it isn't very informative all species columns are being changed

# # Identify rows where changes occurred
# changed_rows <- which(rowSums(data_input != data_clean, na.rm = T) > 0)
# n_change_col <- colSums(data_input != data_clean, na.rm = T)
# # Get names of columns that have changes
# n_change_col = data.frame(values = n_change_col) %>%
#   filter(values > 0)

write.csv(data_clean,  fn_vdci_clean, row.names = F)
write.csv(weekly_data_format, fn_weekly_input_format, row.names = F)

