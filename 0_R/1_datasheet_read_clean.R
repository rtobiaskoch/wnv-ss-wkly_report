#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------L O A D  P A C K A G E S ----------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())


#LOAD PACKAGES
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(tidyverse, purrr, readxl, janitor #manipulation
  )
})



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------- R E A D   D A T A S H E E T S   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#get list of file paths for all files in the dir_datasheet
t = list.files(path = dir_datasheet,
               full.names = T,
               ignore.case = T) 

cat("\nReading in the following datasheet files:\n", 
    paste(unlist(t), collapse = "\n"))

#read all files and bind it into one dataframe
data_input0 = t %>% 
  map(~read_excel(.x, col_names = T)) %>%
  bind_rows() %>%
  filter(!is.na(`Trap Date`)) #remove empty ids becasue VDCI keeps sending us empty ids

cat("\n\nDatasheet contains: \n", 
    sum(data_input0$Total), 
    "mosquitoes \n", 
    nrow(data_input0), 
    "pools \n", 
    length(unique(data_input0$`Collection Site       (Trap ID)`)), "traps\n",
    length(unique(data_input0$Zone)), "Zones\n"
    )

#save this after binding for the Weekly Input for the Report. Maintaining the original stupid format
#save as rds becasue csv fudges up the original colnames
write_rds(data_input0, fn_weekly_input_format_mid)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------- C L E A N   C O L N A M E S   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Rename using !!!
data_input <- data_input0 |>
  rename(!!!col_rename_datasheet) |>
  select(any_of(col_database))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C L E A N   D A T E ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C L E A N   T R A P  ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  mutate(trap_id = str_to_upper(trap_id)) %>%
  mutate(trap_id = str_trim(trap_id)) %>%
 
  
  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C L E A N   S P P  ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  
  mutate(spp2 = case_when(str_detect(spp, regex("^tar", ignore_case = T )) ~ 'Tarsalis',
                          str_detect(spp, regex("^pip", ignore_case = T )) ~ 'Pipiens'
  )
  ) %>%
  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C L E A N   M E T H O D  -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  mutate(method = str_trim(method)) %>%
  mutate(method2 = case_when(str_detect(method, regex("^g", ignore_case = T )) ~ 'G',
                             str_detect(method, regex("^l", ignore_case = T )) ~ 'L'
  )
  )



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   N A------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Identify columns (except test_code) that contain any blanks
# Safely check for blanks in each column
partially_blank_cols <- data_clean |>
  select(-test_code) |>
  map_lgl(~ identical(TRUE, any(is.na(.) | . == ""))) |>
  keep(~ .x) |>
  names()

# Issue warning or message
if (length(partially_blank_cols) > 0) {
  warning("\nThe following columns contain blank values: ",
          paste(partially_blank_cols, collapse = ", "))
} else {
  cat("\nNo columns contain blank values.\n")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   D A T E  F O R M A T -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(any(is.na(data_clean$trap_date))|class(data_clean$trap_date) != "Date") {
  stop("\nthere are missing/misformated dates in the data input\n")
} else {
  cat("\nAll dates in the datasheet are formatted correctly.\n")
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------- C H E C K    S A M P L E   D A T E    -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#check for samples from a previous week
data_input = data_input %>%
  filter(week == week_filter&year == year_filter) #incase samples were added from a previous week/year they still get added  to database

filtered_samples = data_input %>%
  filter(week != week_filter|year != year_filter)


if(nrow(filtered_samples) > 0) {
  write.csv(filtered_samples, "data_mid/non_week_samples.csv")
  print(paste0(filtered_samples$`CSU Pool Number (CMC Enters)`, " sample was removed and not part of ", week_filter, " sample pool"))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K     D U P S --------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

dupe = get_dupes(data_input, csu_id)

if(nrow(dupe) > 0) {
  stop("You have duplicate id(s) ", unique(dupe$csu_id)," in your datasheet(s)")
} else {
  cat("\nNo duplicates in your datasheet.\n")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   S P P   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(any(!is.na(data_clean$spp2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(spp = spp2) %>%
    select(-spp2)
} else {
  stop("\nthere are unknown species in the spp\n")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   M E T H O D   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(any(!is.na(data_clean$method2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(method = method2) %>%
    select(-method2)
} else {
  stop("\nthere are unknown trap methods in the method\n")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   T O T A L   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(any(data_clean$total > 50)) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  stop("One or more pools totals is greater than 50. Current methods pool mosquitoes in pools smaller than 50.")
} else {
   cat("\nAll mosquito pool totals are less than 50\n")
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   T R A P   I D S  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(file.exists(fn_trap)) { #if fn_trap already exists read that 
  trap_data = read.csv(fn_trap)
} else { #use gsheet_pull to get data from google drive
  
  fun_gsheet_pull_prompt(filename = fn_trap, "data", key = key_foco_trap)
  trap_data = read.csv(fn_trap)
}


unmatched_trap = data_clean %>%
  filter(!trap_id %in% trap_data$trap_id)

if(nrow(unmatched_trap) > 0) {
  write.csv(unmatched_trap, "data_mid/unknown_trap_ids.csv")
  stop("There are unknown trap ids in your data, check the data_mid/unkown_trap_ids.csv file.")
} else {
  cat("\nAll traps_ids are present in the datasheets.\n")
}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- W R I T E   T O    F I L E  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


write.csv(data_clean, fn_datasheet_clean, row.names = F)

