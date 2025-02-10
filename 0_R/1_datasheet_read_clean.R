#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------L O A D  P A C K A G E S ----------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())


#LOAD PACKAGES
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(tidyverse, purrr #manipulation
  )
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------ C O N F I G ------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#IF THE CONFIG PARAMS FILE EXISTS US THAT TO DEFINE INPUTS 
# ELSE DEFINE THEM HERE
config_params_file = "1_input/config_params.RDS" # <<<<<<<<<<----------------------------------  USER INPUT

if(file.exists(config_params_file)){
  list2env(readRDS(config_params_file), 
           envir = .GlobalEnv)
} else { # if no config define them yourself peasant.

#INPUTS FOR THIS SCRIPT IF CONFIG FILE DOESN'T EXIST *****************
  
  #SUBDIRECTORIES
  dir_datasheet = "1_input/datasheet/" # <<<<<<<<<<--------------------------------------------  USER INPUT
  
  #TIME FILTERS
  week_filter = 37
  year_filter = 2024
  
  col_rename_datasheet <- c("csu_id" = "CSU Pool Number (CMC Enters)" , 
                            "trap_id" = "Collection Site       (Trap ID)", 
                            "year" = "Year", 
                            "week" = "Week", 
                            "trap_date" = "Trap Date", 
                            "county" = "County", 
                            "method" = "L/G", 
                            "genus" = "Genus", 
                            "spp" = "Species", 
                            "sex" = "Sex", 
                            "no_gravid" = "No. Gravid", 
                            "no_deplete" = "No. Deplete", 
                            "total" = "Total", 
                            "test_code" = "Test Code (CSU Enters)" , 
                            "zone" = "Zone")
  
  col_input_database = c("csu_id", "trap_id", "year", "week", "trap_date", 
                         "county", "method", "spp", "total", "test_code", "zone")
  
  col_database = c( "csu_id", "trap_id", "zone", "year", "week", "trap_date", "county", "method", "spp",
                    "total", "test_code", "cq", "copies_WNV", "seq", "cq", "lat", "long")
  #FILENAMES
  fn_datasheet_clean = "2_mid/y2024_w37_datasheet.csv"
  fn_weekly_input_format_mid = "2_mid/weekly_data_input_format_mid.RData"
  fn_trap = "1_input/foco_trap - data.csv"
  
  #KEYS
  key_foco_trap = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"
  
} # end of if config_params_file exist statement


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------- R E A D   D A T A S H E E T S   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#get list of file paths for all files in the dir_datasheet
t = list.files(path = dir_datasheet,
               full.names = T,
               ignore.case = T)  

#read all files and bind it into one dataframe
data_input0 = t %>% 
  map(~read_excel(.x, col_names = T)) %>%
  bind_rows() %>%
  filter(!is.na(`Trap Date`)) #remove empty ids becasue VDCI keeps sending us empty ids

#save this after binding for the Weekly Input for the Report. Maintaining the original stupid format
#save as rds becasue csv fudges up the original colnames
write_rds(data_input0, fn_weekly_input_format_mid)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   W E E K __________---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(any(data_input0$Week != week_filter)) {
  stop(paste0("One or more of the weeks in your datasheet doesn't match the week_filter."))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   C O L   N A M E S ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

data_input = data_input0 %>%
  rename(!!!col_rename_datasheet)

if(all(col_input_database %in% names(data_input))==F) { # if any of the required col_input_database aren't in the input data throw an error
  
  stop(paste0("The column(s) ", setdiff(col_input_database, names(data_input)), " are missing from your datasheets")
       )
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K     Z O N E --------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(any(is.na(data_input$zone))){
  
  stop("you have missing zones in your data")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K     S A M P L E    I D ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
if(any(is.na(data_input$csu_id))){
  
  stop("you have missing sample ids in your data")
}

if(any(!str_detect(data_input$csu_id, "-"))) {
  stop("you have samples id without a -")
}


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
#-------------------- C H E C K   D A T E  F O R M A T -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(any(is.na(data_clean$trap_date))|class(data_clean$trap_date) != "Date") {
  stop("there are missing/misformated dates in the data input")
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
  
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   S P P   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(any(!is.na(data_clean$spp2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(spp = spp2) %>%
    select(-spp2)
} else {
  stop("there are unknown species in the spp")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- C H E C K   M E T H O D   -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(any(!is.na(data_clean$method2))) { #if there are no missing matches with spp2 then replace spp otherwise throw an message
  data_clean = data_clean %>%
    mutate(method = method2) %>%
    select(-method2)
} else {
  stop("there are unknown trap methods in the method")
}





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# -------------------- D E F I N E   G S H E E T _ P U L L  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fun_gsheet_pull_prompt = function(filename, sheet, key) {
  
  user_input <- readline(prompt = paste0("Would you like to download the trap malfunction file to ", filename, " from Google Drive? (yes/no): "))
  
  # Check the user's input
  if (tolower(user_input) == "yes") {
    # Run the script if the user says "yes"
    
    require(googlesheets4)
    require(googledrive)
    library(googlesheets4)
    
    #if drive doesn't have a token then have user authorize
    if(!googledrive::drive_has_token()) {
      googledrive::drive_auth()
    }
    
    mdata = googlesheets4::read_sheet(key, sheet = sheet)
    mdata <- mdata %>%
      mutate(across(where(is.list), ~ sapply(., paste, collapse = ", ")))
    #mdata = apply(mdata, 2, as.character) #added because error   unimplemented type 'list' in 'EncodeElement' was occuring
    write.csv(mdata, filename, row.names = F, na = "")
    read.csv(filename)
    
    print("...Checking that gdrive download worked...")
    
  } else { #end of user_input yes update
    cat("File", filename, " download skipped. Please ensure the file exists before proceeding.\n")
  }
  
  if(!file.exists(filename)) {
    stop(cat(filename, " file doesn't exist. Please download and rerun this config file. \n"))
  } else {
    cat(filename, "exists.\n")
  }
  
  rm(user_input)
} #end of function


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
  print("All traps_ids are present in the datasheets.")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- W R I T E   T O    F I L E  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

data_clean = data_clean %>%
  select(any_of(col_database))
#   filter(values > 0)

write.csv(data_clean, fn_datasheet_clean, row.names = F)

