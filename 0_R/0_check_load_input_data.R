
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------L O A D  P A C K A G E S ----------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(googlesheets4, googledrive, googledrive, argparse, #importing and exporting
                 tidyverse#manipulation
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
  
  #FILENAMES
  fn_datasheet_clean = "2_mid/y2024_w37_datasheet.csv"
  fn_weekly_input_format_mid = "2_mid/weekly_data_input_format_mid.RData"
  fn_trap = "1_input/foco_trap - data.csv"
  
  #KEYS
  key_database_gsheet = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
  key_trap_malfunction = "1dsTyvZoCN6NUJlTcDLINMfxuGZdJuP2ADpn8noQwL6Q"
  key_foco_trap = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"
  
  dir_input = "1_input/"
  fn_gdrive_database = file.path(dir_input,"wnv-s_database - data.csv") # <<<<<<<--------------------------------------------------- U S E R   I N P U T
  fn_trap_malfunction <- file.path(dir_input, "trap_malfunction - data.csv")
  fn_trap <- file.path(dir_input, "foco_trap - data.csv")
  
} # end of if config_params_file exist statement



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ---------------G S H E E T   P U L L  F U N C T I O N -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fun_gsheet_pull_prompt = function(filename, sheet, key) {
  
  #first check if user has drive token. They don't have them provide one
  if(!drive_has_token()) {
    googledrive::drive_auth()
  }
  
  user_input <- readline(prompt = paste0("Would you like to download/update the file ", filename, " from Google Drive? (yes/no): "))
  
  # Check the user's input
  if (tolower(user_input) == "yes") {
    # Run the script if the user says "yes"
    
    require(googlesheets4)
    require(googledrive)
    
    
    #remove old file if it exists
    if(file.exists(filename)) { file.remove(filename) }
    
    #READ IN DATA
    mdata = read_sheet(key, sheet = sheet)
    mdata <- mdata %>%
      mutate(across(where(is.list), ~ sapply(., paste, collapse = ", ")))
    #mdata = apply(mdata, 2, as.character) #added because error   unimplemented type 'list' in 'EncodeElement' was occuring
    write.csv(mdata, filename, row.names = F, na = "")
    read.csv(filename)
    print("...Checking that gdrive download worked...")
    
    #if file exists process else stop function
    if(file.exists(filename)) {cat(filename, " gdrive downloaded successfully.\n") } else {
      stop(cat(filename, " file doesn't exist. Please rerun function or download manually. \n")) } #end if file check
    
    } #end user_input yes
  
  #If user_input is NO
  if (tolower(user_input) == "no") {
    cat("File", filename, " gdrive download skipped. \n")
    #if user_input no and file exists
    if(file.exists(filename)) {cat(paste0(filename, " modified ", as.Date.POSIXct(file.info(filename)$mtime), " will be used."))
      
    } else {     #if user_input no and file doesn't exist
      cat(filename, " file doesn't exist. Please rerun function or download manually. \n")
      stop()
      } 
     } #end user_input no
  
  #If user input is invalid
  if (!(tolower(user_input) %in% c("no", "yes"))) {
    cat("Please input a valid response.")
  }
  
  rm(user_input)
} #end of function


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ------C H E C K  &  L O A D  F I L E S   F R O M  G D R I V E ------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fun_gsheet_pull_prompt(fn_trap_malfunction, "data", key_trap_malfunction)
fun_gsheet_pull_prompt(fn_trap, "data", key_foco_trap)
fun_gsheet_pull_prompt(fn_gdrive_database, "data", key_database_gsheet)






