
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

source("0_R/gsheet_pull_prompt.R")

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
# ------C H E C K  &  L O A D  F I L E S   F R O M  G D R I V E ------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fun_gsheet_pull_prompt(fn_trap_malfunction, "data", key_trap_malfunction)
fun_gsheet_pull_prompt(fn_trap, "data", key_foco_trap)
fun_gsheet_pull_prompt(fn_gdrive_database, "data", key_database_gsheet)






