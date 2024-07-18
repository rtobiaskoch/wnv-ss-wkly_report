#Config

rm(list = ls())
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(googlesheets4, googledrive, rio, readxl, openxlsx, googledrive, #importing and exporting
                 tidyverse, janitor, lubridate, rquery, #manipulation
                 PooledInfRate, #analysis
                 ggpubr, wesanderson, paletteer, leaflet, patchwork# plotting
                 )
  
  
})


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FUNCTIONS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
source("scripts/check_read_fun.R")
source("scripts/gsheet_read_fun.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#DATA PARAMETERS##
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
year_filter = 2024
week_filter = 28
fc_zones = c("NE", "SE", "NW", "SW")
non_fc_zones = c("LV", "BC", "BE")
all_zones = c("NE", "SE", "NW", "SW", "LV", "BC", "BE")

copy_threshold = 22300
rn_threshold = 34000
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CODE OPTIONS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
update_trap = "NO"
clean_data = "NO"
fc_zone_filter = "YES"
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FILTER CHECK
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
current_year = year(Sys.Date())
last_week = week(Sys.Date())

if(current_year != max(year_filter)) {
  print("the year_filter doesn't match the current_year. Did you remember to update it?")
}

if(last_week != max(week_filter)) {
  print("the week_filter doesn't match last_week. Did you remember to update it?")
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#OUTPUT FILE NAME GENERATION
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(length(year_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_year = paste0(min(year_filter), "-", max(year_filter))
} else { #otherwise fn_year stays the same
  fn_year = year_filter
}

if(length(week_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_week = paste0(min(week_filter), "-", max(week_filter))
} else { #otherwise fn_year stays the same
  fn_week = week_filter
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FILE NAMES INPUT
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
database_gsheet_key = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
trap_gsheet_key = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"
trap_malfunction_key = "1dsTyvZoCN6NUJlTcDLINMfxuGZdJuP2ADpn8noQwL6Q"
trap_active_key = "1SA_PE74KLH6_jG3yR49e8py1uXgb_C02Q3Iz9MWivrY"
standards_key = "1bSMYQ4bZ9uBfrOQ6ylsegNmmGYdf9YFVbxB4qBhnFQo"

fn_gdrive_database = "wnv-s_database"

#weekly input
fn_datasheet_input = "data_input/datasheet" #replaced vdci and cdc input because also have boulder so all in one place
fn_platemap = "data_input/platemap"
fn_pcr = "data_input/pcr"

fn_trap_malfunction = "data_input/trap_malfunction.csv"
fn_trap_active = "data_input/trap_active.csv"
fn_trap = "data_input/foco_trap.csv"
fn_database_input = "data_input/wnv-s_database.csv" 






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# FILE NAMES MID
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn_database_update = "data_mid/wnv-s_database_update.csv"

fn_datasheet_clean = paste0("data_mid/","y",fn_year, "_", "w",fn_week, "_datasheet.csv")

fn_datasheet_clean_test = paste0("data_mid/","y",fn_year, "_", "w",fn_week, "_datasheet_test.csv")

fn_weekly_input_format_mid = "data_mid/weekly_data_input_format_mid.RData"

fn_cq_out = paste0("data_mid/","y",fn_year, "_", "w",fn_week, "_platemap.csv")

fn_abund_out = paste0("data_mid/","y",fn_year, "_", "w",fn_week, "_abundance")

fn_pools_mid = paste0("data_mid/","y",fn_year, "_", "w",fn_week, "_pools.csv")

fn_inactive_trap = "data_mid/inactive_traps.csv"

fn_func_trap = "data_mid/functional_traps.csv"

fn_max_trap_yr = "data_mid/max_trap_zone_yr.csv"


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FILE NAMES OUTPUT
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fn_gdrive_archive = paste0("wnv-s_database_pre_y",year_filter, "_w", week_filter,".gsheet")

fn_data_output = paste0("data_output/","y",fn_year, "_", "w",fn_week, "_data_update.csv")

fn_weekly_input_format = "data_output/weekly_data_input_format.csv"

fn_stds_ctrl_slev_bird = "data_output/std_ctrl_slev_bird.csv"

fn_non_database_sample = "data_output/non_database_samples(std-ctrl-bird-etc).csv"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#SELECTION COLUMN SETTINGS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
trap_col = c("zone", "lat", "long")

input_data_col = c("csu_id", "trap_id", "year", "week", "trap_date", 
                   "county", "method", "spp", "total", "test_code", "zone")

data_col = c("csu_id", "trap_id", "year", "week", "trap_date", 
             "county", "method", "genus", "spp", "sex", "no_gravid",
             "no_deplete", "total", "test_code", "seq", "cq", 
             "zone", "lat", "long")

database_col = c( "csu_id", "trap_id", "year", "week", "trap_date", "county", "method", "spp",
  "total", "test_code", "cq", "copies_WNV", "seq", "zone", "cq", "lat", "long")

pcr_check_col <- c("csu_id", "test_code", "ct_threshold", "plate", "copies_SLEV", "copies_WNV", "cq_SLEV", "cq")

class_col <- c("csu_id" = "character", 
            "trap_id" = "character", 
            "year" = "numeric", 
            "week" = "numeric", 
            "trap_date" = "character", 
            "county" = "character", 
            "method" = "character", 
            "genus" = "character", 
            "spp" = "character", 
            "sex" = "logical", 
            "no_gravid" = "numeric", 
            "no_deplete" = "numeric", 
            "total" = "numeric", 
            "test_code" = "numeric", 
            "seq" = "numeric", 
            "cq" = "numeric",
            "zone" = "character",
            "lat" = "numeric",
            "long" = "numeric")

rename_col <- c("csu_id" = "CSU Pool Number (CMC Enters)" , 
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


weekly_input_report_format <- c(
  "Year",
  "CSU Pool Number (CMC Enters)",
  "IDA Pool (CSU Enters, Leave Blank)",
  "Week",
  "Trap Date",
  "County",
  "Account",
  "Collection Site       (Trap ID)" ,
  "Zone",
  "Method",
  "Genus",
  "SPP",
  "Sex",
  "No. Gravid",
  "No. Deplete",
  "Total",
  "Test Code (CSU Enters)",
  "Test Result (CSU Enters)",
  "Comments"
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GROUP VARS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
grp_vars = c("year", "week", "zone", "spp")
hx_grp_vars = c("week", "zone")
zone_lvls = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC", "WC")
non_routine_zones = c("BC", "WC")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#COLOR SETTINGS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
color_palette = wes_palette('Darjeeling1')

year_col_highlight = "2023"

year_cols_df = data.frame(year = 2000:2050) %>%
  mutate(year = as.factor(year)) %>%
  mutate(col = if_else(as.character(year) == year_col_highlight,
                       year_col_highlight,
                       "other"))

year_cols = c("2023" = "darkred",
              "other" = "grey50")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

