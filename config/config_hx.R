#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# H E A D E R
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE:
#1 checks and loads necessay package for pipeline
#2 defines input data directory
#3 checks if input directories exist
#4 checks if input files exist
#5 defines google sheet keys
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(argparse, #importing and exporting
                 lubridate
  )
})


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------ U S E R   D E F  V A R I A B L E   D A T A   P A R A M E T E R S : -----------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#variables that will change from week to week
year_hardcode = lubridate::isoyear(Sys.Date()) 
# Create an argument parser
parser <- ArgumentParser(description = "Script to handle config file data inputs")

# Add arguments for fn_trap and fn_database_input
parser$add_argument("--input", help = "name of input folder", type = "character", default = "1_input")
parser$add_argument("--year", help = "year of the report", type = "integer", default = year_hardcode)
parser$add_argument("--year_hx", help = "start to historical calculations", type = "integer", default = 2017)
parser$add_argument("--cp_threshold", help = "copy threshold for positive", type = "integer", default = 500)
parser$add_argument("--rn_threshold", help = "flourescent threshold for determining ct", type = "integer", default = 34000)
parser$add_argument("--vi_threshold", help = "vi threshold for plots", type = "integer", default = 0.75)
parser$add_argument("--update", help = "logical argument whether or not to update directory with data from gdrive", type = "logical", default = F)
parser$add_argument("--download", help = "logical argument whether or not to download data from gdrive", type = "logical", default = F)
parser$add_argument("--clean", help = "logical argument whether or not to clean directory with data in repository", type = "logical", default = F)

# Parse arguments
args <- parser$parse_args()

# rename values to match existing names in scripts
dir_input <- args$input

copy_threshold <- args$cp_threshold
rn_threshold = args$rn_threshold
vi_threshold = args$vi_threshold
download <- args$download
update <- args$update
clean <- args$clean

#DATES
year_filter <- args$year
year_filter_hx = seq(args$year_hx, year_filter-1, by = 1)
week_filter_hx = 23:37


rm(week_hardcode, year_hardcode)




# Output the values
cat("Input folder set to:", dir_input, "\n")
cat("Year filter set to:", year_filter, "\n")
cat("Copy threshold set to:", copy_threshold, "\n")
cat("Fluorescent threshold set to:", args$rn_threshold, "\n")
cat("VI threshold set to:", args$vi_threshold, "\n")
cat("Update directory set to:", update, "\n")
cat("Download Googledrive files set to:", download, "\n")
cat("Clean directory set to:", clean, "\n")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ---------------- D E F I N E   G S H E E T   K E Y --------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
key_database_gsheet = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
key_database_culex_sheet = "15icsYx5SkmQIPXV3Vzj6PhyX81M55mIAYfKDj5K3gac"
key_foco_trap = "1G5UcRmcVsVpMtKW_-4WLKX8WT_fvyIYj7sGcoNxDmr4"

fn_trap <- file.path(dir_input, "foco_trap.csv")
fn_gdrive_database = file.path(dir_input,"wnv-s_database.csv") 
fn_gdrive_culex_sheet = file.path(dir_input,"culex_sheet_database.csv")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------- U S E R   D E F I N E D   G R O U P I N G   V A R I A B L E S -------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

zone_lvls = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC")
grp_zones = c("NE", "SE", "NW", "SW","LV", "BC", "BE")
fc_zones = c("NE", "SE", "NW", "SW")
non_fc_zones = c("LV", "BC", "BE")
grp_vars = c("year", "week", "zone", "spp")
grp_var_sym = rlang::syms(grp_vars)
hx_grp_vars = c("week", "zone")
non_routine_zones = c("BC") # <<<<<<<--------------------------------------------------------------------- U S E R   I N P U T


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- O U T F I L E  N A M E  G E N E R A T I O N --------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(length(year_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_year = paste0(min(year_filter), "-", max(year_filter))
} else { #otherwise fn_year stays the same
  fn_year = year_filter
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------- E X P O R T   C O N F I G ----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

config_fn = paste0("config/config_hx_settings.RData")

all_params <- ls(envir = .GlobalEnv)

# Create a list containing all the values of these objects
all_params_list <- mget(all_params, envir = .GlobalEnv)

saveRDS(all_params_list, config_fn)
















