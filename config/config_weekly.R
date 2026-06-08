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
  library(wnvSurv)  # SSOT: calc_season_week() for the --week default
})


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------ U S E R   D E F  V A R I A B L E   D A T A   P A R A M E T E R S : -----------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#variables that will change from week to week
week_hardcode = wnvSurv::calc_season_week(Sys.Date())  # seasonal week (week 23 = first full week of June, leap-week-stable)
year_hardcode = lubridate::isoyear(Sys.Date())
# Create an argument parser
parser <- ArgumentParser(description = "Script to handle config file data inputs")

# Add arguments for fn_trap and fn_database_input
parser$add_argument("--input", help = "name of input folder", type = "character", default = "1_input")
parser$add_argument("--week", help = "week of report", type = "integer", default = week_hardcode)
parser$add_argument("--year", help = "year of the report", type = "integer", default = year_hardcode)
parser$add_argument("--n_hx_years", help = "number of prior seasons in the rolling historical baseline", type = "integer", default = 5)
parser$add_argument("--cp_threshold", help = "copy threshold for positive", type = "integer", default = 500)
parser$add_argument("--rn_threshold", help = "flourescent threshold for determining ct", type = "integer", default = 34000)
parser$add_argument("--vi_threshold", help = "vi threshold for plots", type = "integer", default = 0.75)
parser$add_argument("--update", help = "logical argument whether or not to update directory with data from gdrive", type = "logical", default = F)
parser$add_argument("--download", help = "logical argument whether or not to download data from gdrive", type = "logical", default = F)
parser$add_argument("--push", help = "logical argument whether or not to push directory with data to repository", type = "logical", default = F)

# Parse arguments
args <- parser$parse_args()

# dir_base_input defaults to "1_input"; pass --input to override for staging environments
dir_base_input <- args$input

copy_threshold <- args$cp_threshold
rn_threshold = args$rn_threshold
vi_threshold = args$vi_threshold
download <- args$download
update <- args$update
push <- args$push

#DATES
week_filter <- args$week
year_filter <- args$year
# Rolling historical baseline = the n_hx_years seasons immediately before the
# report year (default 5). Advances by one each season; does NOT widen over time.
# For --year 2026 with default 5 -> seq(2021, 2025).
year_filter_hx = seq(year_filter - args$n_hx_years, year_filter - 1, by = 1)
week_filter_yr= 23:week_filter
week_filter_hx = 23:37


rm(week_hardcode, year_hardcode)

# Output the values
cat("Input base folder set to:", dir_base_input, "\n")
cat("Week filter set to:", week_filter, "\n")
cat("Year filter set to:", year_filter, "\n")
cat("Copy threshold set to:", copy_threshold, "\n")
cat("Fluorescent threshold set to:", args$rn_threshold, "\n")
cat("VI threshold set to:", args$vi_threshold, "\n")
cat("Update directory set to:", update, "\n")
cat("Download Googledrive files set to:", download, "\n")
cat("push directory set to:", push, "\n")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# D E F I N E   D I R E C T O R I E S   &   I N P U T   F I L E S 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# construct week-scoped paths from year and week args
dir_input  <- file.path(dir_base_input, year_filter, paste0("w", week_filter))
dir_datasheet <- file.path(dir_input, "datasheet")
dir_pcr       <- file.path(dir_input, "pcr")
dir_platemap  <- file.path(dir_input, "platemap")
dir_all_spp   <- file.path(dir_input, "all_species")

# output dirs — auto-created later if absent
dir_mid    <- file.path("2_mid",    year_filter, paste0("w", week_filter))
dir_output <- file.path("3_output", year_filter, paste0("w", week_filter))
dir_plots  <- file.path(dir_output, "plots")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# C H E C K   F O R   M I S S I N G   D I R E C T O R I E S
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# --- Input: hard stop if missing (user must populate before running) ---
if (!dir.exists(dir_input)) {
  stop(paste0(
    "Input directory missing: ", dir_input,
    "\nCreate and populate it with datasheet/, pcr/, platemap/, all_species/ before running."
  ))
}

# --- Input subdirs: hard stop if empty or missing ---
input_subdirs <- list(
  datasheet = dir_datasheet,
  pcr       = dir_pcr,
  platemap  = dir_platemap,
  spp       = dir_all_spp
)

empty_dirs <- c()
for (dir_name in names(input_subdirs)) {
  d <- input_subdirs[[dir_name]]
  if (!dir.exists(d) || length(list.files(d)) == 0) {
    empty_dirs <- c(empty_dirs, d)
  }
}

if (length(empty_dirs) > 0) {
  stop(paste0(
    "The following input subdirectories are empty or missing:\n",
    paste(empty_dirs, collapse = "\n"),
    "\nPlease populate them before running.",
    "\nSee 0_R/0_check_load_input_data.R for download helpers."
  ))
} else {
  cat("All input subdirectories are populated.\n")
}

# --- Output dirs: auto-create if missing ---
for (d in c(dir_mid, dir_output, dir_plots)) {
  if (!dir.exists(d)) {
    if (!dir.create(d, recursive = TRUE)) {
      stop(paste0("Could not create output directory: ", d, "\nCheck filesystem permissions."))
    }
    cat("Created directory:", d, "\n")
  }
}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ---------------- D E F I N E   G S H E E T   K E Y --------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
key_database_gsheet = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
key_database_gsheet_slev = "1JsOz5wVl5WSpv1KGqiBFGVjoypL8xZMti5Syg07cPbo"
key_database_culex_sheet = "15icsYx5SkmQIPXV3Vzj6PhyX81M55mIAYfKDj5K3gac"
key_database_folder = "1AIpZAIRSG-DZug74Av3u_7kdyXYsrjKF"
key_trap_gsheet = "1G5UcRmcVsVpMtKW_-4WLKX8WT_fvyIYj7sGcoNxDmr4"
key_rename_key = "1UDLN-K4TJ-Ok_6NXUQNwUTKMNgfAq1xx9HABALCYRNQ"
key_standards_gsheet = "1bSMYQ4bZ9uBfrOQ6ylsegNmmGYdf9YFVbxB4qBhnFQo"
key_foco_trap = "1G5UcRmcVsVpMtKW_-4WLKX8WT_fvyIYj7sGcoNxDmr4"
key_birds = "1YYjQmgNV92y1D39Nt5Y5NG0f_j4sVqVsJSSoUuc2u8k"
key_plots_dir = "1yVA3bOcQ3i4a_GyAagHIYDGPAkeaDY02"
key_inconclusive = "1l90THbcNgUgdO6dUbFXYo6IxVc3VwutKVlx3Cu9cgEQ"

fn_gdrive_database = file.path(dir_input,"wnv-s_database.csv") # <<<<<<<--------------------------------------------------- U S E R   I N P U T
fn_gdrive_database_slev = file.path(dir_input,"slev-s_database.csv") # <<<<<<<--------------------------------------------------- U S E R   I N P U T
fn_gdrive_culex_sheet = file.path(dir_input,"culex_sheet_database.csv") # <<<<<<<--------------------------------------------------- U S E R   I N P U T
fn_key_rename <- file.path(dir_input, "key_rename.csv")
fn_trap <- file.path(dir_input, "foco_trap.csv")
fn_standards <- file.path(dir_input, "standards_input.csv")
fn_inconclusive = file.path(dir_input, "amp_inconclusive_negatives.csv")


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

if(length(week_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_week = paste0(min(week_filter), "-", max(week_filter))
} else { #otherwise fn_year stays the same
  fn_week = week_filter
}



file_prefix = paste0("y", fn_year, "_", "w", fn_week, "_")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------- F I L E  N A M E S  M I D ------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
dir_mid_wk = paste0(dir_mid,"/", file_prefix)

fn_database_update = file.path(dir_mid, "wnv-s_database_update.csv")
fn_database_slev_update = file.path(dir_mid, "slev-s_database_update.csv")
fn_database_culex_update = file.path(dir_mid, "culex_database_update.csv")
fn_datasheet_clean = paste0(dir_mid,"/", file_prefix, "datasheet.csv")
fn_datasheet_clean_test = paste0(dir_mid,"/", file_prefix, "_datasheet_test.csv")
fn_weekly_input_format_mid = file.path(dir_mid, "weekly_data_input_format_mid.RData")
fn_cq_out = paste0(dir_mid,"/", file_prefix, "pcr_clean.RData")
fn_cq_data = paste0(dir_mid_wk, "cq_data.RData")
fn_abund_out = paste0(dir_mid,"/", file_prefix, "abundance.csv")
fn_pools_mid = paste0(dir_mid,"/", file_prefix, "pools.csv")
fn_inactive_trap = file.path(dir_mid, "inactive_traps.csv")
fn_func_trap = file.path(dir_mid, "functional_traps.csv")
fn_max_trap_yr = file.path(dir_mid, "max_trap_zone_yr.csv")
fn_trap_p_wk = file.path(dir_mid, "trap_p_wk.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- F I L E  N A M E S   O U T P U T ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fn_gdrive_archive = paste0("wnv-s_database_pre_y",year_filter, "_w", week_filter,".gsheet")
fn_data_output = paste0(dir_output,"/", file_prefix, "_data_update.csv")
fn_weekly_input_format = file.path(dir_output, "weekly_data_input_format.csv")
fn_stds_ctrl_slev_bird = file.path(dir_output, "std_ctrl_slev_bird.csv")
fn_non_database_sample = file.path(dir_output, "non_database_samples(std-ctrl-bird-etc).csv")
fn_standards_output = file.path(dir_output, "standards_new.csv")
weekly_report_folder = "25_weekly_report"
fn_bird_output = paste0(dir_output, "/",  file_prefix, "rmrp.csv")
dir_plot_wk = paste0(dir_plots,"/", file_prefix)
fn_pcr_plot = paste0(dir_plot_wk, "pcr_plot.png")
fn_p_hx = paste0(dir_plot_wk, "hx_plot_all.png")
fn_report = paste0("y", year_filter, "_w", week_filter, "_weekly_report_output")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------------------- C O L U M N  S E L E C T I O N -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
col_trap = c("zone", "lat", "long")

col_input_database = c("csu_id", "trap_id", "year", "week", "trap_date", 
                   "county", "method", "spp", "total", "test_code", "zone")

col_datasheet = c("csu_id", "trap_id", "year", "week", "trap_date", 
             "county", "method", "genus", "spp", "sex", "no_gravid",
             "no_deplete", "total", "test_code", "seq", "cq", 
             "zone", "lat", "long")

col_database = c( "csu_id", "trap_id", "zone", "year", "week", "trap_date","method", "spp",
  "total", "test_code", "cq", "copies", "seq", "notes")

col_pcr_check <- c("csu_id", "test_code", "ct_threshold", "plate", "copies_SLEV", "copies_WNV", "cq_SLEV", "cq")

col_class_database <- c("csu_id" = "character", 
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



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------- C O L O R  S E T T I N G S -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


pal_mozzy = c("hx_Tarsalis" = "grey50",
              "hx_Pipiens" = "grey30",
              "current_Tarsalis" = "#e9724c",
              "current_Pipiens" = "#820263")

pal_mozzy2 = c("Pipiens" = "#820263",
               "Tarsalis" = "#e9724c",
               "All" = "#faa916")


pal_mozzy3 = c("#ffc857", "#e9724c", "#c5283d")

curr_hx_pal = c("current" = "#e9724c",
                "hx"      = "grey50")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------- E X P O R T   C O N F I G ----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# must be run from repo root — relative path resolves from working directory
config_fn <- file.path("config", "config_weekly_settings",
                        paste0(file_prefix, "config_weekly_", Sys.time(), ".RData"))

all_params <- ls(envir = .GlobalEnv)

# Create a list containing all the values of these objects
all_params_list <- mget(all_params, envir = .GlobalEnv)

saveRDS(all_params_list, config_fn)

# co-locate config with the input data it describes (traceability)
saveRDS(all_params_list, file.path(dir_input, paste0(file_prefix, "config_weekly.RData")))
cat("Config snapshot saved to:", file.path(dir_input, paste0(file_prefix, "config_weekly.RData")), "\n")


