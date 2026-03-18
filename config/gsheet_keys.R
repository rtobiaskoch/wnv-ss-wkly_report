#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# G O O G L E   S H E E T   K E Y S
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# All Google Sheet IDs and Drive folder IDs are defined here in one place.
# To give collaborators access to a sheet, share it in Google Drive with their account
# (or a service account JSON key — see config/gauth_service_account_TEMPLATE.R).
#
# Sheet IDs can be found in the Google Sheets URL:
#   https://docs.google.com/spreadsheets/d/<SHEET_ID>/edit
#
# To override with environment variables (recommended for CI/CD or shared deployments):
#   set GSHEET_WNV_DATABASE=<your_key> in your .Renviron or shell before running.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Helper: read from env var if set, otherwise fall back to hard-coded default
.gkey <- function(env_var, default) {
  val <- Sys.getenv(env_var, unset = "")
  if (nchar(val) > 0) val else default
}

# ---- DATABASES (read & updated by pipeline) --------------------------------------
key_database_gsheet       <- .gkey("GSHEET_WNV_DATABASE",    "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA")
key_database_gsheet_slev  <- .gkey("GSHEET_SLEV_DATABASE",   "1JsOz5wVl5WSpv1KGqiBFGVjoypL8xZMti5Syg07cPbo")
key_database_culex_sheet  <- .gkey("GSHEET_CULEX_DATABASE",  "15icsYx5SkmQIPXV3Vzj6PhyX81M55mIAYfKDj5K3gac")

# ---- REFERENCE / LOOKUP SHEETS (read-only by pipeline) --------------------------
key_trap_gsheet   <- .gkey("GSHEET_FOCO_TRAP",    "1G5UcRmcVsVpMtKW_-4WLKX8WT_fvyIYj7sGcoNxDmr4")
key_foco_trap     <- key_trap_gsheet   # alias — same sheet
key_rename_key    <- .gkey("GSHEET_RENAME_KEY",   "1UDLN-K4TJ-Ok_6NXUQNwUTKMNgfAq1xx9HABALCYRNQ")
key_inconclusive  <- .gkey("GSHEET_INCONCLUSIVE", "1l90THbcNgUgdO6dUbFXYo6IxVc3VwutKVlx3Cu9cgEQ")

# ---- LAB DATA SHEETS (read & updated by pipeline) --------------------------------
key_standards_gsheet <- .gkey("GSHEET_STANDARDS", "1bSMYQ4bZ9uBfrOQ6ylsegNmmGYdf9YFVbxB4qBhnFQo")
key_birds            <- .gkey("GSHEET_BIRDS",      "1YYjQmgNV92y1D39Nt5Y5NG0f_j4sVqVsJSSoUuc2u8k")

# ---- GOOGLE DRIVE FOLDERS --------------------------------------------------------
key_database_folder <- .gkey("GDRIVE_DATABASE_FOLDER", "1AIpZAIRSG-DZug74Av3u_7kdyXYsrjKF")
key_plots_dir       <- .gkey("GDRIVE_PLOTS_FOLDER",    "1yVA3bOcQ3i4a_GyAagHIYDGPAkeaDY02")

rm(.gkey)  # clean up helper from global env
