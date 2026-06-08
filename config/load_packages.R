#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#L O A D   P A C K A G E S   F O R   P I P E L I N E 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(
    argparse, # config
    googlesheets4, googledrive, rio, readxl, openxlsx, googledrive, # importing and exporting
    tidyverse, janitor, lubridate, rquery, stringr, # manipulation
    devtools, cli, # analysis
    ggpubr, wesanderson, paletteer, leaflet, patchwork, plotly # plotting
  )
  Sys.setenv(R_QPDF="true") # to build the vignette during the package build
  library(devtools) # need to install this if you do not have it
  devtools::install_github("https://github.com/CDCgov/PooledInfRate",build_vignettes = TRUE)
  # wnvSurv: single source of truth for shared surveillance functions
  # (calc_season_week, etc). Installed locally from ../wnv-ss_functions —
  # not on CRAN, so loaded explicitly rather than via pacman::p_load.
  library(wnvSurv)

})

# --- Google auth account (per-user; NOT hardcoded) ---------------------------
# Pushing to the master Google Sheets (update = T) needs gargle to know WHICH
# cached account/token to use. A non-interactive `quarto render` will not prompt,
# so we resolve the email here and set `gargle_oauth_email`. Resolution order:
#   1. GARGLE_OAUTH_EMAIL env var   (each user sets this once in ~/.Renviron)
#   2. interactive prompt           (when run in RStudio / an interactive R session)
# This only SELECTS among cached tokens — every user still authenticates ONCE
# interactively to populate the cache:
#   googledrive::drive_auth(email = "<you>@gmail.com")
#   googlesheets4::gs4_auth(email  = "<you>@gmail.com")
gargle_email <- Sys.getenv("GARGLE_OAUTH_EMAIL", unset = "")
if (!nzchar(gargle_email) && interactive()) {
  gargle_email <- trimws(readline(
    "Google account email for Drive/Sheets push (blank to skip): "))
}
if (nzchar(gargle_email)) {
  options(gargle_oauth_email = gargle_email)
  message("gargle OAuth email set to: ", gargle_email)
} else {
  message("No Google email set (GARGLE_OAUTH_EMAIL unset and non-interactive). ",
          "A GSheet push (update = T) will fail until you set GARGLE_OAUTH_EMAIL ",
          "in ~/.Renviron and authenticate once with drive_auth()/gs4_auth().")
}