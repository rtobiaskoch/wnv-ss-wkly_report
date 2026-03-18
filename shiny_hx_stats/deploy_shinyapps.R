#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# D E P L O Y   S H I N Y   A P P   T O   S H I N Y A P P S . I O
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# PURPOSE: Deploy the shiny_hx_stats app so anyone can use it in a browser
#          without installing R.
#
# PREREQUISITES:
#   1. Create a free account at https://www.shinyapps.io
#   2. Install rsconnect:  install.packages("rsconnect")
#   3. Get your token from shinyapps.io:
#        Account → Tokens → Show → Copy to clipboard
#   4. Paste and run the setAccountInfo() call below (once, on your machine).
#      DO NOT commit that call — it contains your secret token.
#
# USAGE:
#   Rscript shiny_hx_stats/deploy_shinyapps.R
#   OR source this file from R console.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
library(rsconnect)

# --- ONE-TIME SETUP (run interactively, never commit) ----------------------------
# rsconnect::setAccountInfo(
#   name   = "YOUR_SHINYAPPS_USERNAME",
#   token  = "YOUR_TOKEN",
#   secret = "YOUR_SECRET"
# )

# --- DEPLOY -----------------------------------------------------------------------
rsconnect::deployApp(
  appDir     = "shiny_hx_stats",
  appName    = "wnv-mosquito-surveillance",
  appTitle   = "WNV/SLEV Mosquito Surveillance — Historical Stats",
  forceUpdate = TRUE
)
