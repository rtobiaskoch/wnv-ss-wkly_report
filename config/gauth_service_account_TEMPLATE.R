#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# G O O G L E   S E R V I C E   A C C O U N T   A U T H   T E M P L A T E
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# PURPOSE: Authenticate with Google Drive/Sheets using a service account
#          instead of an individual user account. This lets anyone with the
#          JSON key run the pipeline without a personal Google login.
#
# SETUP STEPS (one-time, done by repo admin):
#
#  1. Go to https://console.cloud.google.com/
#  2. Create a project (or select existing)
#  3. Enable these APIs:
#       - Google Drive API
#       - Google Sheets API
#  4. IAM & Admin → Service Accounts → Create Service Account
#       - Name: e.g. "wnv-pipeline-bot"
#       - Role: not needed at project level
#  5. Click the service account → Keys → Add Key → JSON
#       - Download the JSON file. Keep it SECRET — never commit it.
#  6. Share each Google Sheet with the service account email
#       (looks like: wnv-pipeline-bot@your-project.iam.gserviceaccount.com)
#       Give it Editor access so the pipeline can write back results.
#  7. Share the Google Drive output folder with the same service account email.
#
# USAGE IN PIPELINE:
#
#  Option A — env var (recommended for CI/CD / shinyapps.io):
#    Set GOOGLE_APPLICATION_CREDENTIALS=/path/to/sa_key.json in your environment.
#    Then call:
#      googledrive::drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
#      googlesheets4::gs4_auth(path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
#
#  Option B — local file (for personal use / testing):
#    Store the JSON key at a path NOT tracked by git (e.g. ~/.config/sa_key.json)
#    Then call:
#      googledrive::drive_auth(path = "~/.config/wnv_sa_key.json")
#      googlesheets4::gs4_auth(path = "~/.config/wnv_sa_key.json")
#
# Example code to add at the top of load_packages.R or the pipeline .qmd:
# -------------------------------------------------------------------------
# sa_key <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = "")
# if (nchar(sa_key) > 0 && file.exists(sa_key)) {
#   googledrive::drive_auth(path = sa_key)
#   googlesheets4::gs4_auth(path = sa_key)
#   message("Authenticated via service account.")
# } else {
#   # Fall back to interactive OAuth for local development
#   googledrive::drive_auth()
#   googlesheets4::gs4_auth(token = googledrive::drive_token())
#   message("Authenticated via interactive OAuth.")
# }
# -------------------------------------------------------------------------
#
# NEVER store the actual JSON key in this file or commit it to git.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
