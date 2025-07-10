gsheet_pull_prompt <- function(filename, sheet = "Sheet1", key, force_update = FALSE) {
  require(googlesheets4)
  require(googledrive)
  require(dplyr)
  
  # Check if filename ends in csv
  if (!grepl("\\.csv$", filename)) {
    stop("Filename must end in .csv")
  }
  
  # Determine if we're in an interactive session
  is_interactive <- interactive()
  
  # Handle file existence and user prompt
  if (file.exists(filename)) {
    if (is_interactive && !force_update) {
      # Interactive mode - ask user
      user_input <- readline(prompt = paste0(filename, " exists. Would you like to replace it from Google Drive? (y/n): "))
      should_download <- tolower(user_input) == "y"
    } else {
      # Non-interactive mode (like Quarto render) - default to not overwriting
      message(filename, " exists. Running in non-interactive mode so not overwriting (use force_update=TRUE to override).")
      should_download <- force_update
    }
  } else {
    # File doesn't exist - we should download
    should_download <- TRUE
  }
  
  # Download logic
  if (should_download) {
    # Authenticate if needed
    if (!googledrive::drive_has_token()) {
      googledrive::drive_auth()
    }
    
    # Read and process data
    mdata <- googlesheets4::read_sheet(key, sheet = sheet)
    
    message("\nRead googlesheet with ", nrow(mdata), " rows and ", ncol(mdata), 
            " columns and following headers:\n", paste(names(mdata), collapse = ", "), "\n")
    
    mdata <- mdata %>%
      dplyr::mutate(across(where(is.list), ~ sapply(., paste, collapse = ", ")))
    
    write.csv(mdata, filename, row.names = FALSE, na = "")
    message("\n...Checking that gdrive download worked...\n")
  } else {
    message("\nFile ", filename, " download skipped.\n")
  }
  
  # Final verification
  if (!file.exists(filename)) {
    stop(filename, " file doesn't exist. Please reauthenticate googledrive, check inputs and rerun.")
  } else {
    message(filename, " exists.\n")
  }
  
  return(invisible(TRUE))
}