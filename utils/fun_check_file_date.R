check_files_date <- function(directory = ".") {
  # Get current date and calculate start of week (Monday)
  current_date <- Sys.Date()
  week_start <- current_date - as.numeric(format(current_date, "%u")) + 1
  
  # List all files in the directory
  files <- list.files(directory, full.names = TRUE)
  
  # Get file info (including modification time)
  file_info <- file.info(files)
  
  # Filter files modified this week
  current_week_files <- files[file_info$mtime >= week_start]
  
  if (length(current_week_files) == 0) {
    warning("Warning: Your config file was from a previous week. You may need to edit and run config/run_config.sh with the appropriate arguments")
    return(FALSE)
  } else {
    message(paste(length(current_week_files), "file(s) found from current week"))
    return(TRUE)
  }
}

# Example usage:
# check_files_date("path/to/your/directory")