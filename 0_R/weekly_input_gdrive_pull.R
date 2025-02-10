library(googledrive)

download_platemaps_from_folders <- function(year_filter, week_filter, platemap_folder = "platemap") {
  # Authenticate with Google Drive (if not already done)
  drive_auth()
  
  # Find the folder for the specified year
  year_folder <- drive_find(as.character(year_filter), 
                            type = "folder",
                            )
  
  if (nrow(year_folder) == 0) {
    stop("No folder found for the specified year.")
  } else if (nrow(year_folder) > 1) {
    stop("Multiple folders found for the specified year. Please be more specific.")
  }
  
  # Find the folder for the specified week within the year folder
  week_folder <- drive_find(week_filter, type = "folder", path = as_id(year_folder$id))
  
  if (nrow(week_folder) == 0) {
    stop("No folder found for the specified week within the year folder.")
  } else if (nrow(week_folder) > 1) {
    stop("Multiple folders found for the specified week. Please be more specific.")
  }
  
  # Find the platemap folder within the week folder
  platemap_folder <- drive_find(platemap_folder, type = "folder", path = as_id(week_folder$id))
  
  if (nrow(platemap_folder) == 0) {
    stop("No platemap folder found within the week folder.")
  } else if (nrow(platemap_folder) > 1) {
    stop("Multiple platemap folders found. Please be more specific.")
  }
  
  # Find all files within the platemap folder
  files <- drive_ls(path = as_id(platemap_folder$id))
  
  # Create a local directory for the downloads
  local_dir <- file.path(year_filter, week_filter, platemap_folder$name)
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Download each file to the local directory
  for (file in files) {
    drive_download(file, path = file.path(local_dir, file$name), overwrite = TRUE)
  }
  
  message("Downloaded platemaps to: ", local_dir)
}

# Example usage:
download_platemaps_from_folders("2024", "w27")
