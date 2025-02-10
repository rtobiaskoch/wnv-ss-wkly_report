#generate report
list2env(readRDS(config_params_file),           envir = .GlobalEnv)
#get tables
file_names = list.files(path = "data_output",
                pattern = "table",
                full.names = T)

#add weeky input
file_names = c(fn_weekly_input_format, file_names)

sheet_names = c('Weekly Data Input',
             "t1a", "t1b", 
             "t2a", 't2b',
             't3a', 't3b')



# Check if files exist
if (!all(file.exists(file_names))) {
  stop("One or more files do not exist.")
}

suppressMessages(
  

# Read files into a list of data frames with error handling
dataframes <- map(file_names, function(file) {
  tryCatch({
    read_csv(file) #using instead of read.csv because the colnames got messed up otherwise
  }, error = function(e) {
    message(paste("Error reading file:", file))
    NULL
  })
})

)

# Remove NULLs if any files failed to read
dataframes <- dataframes[!sapply(dataframes, is.null)]

# Ensure we have the same number of data frames as sheet names
if (length(dataframes) != length(sheet_names)) {
  stop("Mismatch between number of data frames and sheet names.")
}

# Create a new workbook
wb <- createWorkbook()

# Add each data frame as a new sheet in the workbook
walk2(dataframes, sheet_names, function(data, sheet) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, data)
})

# Save the workbook to a file
saveWorkbook(wb, "data_output/weekly_report_output.xlsx", overwrite = TRUE)

gsheet <- gs4_create("weekly_report_R_output")

# Write each data frame to a new sheet in the Google Sheet
walk2(dataframes, sheet_names, function(data, sheet) {
  sheet_write(data, ss = gsheet, sheet = sheet)
})

sheet_delete(gsheet, sheet = "Sheet1")

# Locate the target folder in Google Drive
target_folder <- drive_get("weekly_report")

# Move the Google Sheet to the target folder
drive_mv(gsheet, path = as_id(target_folder))








