read_pcr = function(path, sheet) {
  library(purrr)
  library(dplyr)
  library(readxl)
  
  path %>% 
    map_dfr(~ {
      # Read the Excel file
      data = read_excel(.x, col_names = TRUE, sheet = sheet)
      
      # Get the first column name
      first_col_name = names(data)[1]
      
      # Filter by first column to keep only wells 1-96 or "Well"
      data_filtered = data %>% 
        filter(!!sym(first_col_name) %in% c("Well", as.character(1:96)))
      
      # Set column names from the first row
      if(nrow(data_filtered) > 0) {
        colnames(data_filtered) = data_filtered[1,] %>% as.character()
        
        # Get the new first column name after renaming
        new_first_col = names(data_filtered)[1]
        
        # Remove the header row and add file name
        data_filtered = data_filtered %>% 
          filter(!!sym(new_first_col) != "Well") %>%  # Remove the header row
          mutate(file_name = basename(.x))  # Add file name
      }
      
      return(data_filtered)
    })
}