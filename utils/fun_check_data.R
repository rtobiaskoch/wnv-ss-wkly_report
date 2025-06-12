check_data <- function(df, trap, year_filter, week_filter) {
  
  # Validate year
  if (unique(df$year) != year_filter) {
    stop("The year in your df doesn't match the config year filter")
  }
  
  # Validate week
  if (unique(df$week) != week_filter) {
    stop("The week in your df doesn't match the config week filter")
  }
  
  # Check if all traps in df match trap
  if (all(df$trap_id %in% trap$trap_id)) {
    cat("\nAll traps in df match trap data\n")
  }
  
  
  
    # Identify missing traps and count by zone
    missing_traps <- anti_join(trap, df, by = "trap_id") %>%
      mutate(trap_status = "missing")
    
    if(nrow(missing_traps) == 0) {
      cat("All active foco_traps are present in the data") } else {
        cat("The following traps are not represented in the data:\n",
            paste(missing_traps$trap_id, sep = "\n"), "\nCheck data in missing traps.")
      }
    
    missing_traps %>%
      group_by(zone) %>%
      count()
    
    return(missing_traps)

  
  
 # invisible(NULL)
}
