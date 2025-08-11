insert_blank_row <- function(df, after_row) {
  # Create a blank row with the same column structure
  blank_row <- df[1, ]  # Copy structure
  blank_row[] <- NA     # Fill with NAs
  
  if (after_row >= nrow(df)) {
    # Insert at end
    return(rbind(df, blank_row))
  } else {
    # Insert between rows
    return(rbind(df[1:after_row, ], blank_row, df[(after_row+1):nrow(df), ]))
  }
}

# Usage: insert blank row after row 2
df_new <- insert_blank_row(df, 2)