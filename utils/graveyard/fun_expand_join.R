
expand_join <- function(id_expand,  # data with id's that you want to expand
                        id_name,    # name of the id column in your data 
                        data,       # data you want to expand the ids by
                        fill_drop,  #column that will not have matches that you want to fill with other col from data
                        ...) {      # columns to expand on
  
  # Capture the dots as column names
  dots <- rlang::enquos(...)
  col_names <- purrr::map_chr(dots, rlang::as_name)
  join_by <- c(id_name, col_names)  # create vector for the left_join
  fill_grp = rlang::syms(setdiff(join_by, fill_drop))
  
  # Create list of unique NON-NA values for each expansion variable
  expand_values <- purrr::map(col_names, ~ unique(na.omit(data[[.x]])))
  expand_values <- purrr::map(col_names, ~ unique(data[[.x]]))
  names(expand_values) <- col_names
  
  # Add id_expand with the correct name (id_name)
  expand_values <- c(stats::setNames(list(id_expand), id_name), expand_values)
  
  # Create all combinations
  exp <- do.call(expand.grid, expand_values)
  
  # Perform the join
  result <- dplyr::left_join(exp, data, by = join_by) %>%
    group_by(!!!fill_grp) %>%
    fill(-total, .direction = "downup")
  
  return(result)
}

# # Usage example:
# result <- expand_join(
#   id_expand = c("FC-001", "LV-020", "LC-049"),  # Your trap IDs
#   id_name = "trap_id",                          # Name of ID column in your data
#   data = all_spp_active,                        # Your data frame
#   fill_drop = "spp",
#   year, week, spp                              # Variables to expand on
# )
