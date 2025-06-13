get_abund = function(df, 
                     spp_keep = c("Pipiens", "Tarsalis"), 
                     grp_var = c("zone", "year", "week", "spp"),
                     rm_zone) {
  
  #df input note
  #should be the cleaned all species datasheet
  
  #check inputs
  #-------------------------------------------------------------------------------
  
  if(any(!grp_var %in% colnames(df))) {
    stop("one or more of the grouping variables (grp_var) do not exist in the data")
  }

  req_var = c("trap_id","year","week","zone", "zone2", "trap_status", 
              "method","spp","total")
  
  if(any(!req_var %in% colnames(df))) {
    stop(cat("one or more of the required variables are not in your data.",
             req_var))
  }
  
  # Create grouping variables
  #-------------------------------------------------------------------------------=
  grp_sym <- rlang::syms(grp_var)
  
  trap = df %>%
    dplyr::filter(method == "L") %>%  # only run on Light Traps
    dplyr::filter(!zone %in% rm_zone) %>%  # remove specified zones
    complete(trap_id, year, week, spp = spp_keep) %>% #complete with missing species
    #get totals by trap
    group_by(trap_id, year, week, zone, zone2, trap_status, spp) %>%
    summarise(total = sum(total), .groups = "drop") %>%
    ungroup() %>%
    #fill in missing trap data
    mutate(total = if_else(spp %in% spp_keep & is.na(total), 0, total)) %>%
    group_by(trap_id, year, week) %>%
    fill(zone, .direction = "downup") %>%
    fill(trap_status, .direction = "downup") %>%
    fill(zone2, .direction = "downup") %>%
    filter(spp %in% spp_keep)
  
  trap_sum = df %>%
    dplyr::filter(method == "L") %>%  # only run on Light Traps
    distinct(trap_id, zone, trap_status) %>%
    group_by(zone, trap_status) %>%
    count() %>%
    pivot_wider(names_from = trap_status, 
                values_from = n, 
                values_fill = 0) %>%
    ungroup %>%
    mutate(L_trap = rowSums(select(., where(is.numeric))))


   #get totals by variables you want to group by
  grp = trap %>%
    group_by(!!!grp_sym) %>%
    summarize(
      n_trap = n_distinct(trap_id),
      total = sum(total, na.rm = TRUE),
      .groups = 'drop'
            ) %>%
    mutate(abund = round(total/n_trap, 2))
  
  #rename to zone if zone2 exists
  if("zone2" %in% names(grp)) {
    grp =   grp %>%
      rename(zone = zone2)
  }
  
  output = list("trap" = trap,
                "status_sum" = trap_sum,
                "grp"  = grp)
  
  return(output)
}  