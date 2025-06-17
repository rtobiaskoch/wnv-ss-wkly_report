calc_abund = function(df, 
                     spp_keep = c("Pipiens", "Tarsalis"), 
                     grp_var = c("zone", "year", "week", "spp"),
                     zones = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC"),
                     rm_zone = NULL) {
  
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
    dplyr::filter(trap_status != "malfunction") %>%
    dplyr::filter(!zone %in% rm_zone) %>%  # remove specified zones
    complete(trap_id, year, week, spp = spp_keep) %>% #complete with missing species
    complete(year, week, spp, zone = zones) %>% # fill in any missing zone for the report
    #get totals by trap
    group_by(trap_id, year, week, zone, zone2, trap_status, spp) %>%
    summarise(mosq_L = sum(total), .groups = "drop") %>%
    ungroup() %>%
    #fill in missing trap data
    mutate(mosq_L = if_else(spp %in% spp_keep & is.na(mosq_L), 0, mosq_L)) %>%
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
    mutate(trap_L = rowSums(select(., where(is.numeric))))


   #get totals by variables you want to group by
  grp = trap %>%
    group_by(!!!grp_sym) %>%
    summarize(
      trap_L = if (all(is.na(trap_id))) 0 else n_distinct(trap_id, na.rm = TRUE), #if all na make 0 from filling in missing zones else count distinct
      mosq_L = sum(mosq_L, na.rm = TRUE),
      .groups = 'drop'
            ) %>%
    mutate(abund = round(mosq_L/trap_L, 2))
  
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