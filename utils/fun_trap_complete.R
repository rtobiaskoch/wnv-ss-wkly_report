trap_complete = function(df, #data you want to complete
                         trap, #df with trap_id, 
                         spp_cmplt = c("Pipiens", "Tarsalis"), #spp within the traps that you want to complete
                         zone_cmplt = c("NE", "SE", "NW", "SW","LV", "BC", "BE"), #zones you want to 
                         grp_fill_var = c("trap_id", "year", "week")
                         ) { #variable to group and fill the new complete rows with
  
  
  library(tidyverse)
  
  #check inputs
  #-------------------------------------------------------------------------------
  
  if(any(!grp_fill_var %in% colnames(df))) {
    stop("one or more of the grouping variables (grp_var) do not exist in the data")
  }
  
  req_var = c("trap_id","year","week","trap_status", "zone", "spp","total")
  
  if(any(!req_var %in% colnames(df))) {
    
    missing = setdiff(req_var, names(df))
    stop(cat("The required variables are not in your data.",
             paste0(missing, collapse = ",")))
  } #end req var check
  
  #-------------------------------------------------------------------------------
  
  grp_fill_var = rlang::syms(grp_fill_var)
  df0 = df #for comparison of results
  
  df = df %>%
    
    #complete any missing traps and traps that have missing spp (tarsalis and pipiens)
    complete(trap_id = trap,  
             spp = spp_cmplt,
             year, 
             week) %>%
    
    #fill in missing zones that don't have any traps
    complete(zone = zone_cmplt, year, week) %>%
    
    #fill in associated data for the missing traps and zones
    group_by(!!!grp_fill_var) %>%
    fill(-total, .direction = "downup") %>%
    ungroup %>%
    
    #identify traps that are missing because they won't have any associated data for the week for that trap_id
    mutate(trap_status = replace_na(trap_status, "missing")) %>%
    
    #ensure malfunction or missing traps not included as n for trap totals
    mutate(total = case_when(str_detect(trap_status, "malfunction|missing")                  ~ NA,
                             
    #fill in 0 where traps were set but spp_cmplt were not found
                             str_detect(spp, paste(spp_cmplt, collapse = "|")) & is.na(total) ~ 0,
                             T ~ total )
    ) %>%
    #remove superfluous non culex spp
    filter(spp %in% spp_cmplt | is.na(spp)) %>%
    arrange(year, week, zone, trap_id, spp)

  # drop placeholder rows with no trap_id. complete(zone = ...) above creates a
  # zone-only row (trap_id NA) for any zone absent from the data that week; it is
  # not a real trap and inflates distinct trap counts downstream (e.g. BE = 11).
  # Real traps for those zones are re-added later by joining the trap list.
  is_blank_trap = is.na(df$trap_id) | df$trap_id == ""

  if(any(is_blank_trap)) {
    cat("\nMessage: dropped", sum(is_blank_trap),
        "placeholder rows with blank trap_id from zone(s):",
        paste0(unique(df$zone[is_blank_trap]), collapse = ","), "\n")
  }

  df = df[!is_blank_trap, ]

  # anything left that is not in the trap list is unexpected -> warn, keep the row
  unexpected = setdiff(unique(df$trap_id), trap)
  if(length(unexpected) > 0) {
    warning("trap_complete: trap_id(s) not in trap list: ",
            paste0(unexpected, collapse = ","))
  }


  #check missing traps
  if(any(df$trap_status == "missing")) {
    missing = df %>% filter(trap_status == "missing")
    missing = unique(missing$trap_id)
    
    cat("\nWarning the following traps are missing in your data: ", paste0(missing, collapse = ","), "\n")
  }
  
  cat("Added ", nrow(df)- nrow(df0), " rows to data using trap_complete")
  
  return(df)
} #end fun
