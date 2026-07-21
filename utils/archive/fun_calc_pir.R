#uses the datasheet (pooled data) with a test_code output from the PCR 

calc_pir <- function(df, 
                    grp_var = c("zone", "year", "week", "spp"), #variables to calculate the pools by
                    zone_complete = c("NW", "NE", "SE","SW", "LV", "BE", "BC"), # list of zones to complete the 
                    rm_zone = NULL)  {
  
  #check packages
  #-------------------------------------------------------------------------------
  if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
    stop("PooledInfRate is required. Install with: devtools::install_github('CDCgov/PooledInfRate')")
  }


  #check inputs
  #-------------------------------------------------------------------------------
  if ("csu_id" %in% grp_var) {
    stop("Cannot calculate abundance using csu_id as a grouping variable. 
         CSU IDs represent individual mosquito pools, not trap-level data.")
  }
  
  if(any(!grp_var %in% colnames(df))) {
    stop("one or more of the grouping variables (grp_var) do not exist in the data")
  }
  
  
req_var = c("trap_id","year","week","zone", "zone2",
              "method","spp","total")

if(any(!req_var %in% colnames(df))) {
  
  missing = setdiff(req_var, names(df))
  stop(cat("The required variables are not in your data.",
           paste0(missing, collapse = ",")))
}
  
  
  # Create base grouping variables
  #-------------------------------------------------------------------------------

  grp_var_sym <- syms(grp_var)
  
  #don't fill zones that you don't want in your analysis
  zone_complete = setdiff(zone_complete, rm_zone)
  
  # Calc PIR from the pool data
  #-------------------------------------------------------------------------------
  # Keep every pool that was actually PCR-tested, and only those.
  #
  # Both conditions are load-bearing, for DIFFERENT reasons (verified against
  # PooledInfRate 1.6 in sandbox/test_pIR_single_pool.R):
  #   total > 0        -- an imputed row is a species that was not caught, so no
  #                       pool exists. Passing total == 0 with a non-NA
  #                       test_code makes pIR() fail with "missing value where
  #                       TRUE/FALSE needed". This is the real error the
  #                       original filter was written to prevent.
  #   !is.na(test_code) -- an untested row carries no information; pIR() drops
  #                       NAs internally and can end up with nothing.
  #
  # This was previously filter(total > 1). The `> 1` also removed real pools of
  # a single mosquito -- including positives (CSU23480, NW 2025 wk26 Pipiens),
  # silently forcing pir = 0 and vi = 0. pIR() handles size-1 pools correctly at
  # both zone and trap grouping; only total == 0 and empty input break it.
  df_pir = df %>%
    dplyr::filter(!zone %in% rm_zone) %>%
    dplyr::filter(!is.na(test_code), total > 0) %>%
    tidyr::unite(col = "grp", all_of(grp_var), sep = "_", remove = FALSE)

  if (nrow(df_pir) == 0) {
    stop("No PCR-tested pools remain after filtering; cannot estimate PIR.")
  }

  mle = PooledInfRate::pIR(test_code ~ total|grp, data = df_pir, pt.method = "firth")
  
  
  df_pir = as.data.frame(mle) %>%
    separate(grp,
             into = {{grp_var}},
             sep = "_") %>%
    mutate(year = as.numeric(year),
           week = as.numeric(week)) %>%
    mutate(pir = round(P,4),
           pir_lci = round(Lower,4),
           pir_uci = round(Upper,4)
          ) %>%
   # mutate(across(all_of(grp_var), as.character)) %>% #ensure left_join will work
    select(-P, -Upper, -Lower)
  
  
  # The single-mosquito-pool branch that used to sit here is gone. It existed
  # only to patch groups that filter(total > 1) had emptied, and it assigned a
  # degenerate pir = pir_lci = pir_uci = 0 or 1 with no real confidence bounds.
  # Those groups are now estimated by pIR() directly; keeping the branch would
  # duplicate every one of them in the output.


  #complete any combinations of traps and pools that aren't represented in the pooled data
  df_pir = df_pir %>%
    complete(!!!grp_var_sym)
  
  
  df_pir[is.na(df_pir)] = 0# Fill missing values with 0
  
  #rename to zone if zone2 exists
  if("zone2" %in% names(df_pir)) {
    df_pir =   df_pir %>%
      rename(zone = zone2)
  }
  
  
  #add spp if it wasn't in df it means its all spp
  if(!"spp" %in% names(df_pir)) {
    df_pir = df_pir %>%
      mutate(spp = "All")
  }
  
  return(df_pir)
  #END OF PIR

} #end of function


