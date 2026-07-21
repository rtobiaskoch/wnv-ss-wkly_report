
calc_all = function(abund_input, pir_input) {
  
  
  #ABUNDANCE 
  #group by zone
  abund0 = calc_abund(abund_input, 
                      grp_var = c("zone", "year", "week", "spp"))
  
  #group FC
  abund_fc = calc_abund(abund_input, 
                        grp_var = c("zone2", "year", "week", "spp"))
  
  #group all spp
  abund_all = calc_abund(abund_input, 
                         grp_var = c("zone", "year", "week"))
  
  #group all FC
  abund_all_fc = calc_abund(abund_input, 
                            grp_var = c("zone2", "year", "week"))
  
  abund = bind_rows(abund0,
                    abund_all,
                    abund_fc,
                    abund_all_fc) %>%
    distinct_all() %>%
    # zone here is already-derived (zone2 renamed to zone for the FC rollup
    # rows), not raw provider text, so "FC" must stay a valid literal match -
    # override the raw-extraction default that excludes it.
    wnv_s_clean(all_cols = names(.), zone_raw_lvls = zone_lvls, silence = T) %>%
    arrange(year, week, zone, spp)
  
  #PIR 
  pir0 = calc_pir(pir_input,
                  grp_var = c("zone", "year", "week", "spp"))
  
  pir_fc = calc_pir(pir_input , 
                    grp_var = c("zone2", "year","spp", "week"))
  
  pir_all = calc_pir(pir_input, 
                     grp_var = c("zone", "year", "week"))
  
  pir_fc_all = calc_pir(pir_input , 
                        grp_var = c("zone2", "year", "week"))
  
  pir = bind_rows(pir0, pir_all,
                  pir_fc, pir_fc_all) %>%
    distinct_all() %>%
    # see abund comment above - zone is already-derived here, "FC" must stay valid
    wnv_s_clean(all_cols = names(.), zone_raw_lvls = zone_lvls, silence = T) %>%
    arrange(year, week, zone, spp)
  
  #VI
  # calc_vi() now comes from wnvSurv. Its defaults differ from the archived
  # utils copy (complete = FALSE, zone_complete = wnvSurv::zone_lvls), so both
  # are passed explicitly to keep zone_stats.csv byte-identical:
  #   complete = TRUE       -- the local version completed by default
  #   zone_complete         -- the local version read the `grp_zones` global
  #                            directly; unique() drops the duplicated "BC" in
  #                            config/config_weekly.R
  vi = calc_vi(abund, pir,
          by = grp_vars,
          complete = TRUE,
          zone_complete = unique(grp_zones)) %>%
   # see abund comment above - zone is already-derived here, "FC" must stay valid.
   # spp0 is in rm_col because this is an AGGREGATED stats table, not trap-level
   # data: wnv_s_clean() would otherwise snapshot the (already collapsed) spp
   # into a spurious spp0 column and append it to zone_stats.csv.
   wnv_s_clean(rm_col = c("trap_status", "spp0"), zone_raw_lvls = zone_lvls, silence = T)
  
  
  return(vi)
  
}
