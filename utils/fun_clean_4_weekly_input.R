clean_4_weekly_input = function(rds, update, dir) {
  data_input0 = read_rds(rds)

  # Derive the seasonal week from Trap Date — the SINGLE week authority shared
  # with the counts path (wnvSurv::calc_season_week). Filtering on the derived
  # week (not the submitter-typed Week) guarantees the week used to SELECT the
  # current pool batch is the same week the pool is later LABELLED with (line ~55),
  # so a pool always lands in the same week as the trap-count it came from.
  # Both week AND year are derived from Trap Date (not the submitter-typed Week/Year)
  # so a stale provider year (e.g. VDCI forgets to update it) does not silently
  # route correct samples into non_week_samples.csv. isoyear() is the same year
  # rule wnv_s_clean() applies on the counts path.
  data_input = data_input0 %>%
    filter(
      wnvSurv::calc_season_week(`Trap Date`) == week_filter &
        lubridate::isoyear(`Trap Date`) == year_filter
    ) #incase samples were added from a previous week/year they still get added  to database

  filtered_samples = data_input0 %>%
    filter(
      wnvSurv::calc_season_week(`Trap Date`) != week_filter |
        lubridate::isoyear(`Trap Date`) != year_filter
    )

  # QC: flag pools in the current batch whose submitter-typed Week disagrees with
  # the Trap Date-derived week by more than 1 (usually a bad Trap Date or a
  # mis-keyed Week). Reported, not corrected — the derived week is authoritative.
  if ("Week" %in% names(data_input) && nrow(data_input) > 0) {
    n_wk_mismatch <- sum(
      abs(
        as.integer(data_input$Week) -
          wnvSurv::calc_season_week(data_input$`Trap Date`)
      ) >
        1,
      na.rm = TRUE
    )
    if (n_wk_mismatch > 0) {
      cli::cli_alert_warning(
        "{n_wk_mismatch} pool{?s}: submitter Week differs from Trap Date-derived week by >1 (check Trap Date)"
      )
    }
  }

  if (nrow(filtered_samples) > 0) {
    write.csv(filtered_samples, file.path(dir, "non_week_samples.csv"))
    print(paste0(
      filtered_samples$`CSU Pool Number (CMC Enters)`,
      " sample was removed and not part of week ",
      week_filter,
      " sample pool"
    ))
  }

  order = colnames(data_input)

  test_results = update %>%
    #add back in the "-" if it isn't there already
    mutate(
      csu_id = if_else(
        str_detect(csu_id, "^(CSU|BOU)[0-9]+$"), # Detect CSU or BOU with digits and no hyphen
        str_replace(csu_id, "^(CSU|BOU)([0-9]+)$", "\\1-\\2"), # Add hyphen
        csu_id
      )
    ) %>%
    select(csu_id, test_code) %>% # keep only the test_code
    rename(
      `CSU Pool Number (CMC Enters)` = "csu_id",
      `Test Code (CSU Enters)` = "test_code"
    )

  weekly_data_format = data_input %>%
    select(
      -`Test Code (CSU Enters)`, #drop because going to be added from the data_base_update data
      -Comments,
      `Test Result (CSU Enters)`
    ) %>% #drop because it messes up the order for the export
    left_join(test_results, by = "CSU Pool Number (CMC Enters)") %>%
    mutate(
      `Test Result (CSU Enters)` = if_else(
        `Test Code (CSU Enters)` == 1,
        "Positive",
        "Negative"
      )
    ) %>%
    mutate(
      `Collection Site       (Trap ID)` = str_to_upper(
        `Collection Site       (Trap ID)`
      )
    ) %>%
    #clean up variations in the species
    mutate(
      Species = case_when(
        str_detect(Species, regex("^tar", ignore_case = T)) ~ 'Tarsalis',
        str_detect(Species, regex("^pip", ignore_case = T)) ~ 'Pipiens'
      )
    ) %>%
    #clean up variations in the method of gravid light trap
    mutate(`L/G` = str_trim(`L/G`)) %>%
    mutate(
      `L/G` = case_when(
        str_detect(`L/G`, regex("^g", ignore_case = T)) ~ 'G',
        str_detect(`L/G`, regex("^l", ignore_case = T)) ~ 'L'
      )
    ) %>%
    mutate(Sex = as.character(Sex)) %>%
    mutate(Week = wnvSurv::calc_season_week(`Trap Date`), #seasonal week from date; SAME rule as counts path
           Year = lubridate::isoyear(`Trap Date`)) %>% #year from date too; ignore stale submitter-typed Year
    rename(Spp = "Species", Method = "L/G", Account = "Acct")

  if (any(is.na(weekly_data_format$`Test Code (CSU Enters)`))) {
    stop("Your data didn't merge properly with test results check csu_id")
  }

  return(weekly_data_format)
}
