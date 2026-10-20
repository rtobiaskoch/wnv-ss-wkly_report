# DESCRIPTION
# Guard for the merged culex database: stops if (1) any row has a blank trap_id,
# or (2) a zone has more distinct run traps for the given year/week than the active
# trap list allows. "no trap" stubs are ignored (the combiner pre-seeds them for
# every trap). exempt_zones can exceed the list on purpose (BC "could be more").
#
# df:          merged culex database (needs trap_id, zone, year, week, trap_status)
# trap:        active trap list (trap_id, zone)
# Returns df invisibly so it can sit in a pipe.
check_trap_cap <- function(df,
                           trap,
                           year_filter,
                           week_filter,
                           exempt_zones = "BC") {

  df_name <- deparse(substitute(df))

  # (1) blank trap_id rows are zone-only placeholders, never real traps
  n_blank <- sum(is.na(df$trap_id) | df$trap_id == "")
  if (n_blank > 0) {
    stop(df_name, " has ", n_blank, " rows with a blank trap_id")
  }

  # (2) distinct traps actually run this week vs. traps allowed per zone
  allowed <- trap %>%
    dplyr::count(zone, name = "n_allowed") %>%
    dplyr::mutate(zone = as.character(zone))

  found <- df %>%
    dplyr::filter(year %in% year_filter, week %in% week_filter,
                  trap_status != "no trap") %>%
    dplyr::mutate(zone = as.character(zone)) %>%
    dplyr::group_by(year, week, zone) %>%
    dplyr::summarise(n_found = dplyr::n_distinct(trap_id), .groups = "drop")

  over <- found %>%
    dplyr::left_join(allowed, by = "zone") %>%
    dplyr::filter(!zone %in% exempt_zones, n_found > n_allowed)

  if (nrow(over) > 0) {
    stop(df_name, " has more traps than the trap list allows:\n",
         paste0(over$zone, " wk", over$week, ": ", over$n_found, " > ",
                over$n_allowed, collapse = "\n"))
  }

  cat("\ncheck_trap_cap: no blank trap_ids; zone trap counts within trap list\n")
  invisible(df)
}
