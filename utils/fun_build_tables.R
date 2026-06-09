# utils/fun_build_tables.R
#
# DESCRIPTION
# Build the three "A" report tables (current-week summaries) for the weekly
# WNV report. Extracted from 0_R/tables.R.
#
# Pure function: takes the cleaned current-week stats and pool counts,
# returns a named list of data frames. No file I/O, no globals.
#
# INPUTS
#   current_wk  one row per zone x spp for the current week, with columns:
#               zone, spp, abund, pir, vi, mosq_L, trap_L
#               (produced by calc_all() in the main pipeline)
#   pools       one row per zone x spp with columns:
#               zone, spp, n_pools, n_pos_pools
#               (from pool count data, scoped to the current week)
#
# OUTPUT
#   Named list with three elements:
#     t1a  abundance + PIR + VI by zone (wide format)
#     t2a  collected mosquitoes, traps, and abundance by zone
#     t3a  examined, pools, positive pools, and PIR (per 1000) by zone
#
# DEPENDENCIES
#   insert_blank_row() from utils/fun_insert_blank_row.R
#   tidyverse (dplyr, tidyr) available via pipeline / helper-setup.R
#   janitor::get_dupes() for trap consistency check

build_tables <- function(current_wk, pools) {

  # ---------------------------------------------------------------------------
  # Inner helper: pivot one estimate column wide (zone x spp)
  # col    — unquoted column name to pivot (tidy-eval)
  # prefix — string prefix for the new column names (e.g. "abund_")
  # ---------------------------------------------------------------------------
  pivot_est <- function(df, col, prefix) {
    df %>%
      dplyr::select(zone, spp, {{ col }}) %>%
      tidyr::pivot_wider(
        names_from  = spp,
        values_from = {{ col }},
        names_prefix = prefix
      )
  }

  # ===========================================================================
  # TABLE 1A: abundance + PIR + VI by zone
  # Drop abund_All and pir_All (not reported in this table per original script)
  # ===========================================================================

  # Wide pivot of each metric, one column per species
  t1a_abund <- pivot_est(current_wk, abund, "abund_")
  t1a_pir   <- pivot_est(current_wk, pir,   "pir_")
  t1a_vi    <- pivot_est(current_wk, vi,    "vi_")

  t1a <- t1a_abund %>%
    dplyr::left_join(t1a_pir, by = "zone") %>%
    dplyr::left_join(t1a_vi,  by = "zone") %>%
    dplyr::select(-abund_All, -pir_All) %>%  # excluded from this table
    insert_blank_row(5)                       # blank row after 5th data row (zone separator)

  # ===========================================================================
  # TABLE 2A: collected mosquitoes, trap count, and abundance by zone
  # ===========================================================================

  # Mosquitoes collected per species (wide)
  t2a_collected <- current_wk %>%
    dplyr::select(zone, spp, mosq_L) %>%
    tidyr::pivot_wider(
      names_from   = "spp",
      values_from  = "mosq_L",
      names_prefix = "collected_",
      values_fill  = 0
    )

  # Trap count per zone — should be one row per zone (same for pip and tar)
  t2a_traps <- current_wk %>%
    dplyr::distinct(zone, trap_L)

  # Warn if trap counts differ between species within a zone
  if (nrow(janitor::get_dupes(t2a_traps)) > 0) {
    message("the number of traps for pipiens and tarsalis don't match")
  }

  t2a <- t2a_collected %>%
    dplyr::left_join(t2a_traps,  by = "zone") %>%
    dplyr::left_join(t1a_abund,  by = "zone") %>%  # reuse already-pivoted abund
    dplyr::mutate(dplyr::across(.cols = -zone, ~ round(.x, 2))) %>%
    insert_blank_row(5)

  # ===========================================================================
  # TABLE 3A: examined, pools examined, positive pools, PIR (per 1000) by zone
  # ===========================================================================

  # Mosquitoes examined (light-trap counts), wide by species
  t3a_examined <- current_wk %>%
    dplyr::select(zone, spp, mosq_L) %>%
    tidyr::pivot_wider(
      names_from   = "spp",
      values_from  = "mosq_L",
      names_prefix = "examined_",
      values_fill  = 0
    )

  # Number of pools examined per species
  t3a_pools <- pools %>%
    dplyr::select(zone, spp, n_pools) %>%
    tidyr::pivot_wider(
      names_from   = "spp",
      values_from  = "n_pools",
      names_prefix = "pool_",
      values_fill  = 0
    )

  # Number of positive pools per species
  t3a_p_pools <- pools %>%
    dplyr::select(zone, spp, n_pos_pools) %>%
    tidyr::pivot_wider(
      names_from   = "spp",
      values_from  = "n_pos_pools",
      names_prefix = "pos_pool_",
      values_fill  = 0
    )

  # PIR scaled to per-1000 mosquitoes for display
  t3a_pir <- t1a_pir %>%
    dplyr::mutate(dplyr::across(-zone, ~ round(. * 1000, 2)))

  # Final column order matches report template
  t3a_cols <- c(
    "zone",
    "examined_Pipiens", "examined_Tarsalis", "examined_All",
    "pool_Pipiens",     "pool_Tarsalis",     "pool_all",
    "pos_pool_Pipiens", "pos_pool_Tarsalis", "pos_pool_all",
    "pir_Pipiens",      "pir_Tarsalis",      "pir_All"
  )

  t3a <- t3a_examined %>%
    dplyr::left_join(t3a_pools,   by = "zone") %>%
    dplyr::left_join(t3a_p_pools, by = "zone") %>%
    dplyr::left_join(t3a_pir,     by = "zone") %>%
    dplyr::mutate(
      pool_all     = pool_Pipiens + pool_Tarsalis,       # aggregate across species
      pos_pool_all = pos_pool_Pipiens + pos_pool_Tarsalis
    ) %>%
    insert_blank_row(5) %>%
    dplyr::select(dplyr::all_of(t3a_cols))

  list(t1a = t1a, t2a = t2a, t3a = t3a)
}
