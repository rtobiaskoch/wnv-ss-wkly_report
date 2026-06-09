# tests/testthat/helper-setup.R
#
# Auto-sourced by testthat before any test-*.R file.
# Loads packages and sources only the calc utility functions.
# gdrive/gsheet utilities are intentionally excluded — they require OAuth.

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(rlang)
  library(tibble)
  library(here)
  library(janitor)
  library(ggplot2)
  library(PooledInfRate)
  library(wnvSurv)   # SSOT for calc_season_week / add_week_cols (was utils/fun_calc_week.R)
})

# Source calc utilities in dependency order
source(here::here("utils/fun_calc_abund.R"))
source(here::here("utils/fun_calc_pir.R"))
source(here::here("utils/fun_calc_vi.R"))

# Source table-building utilities
source(here::here("utils/fun_insert_blank_row.R"))
source(here::here("utils/fun_build_tables.R"))

# Source bird report utilities
source(here::here("utils/fun_bird_report.R"))

# Source plot history utilities (fun_plot_hx.R uses zone_lvls and pal_mozzy
# from the config globals, which are not sourced in the test harness — they
# are assigned into globalenv below so sourced functions can resolve them)
source(here::here("utils/fun_plot_hx.R"))

# Config constants needed by plot/clean utils (mirror config/config_weekly.R;
# config/ is not sourced in the test harness).
# Must use assign(..., envir = globalenv()) because helper top-level code may
# run in the test env, not globalenv, and the functions look up these names in
# globalenv at call time.
assign("zone_lvls", c("NW", "NE", "SE", "SW", "FC", "LV", "BE", "BC"),
       envir = globalenv())
assign("pal_mozzy", c(
  "hx_Tarsalis"      = "grey50",
  "hx_Pipiens"       = "grey30",
  "current_Tarsalis" = "#e9724c",
  "current_Pipiens"  = "#820263"
), envir = globalenv())

# Fixture directory — populated in Phase 2 for CSV-based tests
fixture_dir <- here::here("tests/fixtures")
