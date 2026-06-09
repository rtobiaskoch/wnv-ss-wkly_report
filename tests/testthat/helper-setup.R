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

# Fixture directory — populated in Phase 2 for CSV-based tests
fixture_dir <- here::here("tests/fixtures")
