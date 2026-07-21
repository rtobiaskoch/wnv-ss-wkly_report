# test-week-pool-count-match.R
#
# The hard invariant of the pipeline: a pool and the trap-count it came from
# are the SAME collection, so they MUST be filed under the same week. PIR/VI
# join pools to counts on c(year, week, zone, spp); a 1-week disagreement
# silently splits a trap's pool from its own count.
#
# Both ingestion sites must therefore use the SINGLE shared week rule
# wnvSurv::calc_season_week():
#   * counts path -> wnvSurv::wnv_s_clean()        (ADD WEEK block)
#   * pools  path -> utils/fun_clean_4_weekly_input.R (Week = calc_season_week(`Trap Date`))
#
# These tests (a) run the counts path for real and prove its week equals the
# shared rule, (b) prove the shared rule is leap-week-stable and crosses the
# iso/epi boundary correctly, and (c) guard the source of BOTH files so a future
# edit cannot silently revert one path to isoweek/epiweek and re-open the split.

# wnv_s_clean() comes from wnvSurv (loaded in helper-setup.R); the utils copy
# was archived to utils/archive/.

# Dates chosen to expose the two ways the rules used to diverge:
#   2023-07-30 (Sun): isoweek 30 vs season 31  -> old counts rule (isoweek) differs
#   2026-06-03 (Wed): epiweek 22 vs season 23  -> 2026 leap-week; old pools rule (epiweek) differs
boundary_dates <- as.Date(c(
  "2023-07-30", # Sunday on the iso/epi boundary
  "2023-08-02", # Wednesday, typical set day, mid-season
  "2025-06-04", # first Wednesday of June 2025  -> week 23
  "2026-06-01", # Monday anchor of the 2026 leap-week season -> week 23
  "2026-06-03", # first Wednesday of June 2026 -> week 23 (epiweek would say 22)
  "2024-12-31"  # year-end edge
))

# Expected seasonal weeks (week 23 = first full week of June, leap-week-stable)
expected_week <- c(31, 31, 23, 23, 23, 1)


test_that("calc_season_week gives the expected seasonal weeks at the boundary + leap week", {
  expect_equal(wnvSurv::calc_season_week(boundary_dates), expected_week)
})


test_that("counts path (wnv_s_clean) assigns the shared seasonal week", {
  # Minimal culex-count frame: one distinct trap per date so trap_status
  # grouping does not collapse rows. silence = TRUE skips clean_summary (cli).
  df <- tibble::tibble(
    trap_id   = paste0("FC-", seq_along(boundary_dates)),
    trap_date = boundary_dates,
    spp       = "Tarsalis",
    total     = 1,
    zone      = "NE"
  )

  out <- wnv_s_clean(df, silence = TRUE)

  # wnv_s_clean sorts by desc(trap_date); compare on the date, not row order.
  counts_week <- out$week[match(boundary_dates, out$trap_date)]

  # The counts path equals the shared rule...
  expect_equal(counts_week, wnvSurv::calc_season_week(boundary_dates))
  # ...and therefore the expected seasonal weeks.
  expect_equal(counts_week, expected_week)
})


test_that("pools and counts paths assign IDENTICAL weeks for the same trap dates", {
  df <- tibble::tibble(
    trap_id   = paste0("FC-", seq_along(boundary_dates)),
    trap_date = boundary_dates,
    spp       = "Tarsalis",
    total     = 1,
    zone      = "NE"
  )

  out <- wnv_s_clean(df, silence = TRUE)
  counts_week <- out$week[match(boundary_dates, out$trap_date)]

  # The pools path label is exactly `Week = wnvSurv::calc_season_week(`Trap Date`)`
  # (utils/fun_clean_4_weekly_input.R) applied to the same collection dates.
  pools_week <- wnvSurv::calc_season_week(boundary_dates)

  expect_equal(counts_week, pools_week)
})


test_that("the new rule actually differs from the OLD rules (regression direction)", {
  # 2023-07-30: old counts rule isoweek() = 30, shared rule = 31.
  expect_false(identical(
    wnvSurv::calc_season_week(as.Date("2023-07-30")),
    as.numeric(lubridate::isoweek(as.Date("2023-07-30")))
  ))
  # 2026-06-03: old pools rule epiweek() = 22, shared rule = 23 (leap-week fix).
  expect_false(identical(
    wnvSurv::calc_season_week(as.Date("2026-06-03")),
    as.numeric(lubridate::epiweek(as.Date("2026-06-03")))
  ))
})


test_that("both ingestion sources use the shared rule and neither reverts to iso/epiweek for week", {
  # Source-level guard for the pools path, which is not runnable in isolation
  # (reads an RDS, needs a join + globals). If a future edit swaps the week rule
  # in either file, these assertions fail and point at the regression.
  # Counts path now lives in wnvSurv. Guard the INSTALLED function body rather
  # than a source file: that is what actually runs, and it also catches a stale
  # install where the package source is correct but the built copy is not.
  counts_src <- deparse(body(wnvSurv::wnv_s_clean))
  pools_src  <- readLines(here::here("utils/fun_clean_4_weekly_input.R"), warn = FALSE)

  # Counts path: ADD WEEK must use calc_season_week, not isoweek.
  add_week <- grep("week\\s*=\\s*", counts_src, value = TRUE)
  expect_true(any(grepl("calc_season_week", add_week)))
  expect_false(any(grepl("week\\s*=\\s*lubridate::isoweek", counts_src)))

  # Pools path: Week label must use calc_season_week on `Trap Date`, not epiweek.
  expect_true(any(grepl("Week\\s*=\\s*wnvSurv::calc_season_week\\(`Trap Date`\\)", pools_src)))
  expect_false(any(grepl("Week\\s*=\\s*epiweek\\(`Trap Date`\\)", pools_src)))
})
