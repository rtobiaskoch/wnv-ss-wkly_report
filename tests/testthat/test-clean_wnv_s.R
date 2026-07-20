# test-clean_wnv_s.R
# Tests for the force_recompute branch of wnv_s_clean().
#
# Intent:
#   - Raw provider input (counts/datasheet) carries a human-typed week/year that
#     is often wrong (BC types a running submission count; VDCI forgets to update
#     the year). force_recompute = TRUE re-derives BOTH from trap_date using the
#     single week authority wnvSurv::calc_season_week() and lubridate::isoyear().
#   - The default (force_recompute = FALSE) must PRESERVE an existing week/year,
#     which is what protects stored/historical data from being recomputed on
#     read-back (the frozen mid-season baseline).
#   - When week/year are absent, both flag values behave identically (back-compat).

# Two first-full-week-of-June dates -> seasonal week 23 in both 2025 and 2026,
# but with deliberately WRONG provider week/year typed in.
raw_bad <- tibble::tibble(
  trap_id   = c("fc-001", "fc-002"),
  trap_date = as.Date(c("2026-06-01", "2025-06-02")),
  week      = c(1, 99),       # bogus submitter week
  year      = c(1999, 1999)   # stale submitter year
)

test_that("force_recompute = TRUE overrides wrong week/year from trap_date", {
  out <- wnv_s_clean(raw_bad, silence = TRUE, force_recompute = TRUE)

  # week/year are now internally consistent with trap_date
  expect_equal(out$week, wnvSurv::calc_season_week(out$trap_date))
  expect_equal(out$year, as.double(lubridate::isoyear(out$trap_date)))

  # both first-June dates collapse to seasonal week 23; years corrected
  expect_true(all(out$week == 23))
  expect_setequal(out$year, c(2025, 2026))
})

test_that("default (force_recompute = FALSE) preserves provider week/year", {
  out <- wnv_s_clean(raw_bad, silence = TRUE)

  # match rows by trap_id since the function arranges by desc(trap_date)
  chk <- dplyr::arrange(out, trap_id)
  expect_equal(chk$week, c(1, 99))         # bogus values left untouched
  expect_equal(chk$year, c(1999, 1999))    # stale year left untouched
})

test_that("absent week/year are derived identically under both flag values", {
  raw_none <- tibble::tibble(
    trap_id   = c("fc-001", "fc-002"),
    trap_date = as.Date(c("2026-06-01", "2025-06-02"))
  )

  out_default <- wnv_s_clean(raw_none, silence = TRUE)
  out_force   <- wnv_s_clean(raw_none, silence = TRUE, force_recompute = TRUE)

  expect_equal(out_default$week, wnvSurv::calc_season_week(out_default$trap_date))
  expect_equal(out_default$year, as.double(lubridate::isoyear(out_default$trap_date)))

  # with the columns absent, the force_recompute flag makes no difference
  expect_equal(out_default, out_force)
})

# Regression: week 29 2026, CSU/Larimer County started prefixing Fort Collins
# subzone codes with "FC " (e.g. "FC NE" instead of "NE"). The zone-cleaning
# regex used str_extract() against a pattern that included "FC" itself, and
# str_extract() takes the leftmost match - "FC" starts at position 0 in
# "FC NE", so every FC subzone collapsed to "FC" and calc_abund() produced
# NA for NW/NE/SE/SW that week (see utils/fun_clean_wnv_s.R zone_raw_lvls).
test_that("FC-prefixed subzone codes extract to the subzone, not 'FC'", {
  raw_fc_prefixed <- tibble::tibble(
    trap_id   = c("fc-001", "fc-002", "fc-003", "fc-004"),
    trap_date = as.Date("2026-07-13"),
    zone      = c("FC NE", "FC NW", "FC SE", "FC SW")
  )

  out <- wnv_s_clean(raw_fc_prefixed, silence = TRUE)

  expect_setequal(as.character(out$zone), c("NE", "NW", "SE", "SW"))
})

# Regression: calc_all() (utils/fun_calc_all.R) bind_rows()s a zone2-derived
# "FC" rollup row together with the per-subzone rows, then re-runs
# wnv_s_clean() on the combined data to standardize columns. At that point
# "zone" already holds the literal, correct value "FC" - it is not raw
# provider text - so the raw-extraction default (which excludes "FC" to fix
# the bug above) would incorrectly wipe it back to NA. zone_raw_lvls lets
# that internal re-clean step opt back in to "FC" as a valid literal.
test_that("zone_raw_lvls = zone_lvls preserves an already-derived literal 'FC'", {
  already_clean <- tibble::tibble(zone = c("NW", "NE", "FC", "LV"))
  zone_lvls <- c("NW", "NE", "SE", "SW", "FC", "LV", "BE", "BC")

  out_default  <- wnv_s_clean(already_clean, all_cols = "zone", silence = TRUE)
  out_override <- wnv_s_clean(already_clean, all_cols = "zone", zone_raw_lvls = zone_lvls, silence = TRUE)

  # without the override, the literal "FC" gets wiped to NA (the bug)
  expect_equal(as.character(out_default$zone), c("NW", "NE", NA, "LV"))
  # with the override (what calc_all() now passes), "FC" survives
  expect_equal(as.character(out_override$zone), c("NW", "NE", "FC", "LV"))
})
