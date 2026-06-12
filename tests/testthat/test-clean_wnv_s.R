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
