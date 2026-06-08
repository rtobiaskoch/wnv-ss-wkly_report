# Tests for merge_trap_database() — the key-join merge that combines this week's
# trap data into the existing database.
#
# These lock the invariant that a prior bug violated: joining on `key`
# (trap_id|spp|year|week, NOT trap_date) must land each real row on its
# pre-seeded "no trap" stub, NOT duplicate it. The old join keyed on trap_date,
# which is NA in stubs, so real rows never matched -> 148 duplicate w23 rows.
#
# Pure function, no OAuth / network. rquery is called namespaced in the function.

source(here::here("utils", "fun_merge_trap_database.R"))

# Minimal database schema used throughout. `old` mimics the combiner output:
# keyed rows, current-week entries pre-seeded as "no trap" stubs.
make_row <- function(key, trap_id, spp, year, week, trap_status, total, trap_date) {
  tibble::tibble(key = key, trap_id = trap_id, spp = spp, year = year,
                 week = week, trap_status = trap_status, total = total,
                 trap_date = as.Date(trap_date))
}

db_cols <- c("key", "trap_id", "spp", "year", "week", "trap_status", "total", "trap_date")

test_that("a real row coalesces onto its keyed stub (no duplicate, new wins)", {
  old <- make_row("FC036|Tarsalis|2026|23", "FC-036", "Tarsalis", 2026, 23,
                  "no trap", NA_real_, NA)                       # pre-seeded stub
  new <- make_row("FC036|Tarsalis|2026|23", "FC-036", "Tarsalis", 2026, 23,
                  "culex", 5, "2026-06-01")                      # this week's real data

  out <- merge_trap_database(new, old, by = "key", col_database = db_cols)

  # exactly one row for the key — the stub was overwritten, not duplicated
  k <- out[out$key == "FC036|Tarsalis|2026|23", ]
  expect_equal(nrow(k), 1L)
  expect_equal(k$trap_status, "culex")          # new value wins over stub's "no trap"
  expect_equal(k$total, 5)
  expect_equal(k$trap_date, as.Date("2026-06-01"))
})

test_that("a genuine no-trap week stays a single no-trap row", {
  old <- make_row("FC050|Pipiens|2026|23", "FC-050", "Pipiens", 2026, 23,
                  "no trap", NA_real_, NA)
  new <- make_row("FC050|Pipiens|2026|23", "FC-050", "Pipiens", 2026, 23,
                  "no trap", NA_real_, NA)

  out <- merge_trap_database(new, old, by = "key", col_database = db_cols)

  expect_equal(nrow(out), 1L)
  expect_equal(out$trap_status, "no trap")
})

test_that("a brand-new key (not in database) is kept (FULL join)", {
  old <- make_row("FC050|Pipiens|2025|30", "FC-050", "Pipiens", 2025, 30,
                  "culex", 3, "2025-07-20")                       # history only
  new <- make_row("FC099|Tarsalis|2026|23", "FC-099", "Tarsalis", 2026, 23,
                  "culex", 7, "2026-06-02")                       # new trap this week

  out <- merge_trap_database(new, old, by = "key", col_database = db_cols)

  expect_true("FC099|Tarsalis|2026|23" %in% out$key)
  expect_equal(nrow(out), 2L)
})

test_that("a historical key (not in new) is kept unchanged", {
  old <- make_row("BC01|Pipiens|2010|25", "BC-01", "Pipiens", 2010, 25,
                  "culex", 12, "2010-06-22")
  new <- make_row("FC036|Tarsalis|2026|23", "FC-036", "Tarsalis", 2026, 23,
                  "culex", 5, "2026-06-01")

  out <- merge_trap_database(new, old, by = "key", col_database = db_cols)

  hist <- out[out$key == "BC01|Pipiens|2010|25", ]
  expect_equal(nrow(hist), 1L)
  expect_equal(hist$total, 12)
  expect_equal(hist$trap_status, "culex")
})

test_that("REGRESSION GUARD: output has no duplicate or NA keys", {
  # mixed week: 2 real rows landing on stubs, 1 genuine no-trap, 1 history-only
  old <- dplyr::bind_rows(
    make_row("FC036|Tarsalis|2026|23", "FC-036", "Tarsalis", 2026, 23, "no trap", NA_real_, NA),
    make_row("FC050|Pipiens|2026|23",  "FC-050", "Pipiens",  2026, 23, "no trap", NA_real_, NA),
    make_row("FC088GR|Pipiens|2026|23","FC-088GR","Pipiens", 2026, 23, "no trap", NA_real_, NA),
    make_row("BC01|Pipiens|2010|25",   "BC-01",  "Pipiens",  2010, 25, "culex",   12, "2010-06-22")
  )
  new <- dplyr::bind_rows(
    make_row("FC036|Tarsalis|2026|23", "FC-036", "Tarsalis", 2026, 23, "other spp", 0, "2026-06-01"),
    make_row("FC088GR|Pipiens|2026|23","FC-088GR","Pipiens", 2026, 23, "culex",     8, "2026-06-02"),
    make_row("FC050|Pipiens|2026|23",  "FC-050", "Pipiens",  2026, 23, "no trap", NA_real_, NA)
  )

  out <- merge_trap_database(new, old, by = "key", col_database = db_cols)

  expect_equal(nrow(out), dplyr::n_distinct(out$key))   # the invariant
  expect_false(any(is.na(out$key)))
  expect_equal(nrow(out), 4L)                            # 3 w23 + 1 history, no dups
})
