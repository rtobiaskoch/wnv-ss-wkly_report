# Tests for build_tables() (utils/fun_build_tables.R)
# Verifies the three "A" report tables are shaped correctly: column presence,
# the abund_All/pir_All drops, t3a column ordering, and blank-row insertion.

# Fixture builder: n_zones zones, each with Pipiens/Tarsalis/All rows for
# current_wk and Pipiens/Tarsalis rows for pools.
make_inputs <- function(n_zones = 2) {
  zones <- c("NW", "NE", "SE", "SW", "FC", "LV", "BE", "BC")[seq_len(n_zones)]

  current_wk <- tibble::tibble(
    zone = factor(rep(zones, each = 3), levels = zones),
    spp  = factor(rep(c("Pipiens", "Tarsalis", "All"), n_zones),
                  levels = c("Pipiens", "Tarsalis", "All")),
    abund  = seq_len(n_zones * 3),
    pir    = seq_len(n_zones * 3) / 10,
    vi     = seq_len(n_zones * 3) / 5,
    mosq_L = seq_len(n_zones * 3) * 10,
    trap_L = rep(seq_len(n_zones) + 1, each = 3)
  )
  pools <- tibble::tibble(
    zone = factor(rep(zones, each = 2), levels = zones),
    spp  = factor(rep(c("Pipiens", "Tarsalis"), n_zones),
                  levels = c("Pipiens", "Tarsalis")),
    n_pools     = seq_len(n_zones * 2),
    n_pos_pools = seq_len(n_zones * 2) %% 2
  )
  list(current_wk = current_wk, pools = pools)
}

test_that("build_tables returns a named list of the three A tables", {
  io  <- make_inputs(2)
  out <- build_tables(io$current_wk, io$pools)
  expect_named(out, c("t1a", "t2a", "t3a"))
})

test_that("t1a keeps vi_All but drops abund_All and pir_All", {
  io  <- build_tables(make_inputs(2)$current_wk, make_inputs(2)$pools)
  expect_true("vi_All" %in% names(io$t1a))
  expect_false("abund_All" %in% names(io$t1a))
  expect_false("pir_All"   %in% names(io$t1a))
})

test_that("t3a has the full ordered column set", {
  io <- build_tables(make_inputs(2)$current_wk, make_inputs(2)$pools)
  expect_identical(
    names(io$t3a),
    c("zone",
      "examined_Pipiens", "examined_Tarsalis", "examined_All",
      "pool_Pipiens",     "pool_Tarsalis",     "pool_all",
      "pos_pool_Pipiens", "pos_pool_Tarsalis", "pos_pool_all",
      "pir_Pipiens",      "pir_Tarsalis",      "pir_All")
  )
})

test_that("insert_blank_row appends a blank row (2-zone append branch)", {
  out <- build_tables(make_inputs(2)$current_wk, make_inputs(2)$pools)
  # 2 data rows + 1 blank row appended (after_row 5 >= nrow -> append)
  expect_equal(nrow(out$t1a), 3)
  expect_equal(nrow(out$t2a), 3)
  expect_equal(nrow(out$t3a), 3)
})

test_that("insert_blank_row inserts mid-table after the 5th data row (6-zone branch)", {
  out <- build_tables(make_inputs(6)$current_wk, make_inputs(6)$pools)
  # 6 data rows + 1 blank inserted after row 5 -> 7 rows; row 6 is the blank
  expect_equal(nrow(out$t1a), 7)
  expect_true(all(is.na(out$t1a[6, -1])))
})
