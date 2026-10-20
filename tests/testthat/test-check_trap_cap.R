trap <- tibble::tibble(trap_id = c("A1", "A2", "B1"), zone = c("BE", "BE", "LV"))
mk <- function(ids, zones, status = "culex") {
  tibble::tibble(trap_id = ids, zone = zones, year = 2026, week = 37, trap_status = status)
}

test_that("passes when counts are within the trap list", {
  expect_no_error(check_trap_cap(mk(c("A1", "B1"), c("BE", "LV")), trap, 2026, 37))
})

test_that("stops on blank trap_id", {
  expect_error(check_trap_cap(mk(c("A1", NA), c("BE", "BE")), trap, 2026, 37), "blank trap_id")
  expect_error(check_trap_cap(mk(c("A1", ""), c("BE", "BE")), trap, 2026, 37), "blank trap_id")
})

test_that("stops when a zone exceeds the trap list, ignoring no-trap stubs", {
  expect_error(check_trap_cap(mk(c("A1", "A2", "A3"), rep("BE", 3)), trap, 2026, 37), "BE wk37: 3 > 2")
  expect_no_error(check_trap_cap(mk(c("A1", "A2", "A3"), rep("BE", 3), "no trap"), trap, 2026, 37))
})
