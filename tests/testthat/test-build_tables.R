test_that("build_tables returns named list of t1a/t2a/t3a with expected columns", {
  current_wk <- tibble::tibble(
    zone = factor(rep(c("NW", "NE"), each = 3), levels = c("NW", "NE")),
    spp  = factor(rep(c("Pipiens", "Tarsalis", "All"), 2),
                  levels = c("Pipiens", "Tarsalis", "All")),
    abund = c(1, 2, 3, 4, 5, 9),
    pir   = c(0.1, 0.2, 0, 0.3, 0.4, 0),
    vi    = c(0.1, 0.4, 0.5, 1.2, 2.0, 3.2),
    mosq_L = c(10, 20, 30, 40, 50, 90),
    trap_L = c(2, 2, 2, 3, 3, 3)
  )
  pools <- tibble::tibble(
    zone = factor(rep(c("NW", "NE"), each = 2), levels = c("NW", "NE")),
    spp  = factor(rep(c("Pipiens", "Tarsalis"), 2),
                  levels = c("Pipiens", "Tarsalis")),
    n_pools     = c(3, 4, 5, 6),
    n_pos_pools = c(1, 0, 2, 1)
  )

  out <- build_tables(current_wk, pools)

  expect_named(out, c("t1a", "t2a", "t3a"))
  expect_true("vi_All" %in% names(out$t1a))
  expect_false("abund_All" %in% names(out$t1a))
  expect_true(all(c("examined_Pipiens", "pool_all", "pir_All") %in% names(out$t3a)))
})
