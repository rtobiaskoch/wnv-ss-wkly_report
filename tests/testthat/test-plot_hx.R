# Tests for clean_long_hx_wk() and plot_hx() (utils/fun_plot_hx.R)
# zone_lvls and pal_mozzy are provided by helper-setup.R (globalenv).

test_that("clean_long_hx_wk keeps Pipiens and Tarsalis (not just All)", {
  mk <- function(type) tibble::tibble(
    year = 2026, week = c(23, 23, 23),
    zone = factor("NW", levels = zone_lvls),
    spp  = factor(c("Pipiens", "Tarsalis", "All"),
                  levels = c("Pipiens", "Tarsalis", "All", "other spp", "none")),
    abund = c(1, 2, 3), pir = c(0.1, 0.2, 0), vi = c(0.1, 0.2, 0.3),
    type = type
  )
  ytd <- mk("current"); hx <- mk("hx")

  out <- clean_long_hx_wk(ytd, hx, spp_keep = c("Pipiens", "Tarsalis"))

  expect_setequal(as.character(unique(out$spp)), c("Pipiens", "Tarsalis"))
})
