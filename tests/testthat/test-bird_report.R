# tests/testthat/test-bird_report.R
#
# Unit tests for build_bird_report() and plot_birds() in utils/fun_bird_report.R

test_that("build_bird_report filters WNV birds and derives wnv_result", {
  cq_data <- tibble::tibble(
    sample_type = c("bird", "bird", "mosquito", "bird"),
    csu_id      = c("B1", "B2", "M1", "B3"),
    year = "2026", week = "24",
    target_name = c("WNV", "WNV", "WNV", "SLEV"),
    test_code   = c(1, 0, 1, 1),
    cq = c(30, NA, 28, 25), copies = c(100, 0, 200, 50),
    amp_status = c("AMP", "NOAMP", "AMP", "AMP")
  )

  out <- build_bird_report(cq_data)

  expect_named(out, c("birds", "bird_report"))
  expect_false("M1" %in% out$birds$csu_id)
  expect_setequal(out$bird_report$csu_id, c("B1", "B2"))
  expect_equal(out$bird_report$wnv_result[out$bird_report$csu_id == "B1"], "positive")
  expect_equal(out$bird_report$wnv_result[out$bird_report$csu_id == "B2"], "negative")
  # year/week are cast to double for the downstream natural merge
  expect_type(out$birds$year, "double")
  expect_type(out$birds$week, "double")
  # positives are surfaced first via arrange(desc(test_code))
  expect_equal(out$bird_report$csu_id[1], "B1")
})

test_that("plot_birds returns a ggplot object that builds without error", {
  bird_report <- tibble::tibble(
    csu_id = c("B1", "B2", "B3"), year = 2026, week = c(24, 24, 25),
    wnv_result = c("positive", "negative", "negative")
  )
  p <- plot_birds(bird_report)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_birds year filter excludes other years", {
  bird_report <- tibble::tibble(
    csu_id = c("B1", "B2", "B3", "B4"),
    year = c(2025, 2025, 2026, 2026),
    week = c(25, 26, 24, 25),
    wnv_result = c("negative", "negative", "positive", "negative")
  )
  p <- plot_birds(bird_report, year = 2026)
  built <- ggplot2::ggplot_build(p)
  # only 2026 weeks (24, 25) should appear on x-axis
  x_labels <- as.character(built$layout$panel_params[[1]]$x$breaks)
  expect_true(all(x_labels %in% c("24", "25")))
  expect_false("26" %in% x_labels)
})
