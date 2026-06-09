# tests/testthat/test-generate_report.R
#
# Tests for inject_graph_data() and generate_report() (utils/)
# inject_graph_data is implemented in utils/fun_inject_graph_data.R.
# generate_report will be added in a later task (B3+).

test_that("inject_graph_data writes value columns at the configured anchor", {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "graphs")
  # Simulate the template title in A1 so we can verify it is NOT clobbered.
  openxlsx::writeData(wb, "graphs", "GRAPH 1A title", startRow = 1, startCol = 1,
                      colNames = FALSE)

  datasets <- list(
    t1a = tibble::tibble(zone = c("NW", "NE"),
                         abund_Pipiens = c(1.1, 2.2),
                         vi_All        = c(0.5, 0.9))
  )
  layout <- list(t1a = list(start_row = 6, start_col = 3))

  wb2 <- inject_graph_data(wb, "graphs", datasets, layout, drop_first_col = TRUE)

  # skipEmptyCols = FALSE keeps leading empty columns so absolute positions hold
  written <- openxlsx::readWorkbook(wb2, sheet = "graphs", colNames = FALSE,
                                    skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  # value 1.1 should land at row 6, col 3 (col 1 = zone label, dropped)
  expect_equal(written[[3]][6], 1.1)
  expect_equal(written[[4]][7], 0.9)
  # the pre-existing A1 title must survive (inject must not pad/overwrite it)
  expect_equal(written[[1]][1], "GRAPH 1A title")
})
