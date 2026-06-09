# tests/testthat/test-generate_report.R
#
# Tests for inject_graph_data() and generate_report() (utils/)
# inject_graph_data is implemented in utils/fun_inject_graph_data.R.
# generate_report will be added in a later task (B3+).

test_that("generate_report writes an xlsx containing the graphs sheet + data sheets", {
  skip_if_not(file.exists(here::here("tests/fixtures/graph_plot_template.xlsx")),
              "graphs template not present")

  out_path <- tempfile(fileext = ".xlsx")

  weekly_input <- tibble::tibble(a = 1:2, b = c("x", "y"))
  data_sheets  <- list(
    "Weekly Data Input" = weekly_input,
    "t1a" = tibble::tibble(zone = "NW", abund_Pipiens = 1.1),
    "t1b" = tibble::tibble(week = 23, current = 0.1),
    "t2a" = tibble::tibble(zone = "NW", collected_Pipiens = 10),
    "t2b" = tibble::tibble(week = 23, current = 5),
    "t3a" = tibble::tibble(zone = "NW", examined_Pipiens = 10),
    "t3b" = tibble::tibble(week = 23, current = 0.5)
  )
  graph_datasets <- list(t1a = tibble::tibble(zone = "NW", abund_Pipiens = 1.1))
  layout <- list(t1a = list(start_row = 6, start_col = 3))

  generate_report(
    data_sheets    = data_sheets,
    graph_datasets = graph_datasets,
    graph_layout   = layout,
    template_path  = here::here("tests/fixtures/graph_plot_template.xlsx"),
    out_path       = out_path,
    week_filter    = 24,
    update         = FALSE
  )

  expect_true(file.exists(out_path))
  sheets <- openxlsx::getSheetNames(out_path)
  expect_true("graphs" %in% sheets)
  expect_true(all(names(data_sheets) %in% sheets))
})

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
