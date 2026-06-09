# utils/fun_inject_graph_data.R
#
# DESCRIPTION
# Write each weekly dataset into the "graphs" worksheet at the cell anchors
# defined by `layout`. Intended to be called just after loading the graphs
# Excel template, before saving the final report workbook.
#
# openxlsx workbooks are reference (R5/environment) objects, so this mutates
# `wb` in place *and* returns it for chaining into further formatting steps.
#
# INPUTS
#   wb             an openxlsx workbook object (e.g. loaded via
#                  openxlsx::loadWorkbook() from the graphs template).
#   sheet          worksheet name to write into (e.g. "graphs").
#   datasets       named list of data frames; names must match keys of `layout`.
#                  Each data frame contains one weekly summary block
#                  (e.g. t1a, t2a, t3a from build_tables()).
#   layout         named list of list(start_row, start_col) — 1-based cell
#                  anchors for each block. Names must match `datasets`.
#   drop_first_col logical (default TRUE). When TRUE, the first column of each
#                  data frame (typically the zone or week label) is dropped
#                  before writing, because those labels already exist in the
#                  template scaffold and should not be overwritten.
#
# OUTPUT
#   The same workbook `wb` (invisibly mutated; returned for chaining).
#
# DEPENDENCIES
#   openxlsx  — for writeData(); available via helper-setup.R / pipeline env.
#   purrr     — for iwalk(); available via tidyverse.
#   No library() calls here — load via the pipeline or test harness.

inject_graph_data <- function(wb, sheet, datasets, layout,
                              drop_first_col = TRUE) {

  # ---------------------------------------------------------------------------
  # Walk over each named anchor in layout; nm = block name (e.g. "t1a").
  # We write ONLY the value cells at each anchor and never touch other cells,
  # so the template's titles / merged headers / labels are left intact.
  # ---------------------------------------------------------------------------
  purrr::iwalk(layout, function(anchor, nm) {

    # Guard: skip gracefully if this block has no matching dataset
    if (is.null(datasets[[nm]])) {
      warning(
        "inject_graph_data: no dataset supplied for graph block '", nm,
        "'; skipping."
      )
      return(invisible(NULL))
    }

    dat <- datasets[[nm]]

    # Optionally drop the label column (zone/week) — it already lives in the
    # template; writing it again would overwrite formatted cells unnecessarily.
    if (drop_first_col) {
      dat <- dat[, -1, drop = FALSE]
    }

    # Write the value matrix at the configured anchor (no column headers,
    # no row names, NA cells left blank so template formatting is preserved).
    openxlsx::writeData(
      wb       = wb,
      sheet    = sheet,
      x        = dat,
      startRow = anchor$start_row,
      startCol = anchor$start_col,
      colNames = FALSE,
      rowNames = FALSE,
      keepNA   = FALSE
    )
  })

  # Return wb for chaining (the object is already mutated in place, but
  # returning allows wb2 <- inject_graph_data(wb, ...) patterns in tests).
  wb
}
