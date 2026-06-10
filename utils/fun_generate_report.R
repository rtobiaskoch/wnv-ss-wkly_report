# utils/fun_generate_report.R
#
# DESCRIPTION
# Build the weekly report workbook from the pixel-faithful graphs template.
# Strategy: load the template (so the "graphs" sheet keeps every merge/style),
# inject the weekly datasets at config anchors, stamp the current week, add the
# flat data sheets, save. Optional GSheet push (update = TRUE) mirrors the legacy
# 0_R/generate_report.R behaviour. Depends on inject_graph_data().
#
# INPUTS
#   data_sheets     named list of data frames -> one flat worksheet each
#                   (names become the sheet titles in the output workbook).
#   graph_datasets  named list of data frames keyed to graph_layout
#                   (e.g. t1a, hx_vi, t2a, ...) — passed to inject_graph_data().
#   graph_layout    list of list(start_row, start_col) giving the cell anchor
#                   for each dataset block (config: graph_sheet_layout).
#   template_path   path to the graphs template .xlsx; must exist.
#   out_path        full .xlsx output path (written/overwritten on each run).
#   week_filter     current epiweek integer; written into the "Week: N" cells.
#   week_cells      list(row = <integer vector>, col = <integer>) pointing to
#                   the "Week: N" placeholder cells in the template.
#                   Default: row = c(5, 39, 73), col = 2  (cells B5, B39, B73 —
#                   the "Week: " & WEEKNUM(today()) formula cells in the template).
#   update          logical; when TRUE, pushes each data sheet to a new GSheet
#                   and uploads the formatted .xlsx (with the graphs sheet) to
#                   gfolder. Default FALSE.
#   gname           character; base name for the GSheet (used when update = TRUE).
#   gfolder         character; Google Drive folder name to move the GSheet and
#                   upload the .xlsx into (used when update = TRUE and non-NULL).
#
# OUTPUT
#   Returns the out_path invisibly. Side effect: writes the .xlsx file.
#
# DEPENDENCIES
#   openxlsx       — loadWorkbook / writeData / addWorksheet / saveWorkbook
#   purrr          — iwalk() for iterating over named lists
#   googlesheets4  — gs4_create / sheet_write / sheet_delete (update = TRUE only)
#   googledrive    — drive_get / drive_mv / as_id             (update = TRUE only)
#   tools          — file_path_sans_ext
#   inject_graph_data() from utils/fun_inject_graph_data.R (must be sourced first)
#
# No library() calls here — all packages loaded by the pipeline or test harness.

generate_report <- function(data_sheets, graph_datasets, graph_layout,
                            template_path, out_path, week_filter,
                            week_cells = list(row = c(4, 38, 72), col = 2),
                            update = FALSE, gname = NULL, gfolder = NULL) {

  # ---------------------------------------------------------------------------
  # Guard: the template must exist before we do anything else.
  # ---------------------------------------------------------------------------
  stopifnot(file.exists(template_path))

  # ---------------------------------------------------------------------------
  # 1. Load template workbook.
  #    openxlsx::loadWorkbook() preserves every merge, style, and formula in the
  #    "graphs" sheet — this is why we start from the template rather than an
  #    empty workbook.
  # ---------------------------------------------------------------------------
  wb <- openxlsx::loadWorkbook(template_path)

  # ---------------------------------------------------------------------------
  # 2. Inject weekly data into the graphs sheet at the configured anchors.
  #    inject_graph_data() writes value cells only; it does not touch titles,
  #    merged headers, or label columns already present in the template.
  # ---------------------------------------------------------------------------
  wb <- inject_graph_data(wb, sheet = "graphs",
                          datasets = graph_datasets,
                          layout   = graph_layout)

  # ---------------------------------------------------------------------------
  # 3. Stamp the current week into each "Week: N" placeholder cell.
  #    The template has these at column B (col = 2) rows 4, 38, 72 by default.
  #    We write a plain string so the cell text matches the legacy report format.
  # ---------------------------------------------------------------------------
  week_label <- paste0("Week: ", week_filter)

  for (r in week_cells$row) {
    openxlsx::writeData(
      wb       = wb,
      sheet    = "graphs",
      x        = week_label,
      startRow = r,
      startCol = week_cells$col,
      colNames = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Add the flat data sheets.
  #    purrr::iwalk() passes both the data frame (dat) and its name (nm) so we
  #    can create a worksheet with the correct title and write the data in one
  #    pass. These sheets sit beside "graphs" in the output workbook.
  # ---------------------------------------------------------------------------
  purrr::iwalk(data_sheets, function(dat, nm) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, dat)
  })

  # ---------------------------------------------------------------------------
  # 5. Save the assembled workbook to disk.
  #    overwrite = TRUE lets this be called on an existing path (e.g. re-run).
  # ---------------------------------------------------------------------------
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

  # ---------------------------------------------------------------------------
  # 6. Optional Drive push (matches legacy behaviour, plus the .xlsx itself).
  #    Namespaced calls (googlesheets4::, googledrive::) resolve at call time so
  #    the file can be sourced in offline / test contexts without those packages
  #    being attached — they only need to be installed when update = TRUE.
  # ---------------------------------------------------------------------------
  if (isTRUE(update)) {

    # Derive a sensible sheet name from the output filename when gname is NULL.
    sheet_name <- if (!is.null(gname)) {
      gname
    } else {
      tools::file_path_sans_ext(basename(out_path))
    }

    # Create a new Google Sheet (one tab per data_sheets entry) — flat data
    # only; native Google Sheets cannot reproduce the "graphs" sheet's merges/
    # formatting, which is why the .xlsx itself is also uploaded below.
    gsheet <- googlesheets4::gs4_create(sheet_name)

    purrr::iwalk(data_sheets, function(dat, nm) {
      googlesheets4::sheet_write(dat, ss = gsheet, sheet = nm)
    })

    # Remove the default empty "Sheet1" tab created by gs4_create().
    googlesheets4::sheet_delete(gsheet, sheet = "Sheet1")

    # Move the GSheet, and upload the formatted .xlsx (with the graphs sheet),
    # into the target Drive folder when one is specified.
    if (!is.null(gfolder)) {
      target_folder <- googledrive::drive_get(gfolder)
      googledrive::drive_mv(gsheet,
                            path = googledrive::as_id(target_folder))
      googledrive::drive_upload(
        media = out_path,
        path  = googledrive::as_id(target_folder),
        name  = basename(out_path),
        overwrite = TRUE
      )
    }
  }

  invisible(out_path)
}
