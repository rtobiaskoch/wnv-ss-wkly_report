# Pipeline TODOs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convert the weekly report's inline script logic (`tables.R`, `generate_report.R`, the bird chunk) into pure, tested functions in `utils/`; auto-build the pixel-faithful "graphs" Excel sheet from a template; redesign the historical plot to stack Pipiens/Tarsalis; wire the existing `wnvSurv::plot_n_trap` trap-status plot into the pipeline; and extend `smoke_test.R` to render into an **isolated smoke directory** and assert that a generated report (xlsx with `graphs` sheet) and the historical plot are produced for a data-bearing week (2025 w33).

**Architecture:** New logic lives as `fun_*.R` files in `utils/` (the project's current convention — these are pipeline-specific, not general enough for `wnvSurv`). Each pure function returns objects; side effects (writing CSVs, GSheet pushes, `ggsave`) stay in the QMD chunks that call them. The pixel-faithful graphs sheet is produced by loading `tests/fixtures/graph_plot_template.xlsx` as the workbook base — preserving every merge/style for free — and injecting the 6 weekly datasets at fixed cell offsets defined in config.

**Tech Stack:** R, tidyverse (dplyr/tidyr/purrr), `openxlsx`, `ggplot2`, `testthat`, Quarto.

---

## Context

`docs/TODO.md` lists a `## pipeline` section of cleanup work. Two of its items are already complete and are therefore **excluded** from this plan:

- *"set email as an argument for the googleAuth"* — already implemented in `config/load_packages.R:34-46` via `GARGLE_OAUTH_EMAIL`.
- The trap-status plot itself already exists in the `wnvSurv` package source (`../wnv-ss_functions/R/plot_n_trap.R`); only the *wiring into the weekly pipeline* remains.

The remaining work matters because the report-building logic currently lives as loose scripts (`0_R/tables.R`, `0_R/generate_report.R`) sourced from inside the QMD, and `0_R/` is flagged legacy/non-authoritative in `CLAUDE.md`. The bird logic is a 48-line inline chunk with no plot. The "graphs" worksheet that epidemiologists use for charting is currently hand-built each week from CSVs. Converting these to pure functions makes them testable (RSE principle 8), composable, and removes the dependency on legacy `0_R/`.

**Scope (user-confirmed):** (A) report+tables → functions, (B) graphs sheet — *pixel-faithful*, (C) bird chunk → functions + plot, (D) historical plot redesign, (E) wire trap_status. Functions go in `utils/`.

---

## Key Files & Current State

- `0_R/tables.R` — builds A-tables `t1a/t2a/t3a` from globals `current_wk`, `pools`; writes `table1a/2a/3a.csv`. To be replaced by `utils/fun_build_tables.R`.
- `0_R/generate_report.R` — reads table CSVs + weekly input, writes `<fn_report>.xlsx` (7 data sheets), optionally pushes to GSheets. To be replaced by `utils/fun_generate_report.R` + graphs injection.
- `tests/fixtures/graph_plot_template.xlsx` — single sheet `graphs`, 100×15, fixed scaffold with merged headers + blank data regions. The pixel-faithful target.
- `wnv-s_weekly_report_pipeline_v2.qmd` — bird chunk at lines 290-338; B-tables chunk 725-753 (writes `table1b/2b/3b.csv` via `create_hx_report`); A-tables + report at 757-767; historical plot at 678-721.
- `utils/fun_plot_hx.R` — `clean_long_hx_wk()` (filters `spp=="All"`) + `plot_hx()` (dodged current-vs-hx area, faceted by zone). To be redesigned.
- `config/config_weekly.R:423-433` — `pal_mozzy` (`hx_Tarsalis`, `hx_Pipiens`, `current_Tarsalis`, `current_Pipiens`).
- `wnvSurv::plot_n_trap` (source `../wnv-ss_functions/R/plot_n_trap.R`) — needs `zone2` (from `wnv_s_clean`) and a `trap_status` column.
- `utils/fun_insert_blank_row.R` — `insert_blank_row(df, after_row)`, reused by tables.

### Template data-injection offsets (verified from `tests/fixtures/graph_plot_template.xlsx`)

The `graphs` sheet is static except for blank data regions. Inject **value columns only** (zone/week labels already present in col B) at these 1-based `(startRow, startCol)` anchors:

| Block | Dataset (in-memory / CSV)        | startRow | startCol (C=3) | rows | notes |
|-------|----------------------------------|----------|----------------|------|-------|
| 1A    | `t1a` / `table1a.csv`            | 6        | 3              | 9    | 8 zones + blank at row 11; 7 value cols |
| 1B    | `hx_vi` / `table1b_hx_vi.csv`    | 19       | 3              | 15   | weeks 23-37 in col B already |
| 2A    | `t2a` / `table2a.csv`            | 40       | 3              | 9    | blank row at 45 |
| 2B    | `hx_abund` / `table2b_hx_abund.csv` | 53    | 3              | 15   | |
| 3A    | `t3a` / `table3a.csv`            | 73       | 3              | 9    | blank row at 78 |
| 3B    | `hx_pir` / `table3b_hx_pir.csv`  | 86       | 3              | 15   | |

"Week: N" cells live at `B4`, `B38`, `B72` (current `week_filter`).

---

## Phase A — `build_tables()` function

**Files:**
- Create: `utils/fun_build_tables.R`
- Create test: `tests/testthat/test-build_tables.R`
- Modify: `wnv-s_weekly_report_pipeline_v2.qmd:757-759` (replace `source("0_R/tables.R")`)

### Task A1: Extract `build_tables()` as a pure function

- [x] **Step 1: Write the failing test**

Create `tests/testthat/test-build_tables.R`:

```r
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
  # t1a drops abund_All and pir_All but keeps vi_All
  expect_true("vi_All" %in% names(out$t1a))
  expect_false("abund_All" %in% names(out$t1a))
  # t3a has the full ordered column set
  expect_true(all(c("examined_Pipiens", "pool_all", "pir_All") %in% names(out$t3a)))
})
```

- [x] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-build_tables.R")'`
Expected: FAIL with `could not find function "build_tables"`.

- [x] **Step 3: Write the implementation**

Create `utils/fun_build_tables.R` by lifting `0_R/tables.R` into a function. Globals become arguments; remove all `write.csv` side effects (the caller writes). Keep the inner `t1a()` pivot helper and reuse `insert_blank_row()`:

```r
# utils/fun_build_tables.R
# Build the three "A" report tables (current-week summaries) for the weekly
# WNV report. Pure: takes the cleaned current-week stats and pool counts,
# returns a named list of data frames. No file I/O, no globals.
#
# current_wk: one row per zone x spp for the current week, with columns
#             zone, spp, abund, pir, vi, mosq_L, trap_L (from calc_all()).
# pools:      one row per zone x spp with n_pools, n_pos_pools.
build_tables <- function(current_wk, pools) {

  # --- inner helper: wide pivot of one estimate, zone x spp -----------------
  pivot_est <- function(df, col, prefix) {
    df %>%
      dplyr::select(zone, spp, {{ col }}) %>%
      tidyr::pivot_wider(names_from = spp,
                         values_from = {{ col }},
                         names_prefix = prefix)
  }

  # --- TABLE 1A: abundance + PIR + VI by zone -------------------------------
  t1a_abund <- pivot_est(current_wk, abund, "abund_")
  t1a_pir   <- pivot_est(current_wk, pir,   "pir_")
  t1a_vi    <- pivot_est(current_wk, vi,    "vi_")

  t1a <- t1a_abund %>%
    dplyr::left_join(t1a_pir, by = "zone") %>%
    dplyr::left_join(t1a_vi,  by = "zone") %>%
    dplyr::select(-abund_All, -pir_All) %>%
    insert_blank_row(5)

  # --- TABLE 2A: collected, traps, abundance --------------------------------
  t2a_collected <- current_wk %>%
    dplyr::select(zone, spp, mosq_L) %>%
    tidyr::pivot_wider(names_from = "spp", values_from = "mosq_L",
                       names_prefix = "collected_", values_fill = 0)

  t2a_traps <- current_wk %>% dplyr::distinct(zone, trap_L)

  if (nrow(janitor::get_dupes(t2a_traps)) > 0) {
    message("the number of traps for pipiens and tarsalis don't match")
  }

  t2a <- t2a_collected %>%
    dplyr::left_join(t2a_traps, by = "zone") %>%
    dplyr::left_join(t1a_abund, by = "zone") %>%
    dplyr::mutate(dplyr::across(.cols = -zone, ~ round(.x, 2))) %>%
    insert_blank_row(5)

  # --- TABLE 3A: examined, pools, positive pools, PIR per 1000 --------------
  t3a_examined <- current_wk %>%
    dplyr::select(zone, spp, mosq_L) %>%
    tidyr::pivot_wider(names_from = "spp", values_from = "mosq_L",
                       names_prefix = "examined_", values_fill = 0)

  t3a_pools <- pools %>%
    dplyr::select(zone, spp, n_pools) %>%
    tidyr::pivot_wider(names_from = "spp", values_from = "n_pools",
                       names_prefix = "pool_", values_fill = 0)

  t3a_p_pools <- pools %>%
    dplyr::select(zone, spp, n_pos_pools) %>%
    tidyr::pivot_wider(names_from = "spp", values_from = "n_pos_pools",
                       names_prefix = "pos_pool_", values_fill = 0)

  t3a_pir <- t1a_pir %>%
    dplyr::mutate(dplyr::across(-zone, ~ round(. * 1000, 2)))

  t3a_cols <- c("zone",
                "examined_Pipiens", "examined_Tarsalis", "examined_All",
                "pool_Pipiens", "pool_Tarsalis", "pool_all",
                "pos_pool_Pipiens", "pos_pool_Tarsalis", "pos_pool_all",
                "pir_Pipiens", "pir_Tarsalis", "pir_All")

  t3a <- t3a_examined %>%
    dplyr::left_join(t3a_pools,   by = "zone") %>%
    dplyr::left_join(t3a_p_pools, by = "zone") %>%
    dplyr::left_join(t3a_pir,     by = "zone") %>%
    dplyr::mutate(pool_all     = pool_Pipiens + pool_Tarsalis,
                  pos_pool_all = pos_pool_Pipiens + pos_pool_Tarsalis) %>%
    insert_blank_row(5) %>%
    dplyr::select(dplyr::all_of(t3a_cols))

  list(t1a = t1a, t2a = t2a, t3a = t3a)
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-build_tables.R")'`
Expected: PASS.

- [x] **Step 5: Wire into the QMD**

In `wnv-s_weekly_report_pipeline_v2.qmd`, replace the `format-A-tables` chunk (lines 757-759):

```r
#| label: format-A-tables
#| include: false
a_tables <- build_tables(current_wk, pools)
t1a <- a_tables$t1a
t2a <- a_tables$t2a
t3a <- a_tables$t3a

write.csv(t1a, file.path(dir_output, "table1a.csv"), row.names = FALSE)
write.csv(t2a, file.path(dir_output, "table2a.csv"), row.names = FALSE)
write.csv(t3a, file.path(dir_output, "table3a.csv"), row.names = FALSE)
```

- [x] **Step 6: Commit**

```bash
git add utils/fun_build_tables.R tests/testthat/test-build_tables.R wnv-s_weekly_report_pipeline_v2.qmd
git commit -m "refactor: extract build_tables() from 0_R/tables.R into tested utils function"
```

---

## Phase B — `generate_report()` + pixel-faithful graphs sheet

**Files:**
- Modify: `config/config_weekly.R` (add `graph_sheet_layout` + `fn_graph_template`)
- Create: `utils/fun_inject_graph_data.R`
- Create: `utils/fun_generate_report.R`
- Create test: `tests/testthat/test-generate_report.R`
- Modify: `wnv-s_weekly_report_pipeline_v2.qmd:761-767`

### Task B1: Add graph layout config

- [x] **Step 1: Add the layout spec to config** (parameter-driven, RSE principle 3)

Append to `config/config_weekly.R` (after the color settings block, ~line 434):

```r
#------------------------------- G R A P H  S H E E T --------------------------
# Path to the pixel-faithful "graphs" worksheet scaffold. generate_report()
# loads this as the workbook base so all merged headers / styling are preserved,
# then injects weekly data at the (startRow, startCol) anchors below.
fn_graph_template <- "tests/fixtures/graph_plot_template.xlsx"

# One entry per data block. `dataset` is the name passed in the named list to
# inject_graph_data(); anchors are 1-based cell coordinates verified against the
# template. Only value columns are written (zone/week labels already present).
graph_sheet_layout <- list(
  t1a      = list(start_row = 6,  start_col = 3),
  hx_vi    = list(start_row = 19, start_col = 3),
  t2a      = list(start_row = 40, start_col = 3),
  hx_abund = list(start_row = 53, start_col = 3),
  t3a      = list(start_row = 73, start_col = 3),
  hx_pir   = list(start_row = 86, start_col = 3)
)

# Cells holding the current week label ("Week: N") on the graphs sheet.
graph_week_cells <- list(row = c(4, 38, 72), col = 2)
```

- [x] **Step 2: Commit**

```bash
git add config/config_weekly.R
git commit -m "config: add graph_sheet_layout + template path for graphs sheet"
```

### Task B2: `inject_graph_data()` — write datasets into the graphs sheet

**Files:**
- Create: `utils/fun_inject_graph_data.R`
- Test: `tests/testthat/test-generate_report.R` (shared with B3)

- [x] **Step 1: Write the failing test**

Create `tests/testthat/test-generate_report.R`:

```r
test_that("inject_graph_data writes value columns at the configured anchor", {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "graphs")

  datasets <- list(
    t1a = tibble::tibble(zone = c("NW", "NE"),
                         abund_Pipiens = c(1.1, 2.2),
                         vi_All        = c(0.5, 0.9))
  )
  layout <- list(t1a = list(start_row = 6, start_col = 3))

  wb2 <- inject_graph_data(wb, "graphs", datasets, layout, drop_first_col = TRUE)

  written <- openxlsx::readWorkbook(wb2, sheet = "graphs",
                                    colNames = FALSE, skipEmptyRows = FALSE)
  # value 1.1 should land at row 6, col 3 (col 1 = zone label, dropped)
  expect_equal(written[[3]][6], 1.1)
  expect_equal(written[[4]][7], 0.9)
})
```

- [x] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-generate_report.R")'`
Expected: FAIL with `could not find function "inject_graph_data"`.

- [x] **Step 3: Write the implementation**

```r
# utils/fun_inject_graph_data.R
# Write each weekly dataset into the "graphs" worksheet at the cell anchors
# defined by `layout`. Pure w.r.t. inputs: mutates the openxlsx workbook object
# (openxlsx is reference-semantics by design) and returns it for chaining.
#
# wb:             an openxlsx workbook (loaded from the template).
# sheet:          worksheet name (e.g. "graphs").
# datasets:       named list of data frames; names must match `layout`.
# layout:         named list of list(start_row, start_col) (see config).
# drop_first_col: TRUE drops the label column (zone/week) before writing,
#                 because those labels already exist in the template.
inject_graph_data <- function(wb, sheet, datasets, layout,
                              drop_first_col = TRUE) {
  purrr::iwalk(layout, function(anchor, nm) {
    if (is.null(datasets[[nm]])) {
      warning("No dataset supplied for graph block '", nm, "'; skipping.")
      return(invisible(NULL))
    }
    dat <- datasets[[nm]]
    if (drop_first_col) dat <- dat[, -1, drop = FALSE]

    openxlsx::writeData(wb, sheet = sheet, x = dat,
                        startRow = anchor$start_row,
                        startCol = anchor$start_col,
                        colNames = FALSE, rowNames = FALSE,
                        keepNA   = FALSE)
  })
  wb
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-generate_report.R")'`
Expected: PASS (the inject test).

- [x] **Step 5: Commit**

```bash
git add utils/fun_inject_graph_data.R tests/testthat/test-generate_report.R
git commit -m "feat: add inject_graph_data() to fill graphs sheet at config anchors"
```

### Task B3: `generate_report()` — assemble workbook from template

**Files:**
- Create: `utils/fun_generate_report.R`
- Modify test: `tests/testthat/test-generate_report.R` (add a case)

- [x] **Step 1: Add the failing test** (append to `tests/testthat/test-generate_report.R`)

```r
test_that("generate_report writes an xlsx containing the graphs sheet + data sheets", {
  skip_if_not(file.exists("tests/fixtures/graph_plot_template.xlsx"),
              "graphs template not present")

  tmp_out <- withr::local_tempdir()
  out_path <- file.path(tmp_out, "test_report.xlsx")

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
    template_path  = "tests/fixtures/graph_plot_template.xlsx",
    out_path       = out_path,
    week_filter    = 24,
    update         = FALSE
  )

  expect_true(file.exists(out_path))
  sheets <- openxlsx::getSheetNames(out_path)
  expect_true("graphs" %in% sheets)
  expect_true(all(names(data_sheets) %in% sheets))
})
```

- [x] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-generate_report.R")'`
Expected: FAIL with `could not find function "generate_report"`.

- [x] **Step 3: Write the implementation**

```r
# utils/fun_generate_report.R
# Build the weekly report workbook from the pixel-faithful template.
# Strategy: load the template (so the "graphs" sheet keeps every merge/style),
# inject the 6 weekly datasets, add the flat data sheets, then save. The GSheet
# push (update = TRUE) is preserved from the legacy 0_R/generate_report.R.
#
# data_sheets:    named list -> one flat worksheet each (names = sheet titles).
# graph_datasets: named list keyed to graph_layout (t1a, hx_vi, t2a, ...).
# graph_layout:   list of list(start_row, start_col) (config graph_sheet_layout).
# template_path:  path to graph plot template.xlsx.
# out_path:       full .xlsx output path.
# week_filter:    current epiweek (written into the "Week: N" cells).
# update/gname/gfolder: optional GSheet push (mirrors legacy behaviour).
generate_report <- function(data_sheets, graph_datasets, graph_layout,
                            template_path, out_path, week_filter,
                            week_cells = list(row = c(4, 38, 72), col = 2),
                            update = FALSE, gname = NULL, gfolder = NULL) {

  stopifnot(file.exists(template_path))

  # 1. Load template -> preserves the formatted "graphs" sheet.
  wb <- openxlsx::loadWorkbook(template_path)

  # 2. Inject weekly data into the graphs sheet.
  wb <- inject_graph_data(wb, sheet = "graphs",
                          datasets = graph_datasets, layout = graph_layout)

  # 3. Stamp the current week into the "Week: N" cells.
  for (r in week_cells$row) {
    openxlsx::writeData(wb, sheet = "graphs",
                        x = paste0("Week: ", week_filter),
                        startRow = r, startCol = week_cells$col,
                        colNames = FALSE)
  }

  # 4. Add the flat data sheets.
  purrr::iwalk(data_sheets, function(dat, nm) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, dat)
  })

  # 5. Save.
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

  # 6. Optional GSheet push (data sheets only; matches legacy behaviour).
  if (isTRUE(update)) {
    gsheet <- googlesheets4::gs4_create(tools::file_path_sans_ext(basename(out_path)))
    purrr::iwalk(data_sheets, function(dat, nm) {
      googlesheets4::sheet_write(dat, ss = gsheet, sheet = nm)
    })
    googlesheets4::sheet_delete(gsheet, sheet = "Sheet1")
    if (!is.null(gfolder)) {
      target_folder <- googledrive::drive_get(gfolder)
      googledrive::drive_mv(gsheet, path = googledrive::as_id(target_folder))
    }
  }

  invisible(out_path)
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-generate_report.R")'`
Expected: PASS (both cases).

- [x] **Step 5: Wire into the QMD**

Replace the `generate-report` chunk (lines 761-767). It needs the in-memory A-tables and the B-tables (`hx_vi`, `hx_abund`, `hx_pir` already built in the `format-hx-B-tables` chunk) plus the weekly input frame `fn_weekly_input_format` reads:

```r
#| label: generate-report
#| include: false
weekly_input <- readr::read_csv(fn_weekly_input_format, show_col_types = FALSE)

data_sheets <- list(
  "Weekly Data Input" = weekly_input,
  "t1a" = t1a, "t1b" = hx_vi,
  "t2a" = t2a, "t2b" = hx_abund,
  "t3a" = t3a, "t3b" = hx_pir
)

graph_datasets <- list(
  t1a = t1a, hx_vi = hx_vi,
  t2a = t2a, hx_abund = hx_abund,
  t3a = t3a, hx_pir = hx_pir
)

generate_report(
  data_sheets    = data_sheets,
  graph_datasets = graph_datasets,
  graph_layout   = graph_sheet_layout,
  template_path  = fn_graph_template,
  out_path       = file.path(dir_output, paste0(fn_report, ".xlsx")),
  week_filter    = week_filter,
  week_cells     = graph_week_cells,
  update         = update,
  gname          = fn_report,
  gfolder        = weekly_report_folder
)
```

> **Note for implementer:** confirm the B-table objects are named `hx_vi`, `hx_abund`, `hx_pir` and in scope at this point (they are created in the `format-hx-B-tables` chunk, lines 733-748). Confirm `fn_weekly_input_format`, `fn_report`, `dir_output`, `weekly_report_folder` resolve from config. Verify the graph value-column counts match the template header widths (1A=7, 2A=variable, etc.) by opening the generated file once.

- [x] **Step 6: Commit**

```bash
git add utils/fun_generate_report.R tests/testthat/test-generate_report.R wnv-s_weekly_report_pipeline_v2.qmd
git commit -m "feat: generate_report() builds report from graphs template with injected data"
```

---

## Phase C — Bird chunk → functions + plot

**Files:**
- Create: `utils/fun_bird_report.R` (`build_bird_report`, `plot_birds`)
- Create test: `tests/testthat/test-bird_report.R`
- Modify: `wnv-s_weekly_report_pipeline_v2.qmd:290-338`

### Task C1: `build_bird_report()` — pure extraction of bird data

- [x] **Step 1: Write the failing test**

Create `tests/testthat/test-bird_report.R`:

```r
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
  # mosquito row excluded from birds
  expect_false("M1" %in% out$birds$csu_id)
  # bird_report only WNV, with positive/negative label
  expect_setequal(out$bird_report$csu_id, c("B1", "B2"))
  expect_equal(out$bird_report$wnv_result[out$bird_report$csu_id == "B1"], "positive")
  expect_equal(out$bird_report$wnv_result[out$bird_report$csu_id == "B2"], "negative")
})
```

- [x] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-bird_report.R")'`
Expected: FAIL with `could not find function "build_bird_report"`.

- [x] **Step 3: Write the implementation**

```r
# utils/fun_bird_report.R
# Bird (dead-bird WNV surveillance) helpers. Pure: extract and shape; the GSheet
# archive push stays in the QMD. See pipeline bird chunk (legacy lines 290-338).

# build_bird_report(): from the merged Cq table, return the tidy bird records
# and a WNV positive/negative report.
build_bird_report <- function(cq_data, target = "WNV") {
  birds <- cq_data %>%
    dplyr::filter(sample_type == "bird") %>%
    dplyr::mutate(year = as.double(year), week = as.double(week)) %>%
    dplyr::select(csu_id, year, week, target_name, test_code,
                  cq, copies, amp_status) %>%
    dplyr::arrange(year, week, csu_id)

  bird_report <- birds %>%
    dplyr::filter(target_name == target) %>%
    dplyr::arrange(dplyr::desc(test_code)) %>%
    dplyr::mutate(wnv_result = dplyr::if_else(test_code == 1,
                                              "positive", "negative")) %>%
    dplyr::select(csu_id, year, week, wnv_result)

  list(birds = birds, bird_report = bird_report)
}

# plot_birds(): bar of WNV result counts by week. Categorical fill uses a
# WesAnderson/Brewer-style two-colour scale per project conventions.
plot_birds <- function(bird_report,
                       pal = c("positive" = "#c5283d", "negative" = "grey70")) {
  bird_report %>%
    dplyr::count(week, wnv_result) %>%
    ggplot2::ggplot(ggplot2::aes(x = week, y = n, fill = wnv_result)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(x = "Week", y = "Birds tested", fill = "WNV result",
                  title = "Dead bird WNV surveillance") +
    ggplot2::theme_classic()
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-bird_report.R")'`
Expected: PASS.

- [x] **Step 5: Commit**

```bash
git add utils/fun_bird_report.R tests/testthat/test-bird_report.R
git commit -m "refactor: extract build_bird_report() + plot_birds() from bird chunk"
```

### Task C2: Add `plot_birds` smoke test + wire chunk

- [x] **Step 1: Add a smoke test** (append to `tests/testthat/test-bird_report.R`)

```r
test_that("plot_birds returns a ggplot object", {
  bird_report <- tibble::tibble(
    csu_id = c("B1", "B2", "B3"), year = 2026, week = c(24, 24, 25),
    wnv_result = c("positive", "negative", "negative")
  )
  p <- plot_birds(bird_report)
  expect_s3_class(p, "ggplot")
})
```

- [x] **Step 2: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-bird_report.R")'`
Expected: PASS.

- [x] **Step 3: Rewrite the bird chunk in the QMD** (lines 290-338)

```r
#| label: birds
#| include: false
bird_out    <- build_bird_report(cq_data)
birds       <- bird_out$birds
bird_report <- bird_out$bird_report

if (update) {
  update_gsheet(new = birds,
                by = c("csu_id", "year", "week", "target_name"),
                fn_save = fn_bird_output, gkey = key_birds,
                gfolder = "database_archive", gname = "birds_archive",
                col_database = names(birds), sheet = "data")

  bird_sheet <- paste0("y", year_filter, "_w", week_filter)
  if (bird_sheet %in% sheet_names(key_birds)) sheet_delete(key_birds, sheet = bird_sheet)
  safe_sheet_add   <- possibly(sheet_add,   otherwise = NULL)
  safe_sheet_write <- possibly(sheet_write, otherwise = NULL)
  safe_sheet_add(ss = key_birds, sheet = bird_sheet, .after = 1)
  safe_sheet_write(bird_report, ss = key_birds, sheet = bird_sheet)
}

# Bird plot output
p_birds <- plot_birds(bird_report)
ggsave(paste0(dir_plot_wk, "birds.png"), p_birds, height = 6, width = 9, units = "in")
```

- [x] **Step 4: Commit**

```bash
git add tests/testthat/test-bird_report.R wnv-s_weekly_report_pipeline_v2.qmd
git commit -m "feat: bird plot output + wire build_bird_report into pipeline"
```

---

## Phase D — Historical plot: stacked Pipiens/Tarsalis

**Files:**
- Modify: `utils/fun_plot_hx.R` (`clean_long_hx_wk`, `plot_hx`)
- Create test: `tests/testthat/test-plot_hx.R`
- Modify: `wnv-s_weekly_report_pipeline_v2.qmd:678-707`

### Task D1: Keep both species in `clean_long_hx_wk`

- [x] **Step 1: Write the failing test**

Create `tests/testthat/test-plot_hx.R`:

```r
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
```

- [x] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-plot_hx.R")'`
Expected: FAIL — current `clean_long_hx_wk` hardcodes `filter(spp == "All")` and has no `spp_keep` arg, so the result has only `All`.

- [x] **Step 3: Modify `clean_long_hx_wk`** in `utils/fun_plot_hx.R`

Add an `spp_keep` parameter and replace the hardcoded `filter(spp == "All")`:

```r
clean_long_hx_wk = function(ytd, hx,
                            rm_zone = NULL,
                            comb_zone = T,
                            grp_vars = c("year", "week", "zone", "spp"),
                            spp_keep = c("Pipiens", "Tarsalis")) {

  curr_hx_df = bind_rows(ytd, hx) %>%
    filter(!zone %in% rm_zone) %>%
    select(-any_of(c("mosq_L", "trap_L", "zone2"))) %>%
    pivot_longer(cols = -c(grp_vars, type),
                 names_to = "est",
                 values_to = "value") %>%
    mutate(type = factor(type, levels = c("hx", "current")),
           zone = factor(zone, levels = zone_lvls)) %>%
    pivot_wider(names_from = est, values_from = value) %>%
    group_by(zone, week, spp, type) %>%
    summarise(abund = mean(abund, na.rm = T),
              pir = mean(pir, na.rm = T),
              vi = mean(vi, na.rm = T), .groups = "drop") %>%
    filter(spp %in% spp_keep)

  return(curr_hx_df)
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-plot_hx.R")'`
Expected: PASS.

- [x] **Step 5: Commit**

```bash
git add utils/fun_plot_hx.R tests/testthat/test-plot_hx.R
git commit -m "refactor: clean_long_hx_wk keeps Pipiens/Tarsalis via spp_keep arg"
```

### Task D2: Stack species in `plot_hx` with `pal_mozzy`

- [x] **Step 1: Add the failing test** (append to `tests/testthat/test-plot_hx.R`)

```r
test_that("plot_hx returns a ggplot and uses a type_spp fill key", {
  df <- tibble::tibble(
    week = c(23, 23, 23, 23),
    zone = factor("NW", levels = zone_lvls),
    spp  = c("Pipiens", "Tarsalis", "Pipiens", "Tarsalis"),
    type = factor(c("current", "current", "hx", "hx"), levels = c("hx", "current")),
    abund = c(1, 2, 0.5, 1), pir = 0, vi = 0
  )
  p <- plot_hx(df, abund, "Abundance", pallette = pal_mozzy)
  expect_s3_class(p, "ggplot")
  # fill maps to the combined type_spp key
  expect_true("grp" %in% names(p$data) || "grp" %in% names(rlang::quo_get_expr(p$mapping$fill)))
})
```

- [x] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-plot_hx.R")'`
Expected: FAIL — current `plot_hx` fills by `type` only and defaults to a 2-colour palette.

- [x] **Step 3: Modify `plot_hx`** in `utils/fun_plot_hx.R`

Replace the dodged-area `plot_hx` with a stacked-area version keyed by `type_spp`, defaulting to `pal_mozzy`:

```r
plot_hx = function(df, value, text, pallette = pal_mozzy) {

  min_week <- min(df$week, na.rm = TRUE)
  max_week <- max(df$week, na.rm = TRUE)

  # Combine type (hx/current) and species into one fill key that matches the
  # pal_mozzy names: "hx_Pipiens", "current_Tarsalis", etc.
  df <- df %>%
    dplyr::mutate(grp = paste(type, spp, sep = "_"))

  p = ggplot(df, aes(x = week, y = {{ value }},
                     fill = grp, group = grp)) +
    geom_hline(yintercept = 0) +
    geom_area(position = "stack", alpha = 0.6) +
    facet_grid(zone ~ type) +              # current vs hx side-by-side
    theme_classic() +
    ggtitle(text) +
    scale_x_continuous(
      limits = c(min_week, max_week),
      breaks = seq(min_week, max_week, by = 2)
    ) +
    scale_fill_manual(values = pallette)

  return(p)
}
```

> **Decision note for implementer / user:** species are stacked within each `type`. Faceting `zone ~ type` keeps current and historical comparable without overplotting two stacked areas on the same panel. If the user prefers both on one panel, drop `type` from `facet_grid` — but stacked current + stacked hx on one axis overlaps; confirm before changing.

- [x] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-plot_hx.R")'`
Expected: PASS.

- [x] **Step 5: Update the QMD call site** (lines 678-690)

The `plot-zone-stats-area` chunk should request both species; the `plot_hx()` calls themselves are unchanged (the palette now defaults to `pal_mozzy`):

```r
ytd_hx_wk = clean_long_hx_wk(ytd, hx,
                             rm_zone = c("BC"),
                             grp_vars = c("year", "week", "zone", "spp"),
                             spp_keep = c("Pipiens", "Tarsalis"))

p_abund = plot_hx(ytd_hx_wk, abund, "Abundance")
p_pir   = plot_hx(ytd_hx_wk, pir, "PIR")
p_vi    = plot_hx(ytd_hx_wk, vi, "Vector Index") +
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.25))
```

- [x] **Step 6: Commit**

```bash
git add utils/fun_plot_hx.R tests/testthat/test-plot_hx.R wnv-s_weekly_report_pipeline_v2.qmd
git commit -m "feat: stacked Pipiens/Tarsalis historical plot using pal_mozzy"
```

---

## Phase E — Wire `wnvSurv::plot_n_trap` (trap status) into the pipeline

**Files:**
- Modify: `wnv-s_weekly_report_pipeline_v2.qmd` (add a chunk after the abundance/pools section)

### Task E1: Verify input contract, then add the chunk

- [x] **Step 1: Verify the cleaned weekly data has `zone2` + `trap_status`**

Run:
```bash
Rscript -e 'cat(deparse(args(wnvSurv::plot_n_trap)), "\n"); cat(deparse(args(wnvSurv::wnv_s_clean)), "\n")'
```
Then confirm whether `wnv_s_clean()` (used on `culex_update` at QMD line 611) produces a `trap_status` column. `plot_n_trap` requires both `zone2` and `trap_status` (see `../wnv-ss_functions/R/plot_n_trap.R`).
Expected: identify whether `trap_status` exists after `wnv_s_clean`. If it does **not**, the chunk must first call the trap-status assignment used in the combiner repo (`trapHxCombiner::assign_trap_status` / `fun_assign_trap_status.R`) or replicate that logic. **STOP and report to the user which path is needed** before writing the chunk if `trap_status` is absent.

- [x] **Step 2: Add the trap-status chunk** (after the `pools` plot chunk, ~line 604)

```r
### TRAP STATUS PLOT

```{r, trap-status-plot}
# Trap status (malfunction / culex / no culex / no mosquitoes) per zone-week.
# Uses wnvSurv::plot_n_trap; culex_update is already wnv_s_clean()'d above, so
# it carries the zone2 column the function requires.
p_trap_status <- wnvSurv::plot_n_trap(culex_update, rm_zone = c("BC"))
p_trap_status

fn_trap_status <- paste0(dir_plot_wk, "trap_status.png")
ggsave(fn_trap_status, p_trap_status, height = 8, width = 12, units = "in")

if (update) {
  drive_upload(media = fn_trap_status,
               path  = as_id(key_plots_dir),
               name  = basename(fn_trap_status))
}
```
```

- [x] **Step 3: Commit**

```bash
git add wnv-s_weekly_report_pipeline_v2.qmd
git commit -m "feat: wire wnvSurv::plot_n_trap trap-status plot into weekly pipeline"
```

> **Implementer note:** Step 1 found `wnv_s_clean()` (default args) produces both `zone2` and `trap_status` — no combiner import needed. Also found and fixed a pre-existing bug: QMD lines 602-603 called `wnv_s_clean(silence = T)`, but the installed `wnvSurv` only has `verbose` (no `silence`), which errored before `culex_update`/`database_update` could be created. Fixed to `verbose = F`. The trap-status chunk now creates `culex_update` (moved earlier, before `calc-zone-stats`); the `calc-zone-stats` chunk reuses it.

---

## Phase G — Isolated smoke directory + report & historical-plot assertions

The smoke test currently renders into the **live** `3_output/{year}/w{week}` tree and asserts only the xlsx + table CSVs. This phase (1) adds an output-root override so the smoke test writes to an isolated `tests/smoke_out/` (and `tests/smoke_mid/`) instead of polluting live outputs, and (2) makes "completed work" explicit in the pass criteria: a **generated report** (xlsx containing the `graphs` sheet) **and the historical plot** must exist. Week stays 2025 w33 (has input data + golden reference). Depends on Phases B (graphs sheet) and D (historical plot).

**Files:**
- Modify: `config/config_weekly.R` (add `--output`/`--mid` args; derive output/mid roots from them)
- Modify: `tests/smoke_test.R`

### Task G1: Output-root override in config

**Why an override, not editing `dir_output` directly:** `config_weekly.R` already derives `dir_input` from a `--input` arg (line 99) for staging environments. Mirroring that for output keeps the live default (`3_output`/`2_mid`) untouched while letting the smoke test redirect — parameter-driven, no special-casing in the QMD (the QMD just reads the baked config snapshot).

- [x] **Step 1: Add the `--output` and `--mid` arguments**

In `config/config_weekly.R`, next to the existing `--input` argument definition (above line 60), add:

```r
parser$add_argument(
  "--output",
  help = "base output folder; defaults to 3_output. Override for smoke/staging runs.",
  type = "character",
  default = "3_output"
)
parser$add_argument(
  "--mid",
  help = "base intermediate folder; defaults to 2_mid. Override for smoke/staging runs.",
  type = "character",
  default = "2_mid"
)
```

- [x] **Step 2: Use the args when constructing output dirs**

Replace the hardcoded roots at `config/config_weekly.R:144-145`:

```r
# output dirs — base roots overridable via --output / --mid (default 3_output / 2_mid)
dir_base_output <- args$output
dir_base_mid    <- args$mid
dir_mid    <- file.path(dir_base_mid,    year_filter, paste0("w", week_filter))
dir_output <- file.path(dir_base_output, year_filter, paste0("w", week_filter))
dir_plots  <- file.path(dir_output, "plots")
```

- [x] **Step 3: Verify the override creates dirs under the smokedir**

Run:
```bash
Rscript config/config_weekly.R --week 33 --year 2025 --output /tmp/smoke_chk_out --mid /tmp/smoke_chk_mid --download F --update F --push F >/dev/null 2>&1; ls -d /tmp/smoke_chk_out/2025/w33 /tmp/smoke_chk_out/2025/w33/plots
```
Expected: both `/tmp/smoke_chk_out/2025/w33` and its `plots/` subdir exist; nothing was written under the live `3_output`. Clean up: `rm -rf /tmp/smoke_chk_out /tmp/smoke_chk_mid`.

- [x] **Step 4: Commit**

```bash
git add config/config_weekly.R
git commit -m "config: add --output/--mid base-dir overrides for isolated smoke runs"
```

### Task G2: Point smoke_test at the smokedir + assert report and historical plot

- [x] **Step 1: Add isolated-dir constants + thread overrides into the config call**

In `tests/smoke_test.R`, after the existing constants block (lines 26-29), add:

```r
# ---- Isolated smoke output (keeps live 3_output / 2_mid untouched) ----
SMOKE_OUT_ROOT <- here("tests", "smoke_out")
SMOKE_MID_ROOT <- here("tests", "smoke_mid")
```

Change the derived output path (line 37) to read from the smokedir:

```r
smoke_outdir   <- file.path(SMOKE_OUT_ROOT, SMOKE_YEAR, paste0("w", SMOKE_WEEK))
```

Update the config command (lines 91-94) to pass the overrides:

```r
config_cmd <- sprintf(
  "Rscript %s --week %d --year %d --output %s --mid %s --download F --update F --push F",
  shQuote(config_script), SMOKE_WEEK, SMOKE_YEAR,
  shQuote(SMOKE_OUT_ROOT), shQuote(SMOKE_MID_ROOT)
)
```

- [x] **Step 2: Add the generated report + historical plot to the pass criteria**

Replace the `expected_files` block (lines 44-52). The historical plot file is `<dir_plots>/<file_prefix>hx_plot_all.png`, i.e. `plots/y{YEAR}_w{WEEK}_hx_plot_all.png`:

```r
file_prefix <- sprintf("y%d_w%d_", SMOKE_YEAR, SMOKE_WEEK)
expected_files <- c(
  # generated report (xlsx)
  file.path(smoke_outdir, paste0(file_prefix, "weekly_report_output.xlsx")),
  file.path(smoke_outdir, "table1a.csv"),
  file.path(smoke_outdir, "table1b_hx_vi.csv"),
  file.path(smoke_outdir, "table2a.csv"),
  file.path(smoke_outdir, "table2b_hx_abund.csv"),
  file.path(smoke_outdir, "table3a.csv"),
  file.path(smoke_outdir, "table3b_hx_pir.csv"),
  # historical plot
  file.path(smoke_outdir, "plots", paste0(file_prefix, "hx_plot_all.png"))
)
```

> The xlsx name in the original (line 45) used `paste0("y", SMOKE_YEAR, "_w", SMOKE_WEEK, "_weekly_report_output.xlsx")` — confirm `fn_report` in config still resolves to `y{YEAR}_w{WEEK}_weekly_report_output` so the filename matches.

- [x] **Step 3: Assert the generated report contains the `graphs` sheet**

Add a check in COMPARE mode, after the VI comparison block (after line 184), so "generated report" means the pixel-faithful workbook, not just any xlsx:

```r
# ---- Step 5c: report must contain the graphs sheet ----
report_path <- file.path(smoke_outdir, paste0(file_prefix, "weekly_report_output.xlsx"))
if (file.exists(report_path)) {
  sheets <- openxlsx::getSheetNames(report_path)
  if ("graphs" %in% sheets) {
    log_pass("report contains graphs sheet")
  } else {
    log_fail("report missing graphs sheet")
    failures <- c(failures, "report missing graphs sheet")
  }
}
```

Add `library(openxlsx)` to the `suppressPackageStartupMessages` block (lines 20-24).

- [x] **Step 4: Re-capture the golden reference into the smokedir, then run**

Because the output path changed, run setup once to (re)render into the smokedir and refresh the golden file, then run the comparison:

```bash
Rscript tests/smoke_test.R --setup
Rscript tests/smoke_test.R
```
Expected: setup completes; the second run prints `SMOKE TEST PASSED` with `[PASS]` lines for the xlsx, all CSVs, `*_hx_plot_all.png`, the VI columns, and "report contains graphs sheet".

- [x] **Step 5: Ignore the smokedir in git**

Add to `.gitignore` (the smoke outputs are regenerated, not committed):

```gitignore
# Smoke-test isolated outputs
tests/smoke_out/
tests/smoke_mid/
```

- [x] **Step 6: Commit**

```bash
git add tests/smoke_test.R .gitignore
git commit -m "test: smoke_test renders to isolated smokedir; assert report + historical plot"
```

---

## Phase F — Full verification

- [x] **Step 1: Run the full unit-test suite**

Run: `Rscript tests/run_tests.R`
Expected: all tests pass, including the new `test-build_tables`, `test-generate_report`, `test-bird_report`, `test-plot_hx`.

- [x] **Step 2: Render the pipeline end-to-end via the isolated smoke test** (per CLAUDE.md: always run the pipeline after refactor)

> Use 2025 w33 — it has populated input data. **Do NOT use 2026 w24 (no data yet).** This runs the full pipeline into the isolated smokedir built in Phase G.

Run: `Rscript tests/smoke_test.R`
Expected: `SMOKE TEST PASSED` — the report xlsx (with `graphs` sheet), all table CSVs, and `hx_plot_all.png` exist under the smokedir, and VI values match the golden reference.

- [x] **Step 3: Visually verify the graphs sheet is pixel-faithful**

Run:
```bash
Rscript -e 'd <- file.path("tests","smoke_out","2025","w33"); f <- list.files(d, pattern="report_output.xlsx$", full.names=TRUE)[1]; cat("file:", f, "\n"); cat("sheets:", paste(openxlsx::getSheetNames(f), collapse=", "), "\n"); g <- openxlsx::read.xlsx(f, sheet="graphs", colNames=FALSE, skipEmptyRows=FALSE); cat("graphs dims:", paste(dim(g), collapse="x"), "\n"); cat("B4 (week):", g[4,2], "| 1A first value cell C6:", g[6,3], "\n")'
```
Expected: `graphs` present, dims ~100×15, `B4` reads "Week: 33", and data appears at the configured anchors (C6 etc.). Open the file in Excel once to confirm merged headers/styling survived and data lands under the correct headers.

- [x] **Step 4: Confirm the new plots were written to the smokedir**

Run: `find tests/smoke_out/2025/w33 -name "*.png"`
Expected: `*_birds.png`, `*_trap_status.png`, `*_hx_plot_all.png` present.

- [x] **Step 5: Update docs**

Update `docs/TODO.md` to check off the completed pipeline items, and add the new functions to `docs/claude/functions.md`. Note in `CLAUDE.md` that `0_R/tables.R` and `0_R/generate_report.R` are now superseded by `utils/fun_build_tables.R` / `utils/fun_generate_report.R`.

- [x] **Step 6: Final commit**

```bash
git add docs/TODO.md docs/claude/functions.md CLAUDE.md
git commit -m "docs: mark pipeline TODOs complete; document new report/bird/plot functions"
```

---

## Notes & Open Decisions

- **Legacy `0_R/` files** are left in place (not deleted) — deleting them is a separate architecture-TODO item. The QMD no longer sources them after this plan.
- **Graph column-count verification (B3):** the only real risk in the pixel-faithful sheet is a mismatch between a dataset's value-column count and the template's header width for that block. The verification chunk (Phase F Step 3) plus a one-time Excel open catches this; adjust the dataset column selection (not the template) if a block is off.
- **`trap_status` dependency (E1):** flagged as a STOP gate — if the weekly cleaned data lacks `trap_status`, surface to the user rather than silently importing combiner logic.
- **Excluded as already-done:** googleAuth email argument (`config/load_packages.R`); the trap-status plot function itself (exists in `wnvSurv`).
- **Smokedir golden reference (G2):** moving smoke output to `tests/smoke_out/` does not move the golden file — it stays at `tests/fixtures/expected/table1a_w33_2025.csv` (committed). The `--setup` re-run in G2 Step 4 only refreshes the comparison baseline because the render path changed; the VI numbers should be unchanged from the prior golden, so diff it before committing to catch any unintended numeric drift introduced by the report refactor.
- **Smoke isolation scope:** only `--output`/`--mid` are redirected; inputs still read from the live `1_input/2025/w33`. This is intentional — the smoke test validates the pipeline against real input data without writing into live output trees.
