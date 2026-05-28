# Plan: End-to-End Pipeline Smoke Test + Safety Guard

## Context

The pipeline has unit tests for three calculation functions but no way to verify the full Quarto pipeline renders correctly end-to-end. Two gaps need to be filled:

1. **No end-to-end test** — the pipeline could crash or produce wrong values after a code change, and nothing would catch it
2. **No write guard on `update_gsheet()`** — the only thing preventing a real database write when `update=F` is the `if(update)` block in the QMD; if that block were ever accidentally removed, `update_gsheet()` would write silently to production Google Sheets

---

## Part 1 — `update_gsheet()` Safety Guard

### What to build

Add a defensive early-return at the top of `utils/fun_update_gsheet.R` that stops with a clear error if the function is ever called when `update` is not `TRUE` in the calling environment.

```r
# In update_gsheet(), first line of the function body:
if (!isTRUE(get("update", envir = parent.env(environment()), inherits = TRUE))) {
  stop(
    "update_gsheet() was called but `update` is FALSE or not set. ",
    "All calls to update_gsheet() must be inside an `if(update)` block."
  )
}
```

This makes the function self-protecting: even if the `if(update)` guard in the QMD were accidentally removed, `update_gsheet()` would error loudly before touching any GSheet.

### Unit test to add in `tests/testthat/test-update_gsheet.R`

```r
# Verify the guard triggers when update=FALSE
test_that("update_gsheet() errors when update is FALSE", {
  update <- FALSE  # simulate update=F config
  expect_error(
    update_gsheet(new = tibble::tibble(), by = "id", fn_save = tempfile(),
                  gkey = "fake", gfolder = "fake", gname = "fake"),
    regexp = "update.*FALSE"
  )
})
```

---

## Part 2 — Smoke Test with Value Assertions

### Strategy

- Run the pipeline against **week 33 / 2025** (`1_input/w33/` already populated, outputs known-good from prior real run)
- Store a golden copy of `table1a.csv` at `tests/fixtures/expected/table1a_w33_2025.csv` (captured before first smoke test run)
- After rendering, compare actual `table1a.csv` against the golden file, zone-by-zone, for `vi_Pipiens`, `vi_Tarsalis`, and `vi_All` using a 1% relative tolerance (handles floating-point rounding in the CSV serialization)
- Also check file existence for all 7 expected outputs

### Files to create

#### `tests/fixtures/expected/table1a_w28_2025.csv`

Captured from the current (known-good) `3_output/2025/table1a.csv`. Contains columns: `zone`, `abund_Pipiens`, `abund_Tarsalis`, `pir_Pipiens`, `pir_Tarsalis`, `vi_Pipiens`, `vi_Tarsalis`, `vi_All`.

This file is committed to git and serves as the ground-truth reference. It only changes when the canonical test week changes.

#### `tests/smoke_test.R`

```r
# Smoke test: renders the full pipeline against week 28/2025 and asserts
# (1) all expected output files exist and (2) VI values match golden reference.
#
# Usage:   Rscript tests/smoke_test.R
# Returns: exit code 0 (pass) or 1 (fail)
#
# Prerequisites:
#   - 1_input/w28/ must be populated (has been since 2025 season start)
#   - quarto must be on PATH
#   - tests/fixtures/expected/table1a_w28_2025.csv must exist (committed to git)
#
# Note: overwrites 3_output/2025/ outputs for w28 — that is expected;
# a passing run produces identical values to the prior known-good run.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
})

# ---- Constants ----
SMOKE_WEEK  <- 33
SMOKE_YEAR  <- 2025
VI_TOL      <- 0.01  # 1% relative tolerance for floating-point VI comparisons

# ---- Derived paths ----
smoke_input    <- here("1_input", paste0("w", SMOKE_WEEK))
smoke_outdir   <- here("3_output", SMOKE_YEAR)
golden_vi_path <- here("tests", "fixtures", "expected", "table1a_w33_2025.csv")
config_script  <- here("config", "config_weekly.R")
qmd_path       <- here("wnv-s_weekly_report_pipeline_v2.qmd")

# ---- Expected output files ----
expected_files <- c(
  file.path(smoke_outdir, paste0("y", SMOKE_YEAR, "_w", SMOKE_WEEK, "_weekly_report_output.xlsx")),
  file.path(smoke_outdir, "table1a.csv"),
  file.path(smoke_outdir, "table1b_hx_vi.csv"),
  file.path(smoke_outdir, "table2a.csv"),
  file.path(smoke_outdir, "table2b_hx_abund.csv"),
  file.path(smoke_outdir, "table3a.csv"),
  file.path(smoke_outdir, "table3b_hx_pir.csv")
)

# ---- Helpers ----
pass <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail <- function(msg) { cat(sprintf("  [FAIL] %s\n", msg)); FALSE }

cat("=== WNV Pipeline Smoke Test ===\n")
cat(sprintf("  Week: %d | Year: %d | VI tolerance: %.0f%%\n\n", SMOKE_WEEK, SMOKE_YEAR, VI_TOL * 100))

failures <- character(0)

# ---- Step 1: Prerequisites ----
cat("[1/5] Checking prerequisites...\n")
if (!dir.exists(smoke_input))   stop("Missing input dir: ", smoke_input)
if (!file.exists(config_script)) stop("Missing: ", config_script)
if (!file.exists(qmd_path))     stop("Missing: ", qmd_path)
if (!file.exists(golden_vi_path)) stop("Missing golden reference: ", golden_vi_path,
                                        "\nRun the pipeline for w33/2025, then copy 3_output/2025/table1a.csv to that path.")
if (nchar(Sys.which("quarto")) == 0) stop("quarto not on PATH.")
cat("  Prerequisites OK.\n\n")

# ---- Step 2: Generate config ----
cat("[2/5] Generating config...\n")
cmd <- sprintf("Rscript %s --input %s --week %d --year %d --download F --update F --push F",
               config_script, smoke_input, SMOKE_WEEK, SMOKE_YEAR)
if (system(cmd) != 0) stop("Config generation failed.")
cat("  Config generated.\n\n")

# ---- Step 3: Render pipeline ----
cat("[3/5] Rendering pipeline (takes a few minutes)...\n")
t0 <- proc.time()[["elapsed"]]
if (system(paste("quarto render", shQuote(qmd_path))) != 0) stop("Pipeline render failed.")
cat(sprintf("  Rendered in %.1f seconds.\n\n", proc.time()[["elapsed"]] - t0))

# ---- Step 4: Check output files exist ----
cat("[4/5] Checking output files...\n")
for (f in expected_files) {
  if (file.exists(f)) { pass(basename(f)) } else {
    fail(basename(f))
    failures <- c(failures, paste("Missing:", basename(f)))
  }
}
cat("\n")

# ---- Step 5: Check VI values against golden reference ----
cat("[5/5] Checking VI values against golden reference...\n")
actual   <- readr::read_csv(file.path(smoke_outdir, "table1a.csv"), show_col_types = FALSE)
expected <- readr::read_csv(golden_vi_path, show_col_types = FALSE)

vi_cols <- c("vi_Pipiens", "vi_Tarsalis", "vi_All")

# Join on zone; compare each VI column per zone
comparison <- dplyr::inner_join(
  actual   %>% dplyr::select(zone, dplyr::all_of(vi_cols)),
  expected %>% dplyr::select(zone, dplyr::all_of(vi_cols)),
  by = "zone", suffix = c("_actual", "_expected")
)

for (col in vi_cols) {
  act_col <- paste0(col, "_actual")
  exp_col <- paste0(col, "_expected")
  
  mismatches <- comparison %>%
    dplyr::filter(!is.na(.data[[act_col]]) | !is.na(.data[[exp_col]])) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      within_tol = dplyr::case_when(
        is.na(.data[[act_col]]) & is.na(.data[[exp_col]]) ~ TRUE,
        is.na(.data[[act_col]]) | is.na(.data[[exp_col]]) ~ FALSE,
        .data[[exp_col]] == 0 ~ abs(.data[[act_col]]) < 1e-9,
        TRUE ~ abs(.data[[act_col]] - .data[[exp_col]]) / abs(.data[[exp_col]]) <= VI_TOL
      )
    ) %>%
    dplyr::filter(!within_tol)
  
  if (nrow(mismatches) == 0) {
    pass(col)
  } else {
    msg <- sprintf("%s mismatch in zones: %s", col, paste(mismatches$zone, collapse = ", "))
    fail(msg)
    failures <- c(failures, msg)
  }
}
cat("\n")

# ---- Result ----
if (length(failures) == 0) {
  cat("SMOKE TEST PASSED\n")
  quit(status = 0)
} else {
  cat(sprintf("SMOKE TEST FAILED — %d issue(s):\n", length(failures)))
  purrr::walk(failures, ~ cat("  -", .x, "\n"))
  quit(status = 1)
}
```

---

## Execution Order

1. Add guard + unit test to `fun_update_gsheet.R` and `tests/testthat/test-update_gsheet.R`
2. Copy current `3_output/2025/table1a.csv` to `tests/fixtures/expected/table1a_w33_2025.csv` (once, then commit)
3. Create `tests/smoke_test.R`
4. Run: `Rscript tests/run_tests.R` → unit test for guard passes
5. Run: `Rscript tests/smoke_test.R` → full smoke test passes

---

## Verification

```bash
# Unit tests (includes update_gsheet guard test)
Rscript tests/run_tests.R

# Full smoke test (3–5 minutes)
Rscript tests/smoke_test.R
```

Expected smoke test output:
```
=== WNV Pipeline Smoke Test ===
  Week: 33 | Year: 2025 | VI tolerance: 1%

[1/5] Checking prerequisites...
  Prerequisites OK.
[2/5] Generating config...
  Config generated.
[3/5] Rendering pipeline (takes a few minutes)...
  Rendered in 94.2 seconds.
[4/5] Checking output files...
  [PASS] y2025_w33_weekly_report_output.xlsx
  [PASS] table1a.csv
  ...
[5/5] Checking VI values against golden reference...
  [PASS] vi_Pipiens
  [PASS] vi_Tarsalis
  [PASS] vi_All

SMOKE TEST PASSED
```
