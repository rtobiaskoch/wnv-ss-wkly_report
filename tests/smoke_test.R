# Smoke test: renders the full WNV pipeline against a known prior week and asserts
# (1) all expected output files are produced and (2) VI values match a golden reference.
#
# USAGE
#   First run (capture baseline):  Rscript tests/smoke_test.R --setup
#   Subsequent runs (compare):     Rscript tests/smoke_test.R
#
# --setup mode renders the pipeline for SMOKE_WEEK/SMOKE_YEAR, then saves the
# resulting table1a.csv as the golden VI reference. Run once, then commit the
# golden file. Only re-run --setup when you intentionally change expected outputs.
#
# RETURNS exit code 0 (pass) or 1 (fail) — suitable for CI.
#
# PREREQUISITES
#   - 1_input/{SMOKE_YEAR}/w{SMOKE_WEEK}/ must be populated
#   - quarto must be on PATH
#   - tests/fixtures/expected/table1a_w{SMOKE_WEEK}_{SMOKE_YEAR}.csv must exist
#     (created by --setup mode)

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
})

# ---- Constants (update when canonical test week changes) ----
SMOKE_WEEK <- 33
SMOKE_YEAR <- 2025
VI_TOL     <- 0.01  # 1% relative tolerance for floating-point VI comparisons

# ---- Parse args ----
args       <- commandArgs(trailingOnly = TRUE)
setup_mode <- "--setup" %in% args

# ---- Derived paths ----
smoke_input    <- here("1_input", SMOKE_YEAR, paste0("w", SMOKE_WEEK))
smoke_outdir   <- here("3_output", SMOKE_YEAR, paste0("w", SMOKE_WEEK))
golden_path    <- here("tests", "fixtures", "expected",
                       paste0("table1a_w", SMOKE_WEEK, "_", SMOKE_YEAR, ".csv"))
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
log_pass <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
log_fail <- function(msg) cat(sprintf("  [FAIL] %s\n", msg))

cat("=== WNV Pipeline Smoke Test ===\n")
cat(sprintf("  Week: %d | Year: %d | VI tolerance: %.0f%%\n",
            SMOKE_WEEK, SMOKE_YEAR, VI_TOL * 100))
if (setup_mode) cat("  Mode: SETUP (capturing golden reference)\n")
cat("\n")

failures <- character(0)

# ---- Step 1: Prerequisites ----
cat("[1/5] Checking prerequisites...\n")

if (!dir.exists(smoke_input)) {
  stop(sprintf(
    "Input directory not found: %s\n  Run: bash config/run_config.sh --week %d --year %d --download T --update F --push F",
    smoke_input, SMOKE_WEEK, SMOKE_YEAR
  ))
}
if (!file.exists(config_script)) stop("Missing: ", config_script)
if (!file.exists(qmd_path))      stop("Missing: ", qmd_path)
if (nchar(Sys.which("quarto")) == 0) stop("quarto not on PATH — install quarto CLI.")

if (!setup_mode && !file.exists(golden_path)) {
  stop(sprintf(
    "Golden reference not found: %s\n  Run once with --setup to capture baseline:\n  Rscript tests/smoke_test.R --setup",
    golden_path
  ))
}

cat("  Prerequisites OK.\n\n")

# ---- Step 2: Generate config ----
cat("[2/5] Generating config...\n")

config_cmd <- sprintf(
  "Rscript %s --week %d --year %d --download F --update F --push F",
  shQuote(config_script), SMOKE_WEEK, SMOKE_YEAR
)
if (system(config_cmd) != 0) stop("Config generation failed.")
cat("  Config generated.\n\n")

# ---- Step 3: Render pipeline ----
cat("[3/5] Rendering pipeline (this takes a few minutes)...\n")

t0 <- proc.time()[["elapsed"]]
render_result <- system(paste("quarto render", shQuote(qmd_path)))
elapsed <- round(proc.time()[["elapsed"]] - t0, 1)

if (render_result != 0) stop(sprintf("Pipeline render failed (exit code %d).", render_result))
cat(sprintf("  Rendered in %.1f seconds.\n\n", elapsed))

# ---- Step 4: Check output files ----
cat("[4/5] Checking output files...\n")

for (f in expected_files) {
  if (file.exists(f)) {
    log_pass(basename(f))
  } else {
    log_fail(basename(f))
    failures <- c(failures, paste("Missing output:", basename(f)))
  }
}
cat("\n")

# ---- Step 5a: SETUP MODE — save golden reference ----
if (setup_mode) {
  cat("[5/5] Saving golden VI reference...\n")

  actual_path <- file.path(smoke_outdir, "table1a.csv")
  if (!file.exists(actual_path)) stop("Cannot save golden reference: table1a.csv not found.")

  dir.create(dirname(golden_path), showWarnings = FALSE, recursive = TRUE)
  file.copy(actual_path, golden_path, overwrite = TRUE)
  cat(sprintf("  Saved: %s\n", golden_path))
  cat("\n  Commit this file to git, then run smoke tests without --setup.\n\n")

  if (length(failures) == 0) {
    cat("SETUP COMPLETE — golden reference captured.\n")
    quit(status = 0)
  } else {
    cat(sprintf("SETUP WARNING — %d output file(s) missing (see above).\n", length(failures)))
    quit(status = 1)
  }
}

# ---- Step 5b: COMPARE MODE — check VI values against golden reference ----
cat("[5/5] Checking VI values against golden reference...\n")

vi_cols <- c("vi_Pipiens", "vi_Tarsalis", "vi_All")

actual   <- readr::read_csv(file.path(smoke_outdir, "table1a.csv"),
                             show_col_types = FALSE)
expected <- readr::read_csv(golden_path, show_col_types = FALSE)

# Join on zone and compare each VI column with relative tolerance
comparison <- dplyr::inner_join(
  actual   |> dplyr::select(zone, dplyr::all_of(vi_cols)),
  expected |> dplyr::select(zone, dplyr::all_of(vi_cols)),
  by = "zone", suffix = c("_actual", "_expected")
)

for (col in vi_cols) {
  act_col <- paste0(col, "_actual")
  exp_col <- paste0(col, "_expected")

  mismatches <- comparison |>
    dplyr::filter(!is.na(.data[[act_col]]) | !is.na(.data[[exp_col]])) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      within_tol = dplyr::case_when(
        is.na(.data[[act_col]]) & is.na(.data[[exp_col]]) ~ TRUE,
        is.na(.data[[act_col]]) | is.na(.data[[exp_col]]) ~ FALSE,
        .data[[exp_col]] == 0 ~ abs(.data[[act_col]]) < 1e-9,
        TRUE ~ abs(.data[[act_col]] - .data[[exp_col]]) / abs(.data[[exp_col]]) <= VI_TOL
      )
    ) |>
    dplyr::filter(!within_tol)

  if (nrow(mismatches) == 0) {
    log_pass(col)
  } else {
    msg <- sprintf("%s mismatch in zones: %s", col,
                   paste(mismatches$zone, collapse = ", "))
    log_fail(msg)
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
  purrr::walk(failures, \(x) cat("  -", x, "\n"))
  quit(status = 1)
}
