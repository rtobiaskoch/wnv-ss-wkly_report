#!/usr/bin/env Rscript
# scripts/patch_pcr_quantity.R
# -----------------------------------------------------------------------------
# ONE-TIME remediation tool. Run by hand for a week whose QuantStudio export was
# saved without its quantification step (Quantity / Slope / Y-Intercept all NA,
# standards not marked, generic Sample Names) — which makes clean_pcr() set every
# `copies` to 0 and silently zeroes the whole report.
#
# It rebuilds the standard curve from the PLATEMAP's known copies (wnv_std_1e6 ->
# 1e6, independent of QuantStudio) + the wells' Cq, back-calculates Quantity for
# every well via wnvSurv::predict_copies(), writes a CORRECTED copy of the export
# (.xlsx, same layout) into the pcr/ folder, and ARCHIVES the raw .xls to a
# sibling `pcr_raw_uncorrected/` dir so the pipeline reads the corrected file with
# NO qmd changes. Fully reversible (move the raw back, delete the corrected file).
#
# Usage:
#   Rscript scripts/patch_pcr_quantity.R --week 23 --year 2026
#   Rscript scripts/patch_pcr_quantity.R --week 23 --year 2026 --dry-run
#
# QC policy (decided in design): with only 3 standard points per target the curve
# is thin, so we WARN loudly when R2 < 0.98 or efficiency is outside 90-110% but
# STILL WRITE — the user stays in control.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readxl)
  library(writexl)
  library(tibble)
  library(wnvSurv)        # parse_std_copies(), fit_std_curve(), predict_copies()
})

# QC thresholds (curve acceptability; warn-but-write)
R2_MIN  <- 0.98
EFF_LO  <- 0.90
EFF_HI  <- 1.10
UNDET   <- 55.55          # no-amplification Cq sentinel (mirrors clean_pcr)

# ---------------------------------------------------------------------------
# extract_plate: "plate_N" token from a filename (matches clean_pcr/clean_platemap)
# ---------------------------------------------------------------------------
extract_plate <- function(filename) {
  n <- str_extract(basename(filename), "(?<=p)\\d+")
  if (is.na(n)) NA_character_ else paste0("plate_", n)
}

# ---------------------------------------------------------------------------
# grid_to_wells: parse the raw QuantStudio `Results` grid (read with col_names =
# FALSE, all text) into the per-well data rows we need. Column positions are
# DETECTED from the header row (the row whose first cell == "Well"), not hardcoded.
# Returns a list describing the grid so the caller can write back in place.
# ---------------------------------------------------------------------------
grid_to_wells <- function(grid, undet_val = UNDET) {
  col1 <- as.character(grid[[1]])
  hdr_row <- which(col1 == "Well")[1]
  if (is.na(hdr_row)) stop("Could not find the 'Well' header row in the grid.")

  hdr <- as.character(unlist(grid[hdr_row, ]))
  ix <- function(name) {
    j <- which(hdr == name)[1]
    if (is.na(j)) stop("Column '", name, "' not found in Results header.")
    j
  }
  cols <- list(well = ix("Well Position"), target = ix("Target Name"),
               ct = ix("CT"), qty = ix("Quantity"))

  data_rows <- which(col1 %in% as.character(1:96))
  data_rows <- data_rows[data_rows > hdr_row]

  ct_raw <- as.character(unlist(grid[data_rows, cols$ct]))
  cq <- ifelse(str_detect(ct_raw, regex("undetermined", ignore_case = TRUE)),
               undet_val, suppressWarnings(round(as.numeric(ct_raw), 2)))

  wells <- tibble::tibble(
    grid_row      = data_rows,
    well_position = as.character(unlist(grid[data_rows, cols$well])),
    target        = toupper(as.character(unlist(grid[data_rows, cols$target]))),
    cq            = cq
  )
  list(cols = cols, wells = wells)
}

# ---------------------------------------------------------------------------
# compute_quantity_grid: the CORE transform (no file I/O — unit-testable).
# Given the raw grid + cleaned platemap + plate id, fit per-target standard
# curves and write back the Quantity column. Returns the patched grid + QC.
# ---------------------------------------------------------------------------
compute_quantity_grid <- function(grid, platemap_clean, plate, undet_val = UNDET,
                                  quiet = FALSE) {
  say  <- function(...) if (!quiet) message(...)
  warn <- function(...) warning(..., call. = FALSE, immediate. = TRUE)

  parsed <- grid_to_wells(grid, undet_val)
  wells  <- parsed$wells

  # platemap for THIS plate: well -> csu_id, sample_type (where std identity lives)
  pm <- platemap_clean %>%
    dplyr::filter(plate == !!plate) %>%
    dplyr::select(well_position, csu_id, sample_type) %>%
    dplyr::distinct()

  wells <- wells %>%
    dplyr::left_join(pm, by = "well_position") %>%
    dplyr::mutate(
      known_copies = parse_std_copies(sample_type),
      virus        = str_to_upper(str_extract(csu_id, "^[^_]*"))  # wnv_std_1e6 -> WNV
    )

  # standard points: a std well, matched to its OWN virus, that actually amplified
  std <- wells %>%
    dplyr::filter(!is.na(known_copies), virus == target, cq < undet_val) %>%
    dplyr::mutate(log10_copies = log10(known_copies))

  if (nrow(std) == 0) {
    stop("No usable standard wells found for plate '", plate,
         "'. Check the platemap labels (expected csu_id like 'wnv_std_1e6').")
  }

  # fit one curve per target + QC table
  qc <- std %>%
    dplyr::group_by(target) %>%
    dplyr::group_modify(~ {
      f <- fit_std_curve(.x$cq, .x$log10_copies)
      f$cq_min <- min(.x$cq); f$cq_max <- max(.x$cq)
      f
    }) %>%
    dplyr::ungroup()

  say("\n=== standard-curve QC (plate ", plate, ") ===")
  for (i in seq_len(nrow(qc))) {
    q <- qc[i, ]
    say(sprintf("  %-5s slope=%.3f  efficiency=%.1f%%  R2=%.4f  n=%d  (Cq %.2f-%.2f)",
                q$target, q$slope, 100 * q$efficiency, q$r2, q$n_points,
                q$cq_min, q$cq_max))
    if (isTRUE(q$r2 < R2_MIN))
      warn(sprintf("  [%s] R2 %.4f < %.2f — curve is a poor fit; copies may be unreliable.",
                   q$target, q$r2, R2_MIN))
    if (isTRUE(q$efficiency < EFF_LO || q$efficiency > EFF_HI))
      warn(sprintf("  [%s] efficiency %.1f%% outside %g-%g%% — copies may be biased.",
                   q$target, 100 * q$efficiency, EFF_LO * 100, EFF_HI * 100))
  }

  # predict copies for every well: 0 where no amp; curve elsewhere
  pred <- wells %>%
    dplyr::left_join(dplyr::select(qc, target, slope, intercept, cq_min, cq_max),
                     by = "target") %>%
    dplyr::mutate(
      quantity = dplyr::case_when(
        cq >= undet_val          ~ 0,                         # no amplification
        is.na(slope)             ~ NA_real_,                  # no curve for target
        TRUE                     ~ predict_copies(cq, slope, intercept)
      ),
      extrapolated = !is.na(slope) & cq < undet_val & (cq < cq_min | cq > cq_max)
    )

  n_no_curve <- sum(is.na(pred$quantity))
  if (n_no_curve > 0)
    warn(sprintf("  %d well(s) have a target with no standard curve — Quantity left NA.",
                 n_no_curve))
  n_extrap <- sum(pred$extrapolated, na.rm = TRUE)
  if (n_extrap > 0)
    warn(sprintf("  %d amplified well(s) have Cq outside the standard range (extrapolated).",
                 n_extrap))

  # write Quantity back into the preserved grid (positional, all-text)
  grid[parsed$wells$grid_row, parsed$cols$qty] <-
    ifelse(is.na(pred$quantity), NA_character_, format(pred$quantity, scientific = FALSE,
                                                       trim = TRUE))

  list(grid = grid, qc = qc, n_standards = nrow(std),
       n_extrapolated = n_extrap, n_no_curve = n_no_curve)
}

# ---------------------------------------------------------------------------
# patch_pcr_file: file-level wrapper — read, compute, write corrected .xlsx,
# archive the raw .xls. Returns the written + archived paths.
# ---------------------------------------------------------------------------
patch_pcr_file <- function(pcr_file, platemap_clean, dir_archive,
                           dry_run = FALSE, quiet = FALSE) {
  plate <- extract_plate(pcr_file)
  grid <- readxl::read_excel(pcr_file, sheet = "Results", col_names = FALSE,
                             col_types = "text", .name_repair = "minimal")

  out <- compute_quantity_grid(grid, platemap_clean, plate, quiet = quiet)

  # corrected name: keep y####_w##_p# tokens; .xlsx (.xls is not writable here)
  corrected <- file.path(dirname(pcr_file),
                         paste0(tools::file_path_sans_ext(basename(pcr_file)), ".xlsx"))
  archived <- file.path(dir_archive, basename(pcr_file))

  if (file.exists(archived))
    stop("Archive already holds '", basename(pcr_file), "' — this looks like a ",
         "re-run. Undo first (move it back from ", dir_archive, ").")

  if (dry_run) {
    message("  [dry-run] would write: ", corrected)
    message("  [dry-run] would archive raw to: ", archived)
    return(invisible(list(corrected = corrected, archived = archived, dry_run = TRUE)))
  }

  if (!dir.exists(dir_archive)) dir.create(dir_archive, recursive = TRUE)
  writexl::write_xlsx(stats::setNames(list(out$grid), "Results"),
                      path = corrected, col_names = FALSE)
  ok <- file.rename(pcr_file, archived)
  if (!ok) stop("Failed to archive raw file to ", archived)

  message("  wrote corrected: ", corrected)
  message("  archived raw   : ", archived, "  (move back to undo)")
  invisible(list(corrected = corrected, archived = archived,
                 qc = out$qc, n_standards = out$n_standards))
}

# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
main <- function() {
  op <- optparse::OptionParser()
  op <- optparse::add_option(op, "--week", type = "integer", help = "epiweek number")
  op <- optparse::add_option(op, "--year", type = "integer", help = "year (YYYY)")
  op <- optparse::add_option(op, "--input", type = "character", default = "1_input",
                             help = "input root [default %default]")
  op <- optparse::add_option(op, "--dry-run", action = "store_true", default = FALSE,
                             dest = "dry_run", help = "report actions without writing")
  opt <- optparse::parse_args(op)
  if (is.null(opt$week) || is.null(opt$year))
    stop("Both --week and --year are required.", call. = FALSE)

  dir_wk       <- file.path(opt$input, opt$year, paste0("w", opt$week))
  dir_pcr      <- file.path(dir_wk, "pcr")
  dir_platemap <- file.path(dir_wk, "platemap")
  dir_archive  <- file.path(dir_wk, "pcr_raw_uncorrected")  # SIBLING of pcr/ (not
  # a subdir: the pipeline's list.files(dir_pcr) is non-recursive and would
  # otherwise hand the archive directory to read_pcr and crash).

  if (!dir.exists(dir_pcr)) stop("No pcr dir: ", dir_pcr, call. = FALSE)

  # report-local readers (sourced relative to the repo root we are run from)
  source(file.path("utils", "fun_read_platemap.R"))
  source(file.path("utils", "fun_clean_platemap.R"))

  raw_files <- list.files(dir_pcr, pattern = "\\.xls$", full.names = TRUE,
                          ignore.case = TRUE)
  if (length(raw_files) == 0)
    stop("No raw .xls export found in ", dir_pcr,
         " (already corrected? corrected files are .xlsx).", call. = FALSE)

  pm_files <- list.files(dir_platemap, pattern = "\\.xlsx$", full.names = TRUE,
                         ignore.case = TRUE)
  if (length(pm_files) == 0) stop("No platemap .xlsx in ", dir_platemap, call. = FALSE)
  platemap_clean <- clean_platemap(read_platemap(pm_files))

  message("Patching ", length(raw_files), " raw export(s) for ", opt$year,
          " w", opt$week, if (opt$dry_run) "  [DRY RUN]" else "")
  for (f in raw_files) {
    message("\n- ", basename(f))
    patch_pcr_file(f, platemap_clean, dir_archive, dry_run = opt$dry_run)
  }
  message("\nDone. Now run: quarto render wnv-s_weekly_report_pipeline_v2.qmd",
          " (use --download F config so the GSheet pull does not re-fetch).")
}

# Run only when invoked via Rscript; stay silent when source()'d by tests.
if (sys.nframe() == 0L) main()
