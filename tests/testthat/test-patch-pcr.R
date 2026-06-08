# test-patch-pcr.R
# Tests for the one-time PCR Quantity remediation transform
# (scripts/patch_pcr_quantity.R) — the fallback that rebuilds the standard curve
# from platemap-known copies when a QuantStudio export shipped with Quantity blank.
#
# Scientific intent:
#   - A standard well's KNOWN copies come from the platemap label (wnv_std_1e6 ->
#     1e6), independent of the instrument's Quantity export.
#   - Each standard is matched to its OWN virus (csu_id prefix == Target Name) so a
#     WNV standard read on the SLEV target does not pollute the SLEV curve.
#   - Cq == 55.55 (no amplification) maps to 0 copies, never the curve.

# Load the script's transform functions (main() does not run when source()'d).
# Requires the freshly-installed wnvSurv (parse_std_copies/fit_std_curve/predict_copies).
source(here::here("scripts/patch_pcr_quantity.R"))

# Skip the whole file gracefully if wnvSurv hasn't been reinstalled with the
# std_curve functions yet (the feature requires that reinstall + an R restart).
skip_if_no_std_curve <- function() {
  testthat::skip_if_not(
    exists("parse_std_copies", mode = "function"),
    "wnvSurv std_curve functions not available — reinstall wnvSurv and restart R."
  )
}

# ---- synthetic raw QuantStudio `Results` grid (all text, Quantity blank) ------
# Columns: Well | Well Position | Target Name | CT | Quantity
make_grid <- function() {
  m_wnv <- -3.32; b_wnv <- 40        # perfect WNV curve
  m_slev <- -3.40; b_slev <- 41      # perfect SLEV curve
  cq <- function(m, b, logc) sprintf("%.2f", b + m * logc)

  header   <- c("Well", "Well Position", "Target Name", "CT", "Quantity")
  preamble <- c("Block Type", "384-Well", NA, NA, NA)

  # well, well_position, target, CT, quantity(blank)
  rows <- list(
    c("1", "A1", "WNV",  cq(m_wnv, b_wnv, 2), NA),   # wnv_std_1e2
    c("2", "A2", "WNV",  cq(m_wnv, b_wnv, 4), NA),   # wnv_std_1e4
    c("3", "A3", "WNV",  cq(m_wnv, b_wnv, 6), NA),   # wnv_std_1e6
    c("4", "A1", "SLEV", "Undetermined",      NA),   # cross-target: WNV std on SLEV
    c("5", "B1", "SLEV", cq(m_slev, b_slev, 2), NA), # slev_std_1e2
    c("6", "B2", "SLEV", cq(m_slev, b_slev, 4), NA), # slev_std_1e4
    c("7", "B3", "SLEV", cq(m_slev, b_slev, 6), NA), # slev_std_1e6
    c("8", "C1", "WNV",  "28.00",             NA),   # unknown, amplifies
    c("9", "C2", "WNV",  "Undetermined",      NA)    # unknown, no amp
  )
  g <- rbind(preamble, header, do.call(rbind, rows))
  as.data.frame(g, stringsAsFactors = FALSE)
}

make_platemap <- function() {
  tibble::tibble(
    well_position = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2"),
    csu_id        = c("wnv_std_1e2", "wnv_std_1e4", "wnv_std_1e6",
                      "slev_std_1e2", "slev_std_1e4", "slev_std_1e6",
                      "CSU0001", "CSU0002"),
    sample_type   = c("std 1e2", "std 1e4", "std 1e6",
                      "std 1e2", "std 1e4", "std 1e6", "mozzy", "mozzy"),
    plate         = "plate_1"
  )
}

# read the Quantity column out of a patched grid, keyed by well + target
patched_quantity <- function(grid) {
  col1 <- as.character(grid[[1]])
  hdr <- which(col1 == "Well")[1]
  data_rows <- which(col1 %in% as.character(1:96)); data_rows <- data_rows[data_rows > hdr]
  tibble::tibble(
    well_position = as.character(grid[data_rows, 2]),
    target        = as.character(grid[data_rows, 3]),
    quantity      = suppressWarnings(as.numeric(as.character(grid[data_rows, 5])))
  )
}

test_that("extract_plate pulls the plate token from a filename", {
  skip_if_no_std_curve()
  expect_equal(extract_plate("ss_y2026_w23_p1_pcr.xls"), "plate_1")
  expect_equal(extract_plate("SS-y2025-w35-p2-pcr.xls"), "plate_2")
})

test_that("standards' wells recover their known copies from the rebuilt curve", {
  skip_if_no_std_curve()
  out <- compute_quantity_grid(make_grid(), make_platemap(), plate = "plate_1",
                               quiet = TRUE)
  q <- patched_quantity(out$grid)

  expect_equal(out$n_standards, 6)   # 3 WNV + 3 SLEV (cross-target row excluded)

  get <- function(wp, tg) q$quantity[q$well_position == wp & q$target == tg]
  expect_equal(get("A1", "WNV"), 1e2, tolerance = 1e-2)
  expect_equal(get("A3", "WNV"), 1e6, tolerance = 1e-2)
  expect_equal(get("B3", "SLEV"), 1e6, tolerance = 1e-2)
})

test_that("no-amplification wells map to 0 copies, amplified unknowns are positive", {
  skip_if_no_std_curve()
  out <- compute_quantity_grid(make_grid(), make_platemap(), plate = "plate_1",
                               quiet = TRUE)
  q <- patched_quantity(out$grid)

  expect_equal(q$quantity[q$well_position == "C2"], 0)          # Undetermined -> 0
  expect_gt(q$quantity[q$well_position == "C1"], 0)             # CT 28 -> positive
  # WNV curve (slope -3.32, intercept 40): copies(28) = 10^((28-40)/-3.32)
  expect_equal(q$quantity[q$well_position == "C1"],
               10^((28 - 40) / -3.32), tolerance = 1e-2)
})

test_that("QC reports a clean curve for the synthetic standards", {
  skip_if_no_std_curve()
  out <- compute_quantity_grid(make_grid(), make_platemap(), plate = "plate_1",
                               quiet = TRUE)
  wnv <- out$qc[out$qc$target == "WNV", ]
  expect_equal(wnv$slope, -3.32, tolerance = 1e-6)
  expect_equal(wnv$r2, 1, tolerance = 1e-8)
})

test_that("a plate with no standards errors clearly", {
  skip_if_no_std_curve()
  pm_no_std <- make_platemap()
  pm_no_std$sample_type <- "mozzy"; pm_no_std$csu_id <- "CSU9999"
  expect_error(
    compute_quantity_grid(make_grid(), pm_no_std, plate = "plate_1", quiet = TRUE),
    "No usable standard wells"
  )
})
