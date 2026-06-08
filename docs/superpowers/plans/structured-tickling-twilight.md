# Plan — PCR copies fallback (recompute Quantity from the standard curve)

Design spec: `docs/superpowers/specs/2026-06-07-pcr-copies-fallback-design.md`

## Context

When a QuantStudio plate is exported before its quantification step is set up, the
`Results` sheet has `Quantity`/`Slope`/`Y-Intercept` all `NA` (observed on
`1_input/2026/w23/pcr/ss_y2026_w23_p1_pcr.xls`: 0/110 wells quantified, no
`STANDARD`-task wells, generic `Sample Name`, but CT values present). Because
`clean_pcr()` does `quantity = replace_na(quantity, 0)` → `copies = 0`, every well
gets `copies = 0`: the standards plot's `log_copies` are all zero and no sample can
be called positive — the whole weekly report is blocked.

The known copy numbers for the standards live in the **platemap**
(`csu_id = wnv_std_1e6` → `sample_type = "std 1e6"`), independent of QuantStudio, so
we can rebuild the standard curve ourselves and back-calculate copies. This is
absolute qPCR quantification: fit `Cq = m·log10(N₀) + b`, invert to
`N₀ = 10^((Cq − b)/m)`.

Decisions (user-approved): fallback-only manual tool; pure math in `wnvSurv`
(Approach A); report-local one-time script writes a corrected sidecar `.xlsx` and
archives the raw, pipeline unchanged; warn-but-write when curve QC is poor (only 3
standard points/target).

## Part 1 — wnvSurv pure functions (`wnv-ss_functions`)

New file `R/std_curve.R`, three exported, side-effect-free functions:

- `parse_std_copies(label)` — `"wnv_std_1e6" | "std 1e6" | "1e6"` → `1e6`;
  non-standard → `NA_real_`. Vectorized string→numeric (regex `1e<digits>` or
  `(?<=e)\d+` exponent), case-insensitive.
- `fit_std_curve(cq, log10_copies)` — `lm(cq ~ log10_copies)`; returns one-row
  tibble `slope, intercept, r2, efficiency, n_points` where
  `efficiency = 10^(-1/slope) - 1`. **Errors** if < 2 distinct `log10_copies`.
  Caller passes clean points (real cq, virus-matched).
- `predict_copies(cq, slope, intercept)` — `10^((cq - intercept)/slope)`. Pure,
  vectorized; no no-amp special-casing (that lives in the caller).

Tests `tests/testthat/test-std_curve.R`:
- `parse_std_copies`: the three dilutions, mixed case, non-standard → NA.
- `fit_std_curve`: recovers known slope/intercept from a synthetic perfect line
  (R² == 1); efficiency matches `10^(-1/m)-1`; `n_points` correct; errors on one
  copy level.
- `predict_copies`: round-trip — fit from known points, predict at the standards'
  Cq, recover input copies within tolerance.

Then `devtools::document()` + `devtools::test()` (user runs install via `! …`,
then restarts R — sandbox cannot build/install packages).

## Part 2 — report remediation script (`wnv-ss-wkly_report`)

New `scripts/patch_pcr_quantity.R`, args `--week`, `--year` (resolves
`1_input/<year>/w<week>/pcr` and `/platemap`, matching the pipeline path
convention). Reuses existing report functions: `read_platemap()`,
`clean_platemap()` (`utils/fun_clean_platemap.R`); reads the raw `.xls` via
`readxl::read_excel`.

Steps:
1. Read raw `.xls` `Results` sheet as a **full grid** (`col_names = FALSE`, all
   rows) to preserve QuantStudio's multi-block layout verbatim; record the
   `Quantity` column index and the data-row span (col 1 ∈ `1:96`).
2. `read_platemap(..., sheet="pcr", range="A1:M9", col_pattern="(?<=x)\\d+",
   val_to="csu_id")` then `clean_platemap()` → well → `csu_id`, `sample_type`.
3. Build standard set: keep wells where `str_extract(csu_id, "^[^_]*")` matches
   `Target Name`; `wnvSurv::parse_std_copies()` for known copies; drop Cq `55.55`.
4. Per `(plate, target)`: `wnvSurv::fit_std_curve()`; print QC table; **warn** (not
   stop) if `r2 < 0.98` or `efficiency ∉ [0.90, 1.10]`.
5. Compute `Quantity` for all wells via `wnvSurv::predict_copies()`; set `0` where
   Cq undetermined/no-amp; warn + list wells whose Cq is outside the standard range
   (extrapolation).
6. Overwrite **only** the `Quantity` cells in the preserved grid; write
   `dir_pcr/ss_y<year>_w<week>_p<plate>_pcr.xlsx` (`.xlsx`, name keeps
   `y####_w##_p#` tokens for `clean_pcr`'s regex). Use `writexl::write_xlsx` /
   `openxlsx` with `col_names = FALSE`.
7. Move raw `.xls` → `dir_pcr/uncorrected_raw/` (pipeline's non-recursive
   `list.files` ignores it). Idempotency guard: error if `uncorrected_raw/` already
   has the raw. Print summary + undo instructions.

Report test `tests/testthat/`: fixture with all-NA Quantity → patch logic emits a
file `read_pcr()` parses, with non-zero `copies` for amplified wells and `55.55`
wells at 0.

## Critical files

| File | Change |
|---|---|
| `wnv-ss_functions/R/std_curve.R` | new — 3 pure functions |
| `wnv-ss_functions/tests/testthat/test-std_curve.R` | new — unit tests |
| `wnv-ss-wkly_report/scripts/patch_pcr_quantity.R` | new — remediation script |
| `wnv-ss-wkly_report/tests/testthat/test-patch-pcr.R` | new — fixture test |
| `wnv-ss-wkly_report/utils/fun_clean_platemap.R`, `fun_read_pcr.R` | read-only reuse |

No edits to `wnv-s_weekly_report_pipeline_v2.qmd` or `clean_pcr`.

## Verification (end-to-end on 2026 w23)

1. wnvSurv: `devtools::test()` green; user installs + restarts R.
2. `Rscript scripts/patch_pcr_quantity.R --week 23 --year 2026` → prints QC table
   (expect WNV slope ≈ −3.23, E ≈ 104%; SLEV flagged noisy), writes corrected
   `.xlsx`, archives raw to `pcr/uncorrected_raw/`.
3. Confirm `list.files(dir_pcr)` shows only the corrected `.xlsx`; `read_pcr` +
   `clean_pcr` yield non-zero `copies`; standard wells' `log_copies` span ~2–6.
4. `quarto render wnv-s_weekly_report_pipeline_v2.qmd` (with `--download F` so the
   GSheet pull doesn't reintroduce issues) → standards plot shows a real curve.
5. Undo check: move raw back, delete corrected `.xlsx` → original state restored.

## Out of scope

Pipeline qmd / `clean_pcr` unchanged; no auto in-pipeline fallback; no historical
backfill. The separate `zone`-dropped bug (missing `key_rename.csv` rows) is tracked
independently.
```
