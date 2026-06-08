# Design — PCR copies fallback (recompute Quantity from the standard curve)

**Date:** 2026-06-07
**Status:** Approved (brainstorming) — pending spec review
**Repos touched:** `wnv-ss_functions` (wnvSurv, new pure functions + tests) and
`wnv-ss-wkly_report` (new one-time remediation script).

## Problem

When the QuantStudio plate is exported without its quantification step set up
(standards not marked `Task = STANDARD`, no known copy numbers entered, analysis
not re-run), the `Results` sheet has **`Quantity`, `Slope`, and `Y-Intercept` all
`NA`**. Observed on `1_input/2026/w23/pcr/ss_y2026_w23_p1_pcr.xls`: 0/110 wells have
a Quantity, no `STANDARD`-task wells, generic `Sample Name = "Sample 1"`, yet CT
values are present (the instrument ran).

Downstream, `clean_pcr()` does `quantity = replace_na(quantity, 0)` then
`copies = if_else(cq >= 55.55, 0, round(quantity, 2))`, so **every well gets
`copies = 0`** → standards plot `log_copies` all zero, and no sample can be called
positive. This blocks the entire weekly report.

This is an upstream data/QC failure, not a pipeline code bug. We want a way to
recover when it happens, without re-running the instrument.

## Goal

A `wnvSurv` "formula" (pure functions) that reconstructs the standard curve from
the standard wells' Cq + their **known** copy numbers — which live in the
**platemap**, independent of QuantStudio — and back-calculates `copies` for every
well. A repo-local one-time script wraps this to patch a single week's input.

Confirmed feasible: the platemap labels standards by `csu_id`
(`wnv_std_1e6` → `sample_type = "std 1e6"`), and after matching each standard to
its own virus the Cq values form usable curves (WNV: 1e2/1e4/1e6 → 32.43/25.63/19.51,
slope ≈ −3.23, E ≈ 104%; SLEV noisier).

## Decisions (from brainstorming)

1. **Fallback-only, manual.** Never auto-fires inside the pipeline. Run by hand
   for an affected week.
2. **Sidecar corrected file; raw archived.** Write a corrected file the pipeline
   reads in place of the raw; move the raw into `pcr/uncorrected_raw/` (preserved,
   reversible). Pipeline qmd is unchanged.
3. **Warn-but-write on bad QC.** With only 3 standard points per target, the script
   prints a QC table and warns loudly when R² < 0.98 or efficiency ∉ [90, 110]%,
   but still writes. The user stays in control.
4. **Math in wnvSurv (pure primitives); plumbing in the script** (Approach A). The
   package makes no assumptions about column names or plate layout.
5. **Script at `scripts/patch_pcr_quantity.R`.**

## Science

Absolute qPCR quantification. Fit `Cq = m·log10(N₀) + b` by OLS on the standard
wells; invert to predict `N₀ = 10^((Cq − b)/m)`.

- **Efficiency** `E = 10^(−1/m) − 1`. Ideal slope `m ≈ −3.32` (E = 100%);
  acceptable band ~`m ∈ [−3.58, −3.10]` (E 90–110%).
- **R²** of the fit (linearity), target > 0.98.

**Assumptions flagged explicitly:**
- Only **3 standard points** per target (1e2/1e4/1e6) → 1 residual d.f.; a single
  bad replicate cannot be detected statistically. This is the minimum defensible
  curve and the reason for warn-but-write rather than silent acceptance.
- Unknowns with Cq outside the standard range are **extrapolated**; the script
  flags those wells.
- `Cq = 55.55` (no-amp sentinel) maps to **0 copies**, not the curve.

## Components

### wnvSurv (new file `R/std_curve.R`, exported, pure)

```r
parse_std_copies(label)
#   "wnv_std_1e6" | "std 1e6" | "1e6" -> 1e6 ; non-standard -> NA_real_
#   Pure string -> numeric. Vectorized.

fit_std_curve(cq, log10_copies)
#   OLS lm(cq ~ log10_copies). Returns one-row tibble:
#     slope, intercept, r2, efficiency, n_points
#   Errors if < 2 distinct log10_copies values (cannot fit).
#   Caller is responsible for passing clean points (real cq, matched virus).

predict_copies(cq, slope, intercept)
#   10^((cq - intercept) / slope). Pure, vectorized math.
#   No special-casing of no-amp — that rule lives in the caller.
```

Each is independently unit-testable and free of I/O, grouping, or domain column
names.

### Report script `scripts/patch_pcr_quantity.R`

Inputs: `--week`, `--year` (resolves `dir_pcr`, `dir_platemap` from the same path
convention the pipeline uses, `1_input/<year>/w<week>/`).

Steps:
1. Read the raw `.xls` `Results` sheet as a **full grid** (`col_names = FALSE`,
   all rows) so the multi-block QuantStudio layout is preserved verbatim.
2. Read + clean the platemap → well → `csu_id`, `sample_type`, target-agnostic
   labels.
3. Build the **standard set**: keep wells whose virus prefix
   (`str_extract(csu_id, "^[^_]*")`) matches the row's `Target Name`; parse known
   copies via `wnvSurv::parse_std_copies()`; drop undetermined Cq (`55.55`).
4. Per `(plate, target)`: `wnvSurv::fit_std_curve()` → params + QC. Print a QC
   table; **warn** (not stop) if R² < 0.98 or E ∉ [90,110]%.
5. Compute `Quantity` for **all** wells: `wnvSurv::predict_copies(cq, slope, intercept)`,
   then set `0` where Cq is undetermined/no-amp. Flag wells whose Cq is outside the
   standard Cq range (extrapolation).
6. Overwrite **only** the `Quantity` column cells in the preserved grid; write to
   `dir_pcr/ss_y<year>_w<week>_p<plate>_pcr.xlsx` (`.xlsx` because `.xls` is not
   writable by `writexl`/`openxlsx`; name keeps the `y####_w##_p#` tokens so
   `clean_pcr`'s regex still extracts year/week/plate).
7. Move the raw `.xls` into a **sibling** dir `<dir_wk>/pcr_raw_uncorrected/`.
   (NOTE — implementation deviation from an earlier draft that put this *inside*
   `pcr/`: the pipeline's `list.files(dir_pcr, full.names=TRUE)` is non-recursive
   but **does list subdirectories**, so an archive subdir of `pcr/` would be handed
   to `read_pcr` and crash. A sibling dir keeps `pcr/` containing only the corrected
   `.xlsx`.) Print a summary + how to undo (move the raw back, delete the corrected
   `.xlsx`).

The pipeline (`wnv-s_weekly_report_pipeline_v2.qmd`) is **unchanged**: after running
the script, `list.files(dir_pcr)` sees only the corrected `.xlsx`, `read_pcr` →
`clean_pcr` find a populated `Quantity`, and `copies`/`log_copies` come out correct.

## Data flow

```
raw ss_..._pcr.xls (Quantity all-NA)
        │
   patch_pcr_quantity.R
     ├─ platemap → known copies per std well
     ├─ wnvSurv::fit_std_curve()  per (plate,target)  → params + QC (warn)
     ├─ wnvSurv::predict_copies() → Quantity for 96 wells (55.55 → 0)
     ├─ write corrected ss_..._pcr.xlsx  (only Quantity cells changed)
     └─ move raw → pcr/uncorrected_raw/
        │
   quarto render  (UNCHANGED)
```

## Error handling

- `fit_std_curve`: hard error on < 2 distinct copy levels (cannot fit a line).
- Bad QC (R²/efficiency): warn loudly, continue (decision 3).
- No standard wells found for a target: error with a clear message (nothing to fit).
- Extrapolated unknowns: warn with the count and well positions.
- Idempotency guard: if `uncorrected_raw/` already holds the raw (script run
  before), error rather than clobber, telling the user to undo first.

## Testing

**wnvSurv (`tests/testthat/test-std_curve.R`):**
- `parse_std_copies`: `1e2/1e4/1e6` variants, mixed case, non-standard → NA.
- `fit_std_curve`: recovers known slope/intercept from a synthetic perfect line
  (R² == 1); efficiency formula matches `10^(-1/m)-1`; `n_points` correct; errors
  on a single copy level.
- `predict_copies`: round-trips — fit a curve from known points, predict at the
  standards' Cq, recover the input copies within tolerance.

**Report (`tests/testthat/`):** fixture test that running the patch logic on a
synthetic all-NA-Quantity input emits a file `read_pcr()` can parse and yields
non-zero `copies` for amplified wells; `55.55` wells stay 0.

## Out of scope

- No change to pipeline qmd or `clean_pcr`.
- Not wired as an automatic in-pipeline fallback.
- No backfill of historical weeks (their QuantStudio Quantity is intact).
- The separate `zone`-dropped bug (missing `key_rename.csv` rows) — tracked
  independently.
```
