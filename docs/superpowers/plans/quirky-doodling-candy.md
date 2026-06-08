# Plan: Unify week assignment across the WNV surveillance pipelines (phased, with stop gates)

## Context

The reported `week` is meant to be a **seasonal week** where week 23 = the first full week
of June every year, so year-over-year plots stay anchored to the start of surveillance. Four
problems break this today:

1. **Three different week rules**, so the *same trap on the same `trap_date`* can be filed
   under different weeks:
   | Site | Repo | Rule |
   |---|---|---|
   | weekly counts | report `utils/fun_clean_wnv_s.R:274-281` | `isoweek(trap_date)` |
   | weekly pools | report `utils/fun_clean_4_weekly_input.R:55` | `epiweek(Trap Date)` |
   | historical counts | combiner `R/fun_wnv_s_clean.R:239-243` | `coalesce(submitter_week, isoweek)` |
   | SSOT (unused) | `wnv-ss_functions/R/wnv_s_clean.R:213-216` | `coalesce(submitter_week, isoweek)` |

   Measured on the rebuilt history file: pools (`epiweek`) and counts (`isoweek`) disagree for
   **16.9%** of distinct collection dates; the `coalesce` to submitter-typed week leaves **~7%**
   of real rows disagreeing with the date (2019-2024). PIR/VI join pools to counts on
   `c(year, week, zone, spp)`, so a 1-in-6 mismatch silently splits a trap's pool from its own
   count. **Pools come from the same traps as the counts — their week MUST match by construction.**

2. **2026 leap-week break.** 2025 had 53 epiweeks, so `epiweek(first-week-June 2026) = 22`, not
   23. Hardcoded `23:37` / `23:week_filter` filters would drop the first surveillance week.

3. **No single source of truth.** The `wnvSurv` package (`wnv-ss_functions`) exists to end the
   duplication but is not yet consumed by either pipeline.

4. **Historical window is a fixed start, not rolling.** `config/config_weekly.R:55` uses
   `seq(args$year_hx, year-1)` with `--year_hx` hardcoded to **2017** (`#TODO change to rolling
   window`), so the "rolling average" silently widens every season. It must become the **5 prior
   seasons** (`year-5 … year-1`). A second inconsistent hardcode at qmd:660 (`2015:(year-1)`)
   feeds the plotted line, so plot and table currently use different baselines.

**Hard constraints (user):**
- The historical baseline is a 5-year rolling average. It may change at a **season boundary**
  (window advances) but must **not** wobble mid-season. The historical correction must be applied
  **once, frozen for the season, and validated** before going live.
- **`wnv_s_clean()` is crucial and has forked** across the three repos into different feature
  sets (report: dedup + PCR `cq`/`copies` + `zone`/`spp` factor coercion; combiner/wnvSurv:
  zone-from-trap_id fallback + `spp0`/`trap_status`). A full unification is a separate, larger
  effort — **out of scope here.**
- The work must proceed in **phases with explicit STOP gates** so outputs can be manually checked
  for pipeline functionality before continuing.

**Scope decision (confirmed):** Share **only** the new week utility from `wnvSurv` now; each
pipeline keeps its own local `wnv_s_clean` and just calls `wnvSurv::calc_season_week()` in the
week block. Additionally, **incorporate the combiner's `clean_summary()` into the report** — it
is a drop-in (identical signature `clean_summary(df0, df, col_name, label)`) that only swaps the
report's noisy `cat()` logging for tidy `cli::cli_alert_*` output, with **no effect on data**.

## The week rule (already written and tested — reuse, do not rewrite)

`utils/fun_calc_week.R` (passing tests in `tests/testthat/test-calc_week.R`):
- `first_monday_of_june(year)` — seasonal anchor (= week 23).
- `calc_season_week(date)` = `23 + (epiweek(date) - epiweek(first_monday_of_june(year)))`. Equals
  `epiweek` in normal years (preserves history); patches +1 in 2026.
- `add_week_cols(df, date_col)` — adds `week` (seasonal) + `epiweek` (raw MMWR).

This file is the canonical logic to move into `wnvSurv`.

---

## Phase 1 — wnvSurv foundation (no pipeline behavior change)

1. Add `wnv-ss_functions/R/calc_week.R`: `calc_season_week()`, `add_week_cols()`,
   `first_monday_of_june()` ported verbatim from the report's `utils/fun_calc_week.R`, with
   roxygen `@export` and `@importFrom lubridate epiweek year`.
2. Port `tests/testthat/test-calc_week.R` into wnvSurv.
3. `devtools::document()`; install wnvSurv into **both** pipelines' renv environments using the
   proper tooling (`renv::install("local::../wnv-ss_functions")` then `renv::snapshot()` — never
   hand-edit `renv.lock`).

**STOP / manual check 1:** `devtools::test()` + `devtools::check()` pass in wnvSurv;
`wnvSurv::calc_season_week(as.Date("2026-06-01")) == 23` in a fresh session of each pipeline. No
pipeline output has changed yet.

---

## Phase 2 — Combiner: correct historical week + regenerate the frozen master

1. Add `library(wnvSurv)` to `pipelines/pipeline_combine_culex_sheet.qmd`.
2. `R/fun_wnv_s_clean.R:239-243`: replace the `coalesce(submitter, isoweek)` block with
   `wnvSurv::calc_season_week(trap_date)` + add `epiweek = lubridate::epiweek(trap_date)`. Rows
   **with** `trap_date` get the seasonal week; **imputed grid rows** (no `trap_date`, ~35% of
   output) keep their skeleton week (`expand_trap`, 23-37) and get `epiweek = NA`. Keep submitter
   `week` only to emit a QC warning when it differs by >1.
3. `make_key()` / `culex_dedup()` (key on `c(trap_id, spp, year, week)`) now operate on the
   corrected seasonal week — no change needed.
4. **Bookkeeping `week_submitted` column** (added per user request): the corrected `week`
   overwrites ~7.9% of entries (1522/19380 dated rows), so preserve the original
   human-entered week for audit. `wnv_s_clean` emits `week_submitted` (captured before the
   overwrite); a lookup built from `culex_clean` (pre-`prep_for_skeleton`) is `left_join`ed
   onto the final output beside `epiweek`. Imputed/no-trap rows → `NA`. Verified non-invasive:
   `prep_for_skeleton` uses explicit `select()`/`distinct()` so it drops the column with no
   row-explosion; no core skeleton function changes.
5. Regenerate `culex_sheet_database_expand.csv` **once**. This becomes the new master
   `culex_sheet_database` (now carrying `week`, `epiweek`, `week_submitted`).
6. Produce a **before/after diff of the historical baseline** (`sandbox/diff_week_baseline.R`):
   per-trap-date week-label changes + `(week, zone, spp)` abundance over the 2021-2025 window,
   old vs corrected master; diff tables + overlay plot quantifying the one-time shift.

**STOP / manual check 2:** User reviews the before/after diff — confirm the shift is explainable
as a start-of-season correction (iso/epi boundary + submitter-leakage reallocation), imputed-row
counts are unchanged, `epiweek` is populated where dates exist and `NA` otherwise, and combiner
tests pass (`Rscript tests/testthat.R`). The corrected master is now frozen for the season.

---

## Phase 3 — Report: consume wnvSurv at both ingestion sites + rolling window

1. `config/load_packages.R`: add `library(wnvSurv)` and `library(cli)`. (Report auto-sources all
   of `utils/` via `purrr::walk(list.files("utils"), source)` at qmd:47-52.)
2. **Counts path** `utils/fun_clean_wnv_s.R:274-281`: replace `isoweek(trap_date)` with
   `wnvSurv::calc_season_week(trap_date)` + add `epiweek`. Keep the `!"week" %in% names(df)` gate
   so stored/historical data is never recomputed on read-back (this is what keeps the frozen
   baseline stable mid-season).
3. **Pools path** `utils/fun_clean_4_weekly_input.R`:
   - `:8`/`:11` filter — select the current week from `Trap Date`
     (`wnvSurv::calc_season_week(\`Trap Date\`) == week_filter`) instead of submitter `Week`.
   - `:55` — `Week = wnvSurv::calc_season_week(\`Trap Date\`)` + add `epiweek`.
   - Emit the submitter-vs-derived QC warning.
4. **clean_summary incorporation:** replace the report's `cat()`-based `clean_summary()` (top of
   `fun_clean_wnv_s.R`) with the combiner's `cli`-based version (drop-in, same signature). The
   existing `if(!silence)` gating stays. No data change.
5. Remove the now-duplicated local `utils/fun_calc_week.R`; point
   `tests/testthat/helper-setup.R:20` at `library(wnvSurv)` instead of sourcing the local file.
6. `config/config_weekly.R:22`: `--week` default `isoweek(Sys.Date())` →
   `wnvSurv::calc_season_week(Sys.Date())`. The `23:37` / `23:week_filter` filters stay (valid now
   that every week is seasonal). (`config/config_hx.R` is unused — ignore.)
7. **Rolling window:** `config/config_weekly.R` — replace `--year_hx` (fixed 2017) with a
   window-length arg `--n_hx_years` (default 5), so `year_filter_hx = seq(year_filter -
   args$n_hx_years, year_filter - 1)`. `wnv-s_weekly_report_pipeline_v2.qmd:660`: replace
   hardcoded `2015:(year_filter-1)` with `year_filter_hx` so plot (`hx2`) and table (`hx`,
   qmd:649) share one baseline. For 2026 → seasons **2021-2025**.
8. **Schema (persist `epiweek` + `week_submitted`):** add `"epiweek"` and `"week_submitted"`
   to the schema vectors in `config/config_weekly.R` (`col_input_database`, `col_datasheet`,
   `col_database`, column-type map ~line 258) so the `select(all_of(...))` steps at qmd:421/567
   carry them through instead of dropping them. (`week_submitted` only applies to the culex
   counts schema, where the combiner produces it; the pool path has no submitter-week column.)
9. **New test** `tests/testthat/test-week-pool-count-match.R`: for `trap_date`s spanning the
   iso/epi boundary and the 2026 leap week, assert the pools path and counts path assign the
   **identical** `week`, and both add `epiweek`.

**STOP / manual check 3:** Run `Rscript tests/run_tests.R` (existing `calc_*` + new matching test
pass). Regenerate config offline (`bash config/run_config.sh --week 23 --year 2026 --download F
--update F`) and render the report against the **Phase-2 corrected master**. Manually confirm:
first-June 2026 is **not** dropped at the `23:week_filter` filter; `current_wk` and `hx` align;
the matching invariant holds (join `database_new` to `culex_new` on
`c(trap_id, trap_date, spp, zone)` → 0 week mismatches); `year_filter_hx` = `2021:2025` and the
caption (qmd:698) reads "2021 to 2025"; `zone_stats.csv` carries `epiweek`; plots/tables render.

---

## Phase 4 — Go live (only after sign-off at check 3)

1. Backfill the `epiweek` column in both master Google Sheets (`epiweek(trap_date)` where a date
   exists, `NA` for imputed rows); historical `week` comes from the Phase-2 rebuild — do not
   hand-edit.
2. Re-run with `--update T` to push the corrected master + schema to the live sheets.

**STOP / manual check 4:** Verify the live sheets carry `week` (seasonal) + `epiweek`, and that a
re-render in a subsequent week leaves the historical line identical week to week (stability proof).

---

## Verification summary
- **Unit tests:** report `Rscript tests/run_tests.R`; wnvSurv `devtools::test()`/`check()`;
  combiner `Rscript tests/testthat.R` (`test-wnv_s_clean.R`, `test-expand_trap.R`,
  `test-assign_trap_status.R`).
- **Matching invariant** on the current week's real data: 0 week mismatches between pools and
  counts for shared trap-dates.
- **Rolling window** resolves to `2021:2025` for `--year 2026`; plot and table share it.
- **Baseline-shift diff** (Phase 2) reviewed before going live.
- **Mid-season stability**: successive re-renders against the frozen master give an identical
  historical line.

## Out of scope (noted, not changed here)
- Full unification of `wnv_s_clean()` across the three repos (separate, larger effort).
- Full migration of all shared functions to `library(wnvSurv)`.
- `config/config_hx.R` — unused/unwired; left untouched.
