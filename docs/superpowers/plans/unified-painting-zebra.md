# Plan: De-fragilize PCR file reads & provider-supplied week/year

## Context

Two `docs/TODO.md` architecture items (lines 19–20) make the weekly pipeline
depend on unreliable, human-entered inputs:

1. **PCR folder reads grab everything.** `wnv-ss_weekly_report_v2.qmd` (line 181)
   calls `list.files(path = dir_pcr, ...)` with **no `pattern`**, so every file in
   the folder — including the QuantStudio `.eds` export — is passed to
   `read_pcr()`, which runs `readxl::read_excel()` on each. Non-Excel files can
   error or pollute the combined frame.

2. **Provider-typed `week`/`year` are trusted.** Two collaborators submit bad
   metadata: BC types a running count of submission weeks (unrelated to the
   calendar) and VDCI sometimes forgets to update the year. Today these values
   survive into the pipeline:
   - **Counts / all_species path** (qmd lines 504–512): `wnv_s_clean()`'s gate
     `!"week" %in% names(df)` (`utils/fun_clean_wnv_s.R:309`, and `:280` for
     year) only *derives* week/year when absent, so provider values are kept.
   - **Pools path** (`utils/fun_clean_4_weekly_input.R`): already recomputes
     `Week` from `Trap Date` (line 102) — good — but still **filters on the
     provider `Year`** (lines 11, 17), so a stale VDCI year silently routes
     samples into `non_week_samples.csv`.

   That same gate is **intentionally** protecting historical read-back (stored
   DBs re-cleaned at qmd lines 104, 114, 123, 615, 639) from being recomputed
   mid-season — see the comment at `fun_clean_wnv_s.R:301–308` and the epiweek
   memory. So the fix must force re-derivation **only on raw-input paths**, never
   globally.

**Decisions (confirmed with user):** re-derive **both week and year** from
`trap_date`; implement via a **`force_recompute` parameter on `wnv_s_clean()`**
(default `FALSE` preserves all current behavior incl. the historical gate).

Outcome: a run no longer needs manual week/year edits, and the PCR folder can
hold the `.eds` alongside the `.xlsx` without breaking the read.

---

## Change 1 — PCR: only read `.xls`/`.xlsx`

**File:** `wnv-ss_weekly_report_v2.qmd`, `pcr-clean` chunk (line 181).

Add an Excel-only pattern to the file listing:

```r
fn_path = list.files(path = dir_pcr,
               pattern = "\\.xlsx?$",   # ignore .eds and any non-Excel files
               full.names = T,
               ignore.case = T)
```

`ignore.case = T` already covers `.XLSX`/`.XLS`. No other change needed — the
platemap chunk (line 215) reads a separate `dir_platemap` folder and is out of
scope for this TODO.

---

## Change 2 — `wnv_s_clean()`: add `force_recompute` parameter

**File:** `utils/fun_clean_wnv_s.R`.

1. Add the argument to the signature (default `FALSE`, so the historical
   read-back call sites are unaffected):

   ```r
   wnv_s_clean <- function(df,
                           all_cols = c(...),
                           zone_lvls = c(...),
                           distinct_col = names(df),
                           silence = F,
                           rm_dupes = T,
                           rm_col = c(),
                           force_recompute = FALSE) {   # NEW
   ```

2. Widen the **ADD YEAR** gate (currently line 280) so it also fires — and
   overwrites — when `force_recompute` is `TRUE`:

   ```r
   if ("trap_date" %in% names(df) &
       (force_recompute | !"year" %in% names(df)) &
       "year" %in% col_2_clean) {
     df <- df %>% mutate(year = lubridate::isoyear(trap_date))
     ...
   }
   ```

3. Widen the **ADD WEEK** gate (currently line 309) the same way, keeping
   `wnvSurv::calc_season_week()` as the single week authority:

   ```r
   if ("trap_date" %in% names(df) &
       (force_recompute | !"week" %in% names(df)) &
       "week" %in% col_2_clean) {
     df <- df %>% mutate(week = wnvSurv::calc_season_week(trap_date))
     ...
   }
   ```

   Year uses `isoyear()` to match the existing ADD YEAR block and the value
   stored historically; for summer surveillance dates `isoyear == calendar
   year`. The downstream CLEAN YEAR / CLEAN WEEK coercion blocks run unchanged.

   Update the doc comment above the WEEK block to note that `force_recompute`
   overrides the gate for raw provider input.

---

## Change 3 — Wire `force_recompute = TRUE` into raw-input call sites only

**File:** `wnv-ss_weekly_report_v2.qmd`.

- **Counts / all_species** (line 512): the `trap_date` is already parsed inline
  just above, hence `rm_col = c("trap_date")`. `week`/`year` are still in
  `col_2_clean`, so the widened gates fire:
  ```r
  wnv_s_clean(rm_col = c("trap_date"), force_recompute = TRUE)
  ```
- **Datasheet clean** (line 158): raw VDCI/BC pool sheet used by `check_data`:
  ```r
  wnv_s_clean(force_recompute = TRUE)
  ```

**Do NOT touch** the stored-DB calls (lines 104, 114, 123, 615, 639) or the
`calc_pools` call (line 588) — they must keep `force_recompute = FALSE` so the
frozen historical baseline is preserved.

---

## Change 4 — Pools path: derive `Year` from `Trap Date`

**File:** `utils/fun_clean_4_weekly_input.R`.

The week side is already handled. Make the year robust to VDCI's stale entry:

- Selection filter (lines 9–13) and its inverse (lines 15–19): replace
  `Year == year_filter` / `Year != year_filter` with
  `lubridate::isoyear(\`Trap Date\`) == year_filter` (and `!=`).
- Output labelling (line 102): also overwrite `Year` from the date, alongside
  the existing `Week` derivation:
  ```r
  mutate(Week = wnvSurv::calc_season_week(`Trap Date`),
         Year = lubridate::isoyear(`Trap Date`))
  ```

This keeps the existing submitter-vs-derived QC warning (lines 24–37) meaningful
and stops correct samples from being dumped to `non_week_samples.csv` over a
typo'd year.

---

## Tests

**File:** `tests/testthat/helper-setup.R` — source the now-parameterized cleaner
(it is not currently in the harness):
```r
source(here::here("utils/fun_clean_wnv_s.R"))
```
(`zone_lvls` is already assigned into `globalenv()` in this helper.)

**New file:** `tests/testthat/test-clean_wnv_s.R` covering the new branch:
- A small frame with `trap_id`, `trap_date`, and **wrong** `week`/`year`:
  - `force_recompute = TRUE` → `week`/`year` overwritten to match
    `wnvSurv::calc_season_week(trap_date)` / `lubridate::isoyear(trap_date)`.
  - `force_recompute = FALSE` (default) → wrong `week`/`year` **preserved**
    (guards the historical read-back contract).
- A frame with **no** `week`/`year` column → derived identically under both
  flag values (back-compat).

Existing `test-calc_week.R` and `test-week-pool-count-match.R` should continue
to pass unchanged.

---

## Verification

1. **Unit tests:** `Rscript tests/run_tests.R` — new `test-clean_wnv_s.R` plus
   the full existing suite green.
2. **Syntax check** of the edited R files (e.g. `Rscript -e 'parse("utils/fun_clean_wnv_s.R"); parse("utils/fun_clean_4_weekly_input.R")'`).
3. **End-to-end pipeline** on the current week (offline, no live writes):
   `quarto render wnv-ss_weekly_report_v2.qmd` after running config with
   `--download F --update F --push F`. Confirm:
   - PCR chunk logs only the `.xlsx` file even with an `.eds` present in
     `dir_pcr`.
   - `datasheet_clean` / `all_spp` carry `week`/`year` matching the trap dates
     regardless of what BC/VDCI typed.
   - `non_week_samples.csv` is empty for correctly-dated samples even if the
     provider year is stale.
4. **Manual spot-check:** temporarily set a wrong `week`/`year` on one input row
   and confirm the report still keys it to the correct seasonal week/year.

---

## Files touched
- `wnv-ss_weekly_report_v2.qmd` (pcr-clean, datasheet-clean, clean-all-spp chunks)
- `utils/fun_clean_wnv_s.R`
- `utils/fun_clean_4_weekly_input.R`
- `tests/testthat/helper-setup.R`
- `tests/testthat/test-clean_wnv_s.R` (new)
- `docs/TODO.md` (check off the two items)
