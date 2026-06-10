# Plan: Unit-test the key-join merge + migrate combiner to `wnvSurv::make_key`

## Context

We recently fixed a database-merge duplication bug in the weekly pipeline. The
root cause: the `clean-update-all-spp-culex` merge joined on
`by = c("trap_id","trap_date","spp","zone")`, but the trap-history combiner
pre-seeds keyed `no trap` **stub** rows with `trap_date = NA`. A real row (real
`trap_date`) never matched its own stub → the `FULL` join kept both → 148
duplicate w23 rows. The fix switched the merge to a stable composite key
(`make_key()` → `trap_id|spp|year|week`, joined `by = "key"`), validated against
real data (43,260 rows, 0 duplicate keys). The keyed, de-duplicated database
(`sandbox/culex_sheet_database_MERGED_w23.csv`) is now the Drive SSOT, so the
fix is live from w24 onward.

This plan addresses two follow-ups:

1. **Unit-test the merge behavior.** It is currently untestable: the merge is
   inline in the QMD `else` branch *and* duplicated inside `update_gsheet()`,
   with GSheet/network side effects. We extract it into a pure function and test
   the de-duplication invariant so this class of bug can't silently return.
2. **Remove the `make_key` fork.** The combiner ships its own
   `R/fun_make_key.R` and `source()`s it at pipeline line 46 — *after*
   `library(wnvSurv)` — so the local copy shadows the SSOT. The two are
   behaviorally identical (same gsub→paste→select algorithm; only docs/style
   differ), so migrating to `wnvSurv::make_key` is safe and unifies the SSOT.

**Scope decisions (confirmed with user):** migrate **`make_key` only** — the
other forked shared functions (`wnv_s_clean`, `key_rename`,
`parse_flexible_date`, `culex_dedup`; 50–462 line diffs) are deferred to a
separate, careful per-function reconciliation pass. For testing: **refactor +
write tests**, not document-only.

---

## Part A — Make the merge testable + add tests (wkly repo)

### A1. Extract the merge into a pure function
New file `utils/fun_merge_trap_database.R`:

```r
# Pure database-merge step shared by update_gsheet() and the QMD update branch.
# FULL natural_join coalesces new (first arg) over old, so real rows overwrite
# their pre-seeded keyed stubs. No I/O, no globals -> unit-testable.
merge_trap_database <- function(new, old, by = "key", col_database = names(old)) {
  out <- rquery::natural_join(new, old, jointype = "FULL", by = by) |>
    dplyr::select(dplyr::all_of(col_database))
  if ("trap_date" %in% names(out)) {
    out <- out |>
      dplyr::mutate(trap_date = lubridate::as_date(lubridate::as_datetime(trap_date))) |>
      dplyr::arrange(dplyr::desc(trap_date))
  }
  out
}
```
This is exactly the logic currently duplicated in the two call sites below —
behavior is preserved, only relocated.

### A2. Route both call sites through it (removes duplication)
- `utils/fun_update_gsheet.R`: replace its inline `update = rquery::natural_join(...) %>% select(...)` + the `trap_date` re-coercion block with
  `update <- merge_trap_database(new, old, by = by, col_database = col_database)`.
  Keep everything else (auth guard, `drive_cp`, `sheet_write`, `write.csv`) unchanged.
- `wnv-s_weekly_report_pipeline_v2.qmd`, `clean-update-all-spp-culex` `else` branch: replace the
  `culex_merged <- rquery::natural_join(culex_new, culex_database, jointype="FULL", by="key") %>% ...`
  block with
  `culex_merged <- merge_trap_database(culex_new, culex_database, by = "key", col_database = names(culex_database))`.
  Source the new util near the other `utils/` loads.

### A3. Add `tests/testthat/test-merge_trap_database.R`
Follow the `test-update_gsheet.R` pattern: `source(here::here("utils","fun_merge_trap_database.R"))`
at the top; build tiny synthetic `old`/`new` tibbles (cols:
`key, trap_id, spp, year, week, trap_status, total, trap_date`). No OAuth/network.
Cases:
1. **No duplication + coalesce direction** — `old` = keyed stub `(K, "no trap", total NA, date NA)`; `new` = real `(K, "culex", total 5, date d)`. Expect one row for K with `total == 5`, `trap_status == "culex"`, `trap_date == d`.
2. **Genuine no-trap kept** — `new (K,"no trap",NA)` + stub `(K,"no trap",NA)` → one row, no dup.
3. **Brand-new key** — `new` has K2 absent from `old` → present in output (FULL join).
4. **Historical key untouched** — `old` has K3 absent from `new` → present, values unchanged.
5. **Core regression guard** — `expect_equal(nrow(out), dplyr::n_distinct(out$key))` and `expect_false(any(is.na(out$key)))`. This is the invariant the old `trap_date` join violated.

`rquery` is already a pipeline dependency (installed); the function calls it
namespaced (`rquery::natural_join`) so no `library(rquery)` is needed in tests.

---

## Part B — Migrate combiner to `wnvSurv::make_key` (combiner repo: `../wnv-ss_trap_hx_combiner`)

1. Namespace the three internal call sites (bare `make_key(...)` → `wnvSurv::make_key(...)`):
   - `R/fun_dedup_culex.R:65`
   - `R/fun_expand_trap.R:126`
   - `R/fun_prep_for_skeleton.R:75`
2. `DESCRIPTION`: add `wnvSurv` to `Imports:` and add a `Remotes:` line
   `rtobiaskoch/wnv-ss_functions` (wnvSurv is GitHub, not CRAN).
3. Delete `R/fun_make_key.R`.
4. Remove `source("R/fun_make_key.R")` (pipeline `pipelines/pipeline_combine_culex_sheet.qmd` line 46).
5. `devtools::document()` to refresh `NAMESPACE` (no export change expected —
   `make_key` was never exported by the combiner; explicit `wnvSurv::` avoids
   needing an `importFrom`).

**Do NOT touch** the other forked sources (`fun_wnv_s_clean.R`, `key_rename.R`,
`fun_parse_flexible_date.R`, `fun_dedup_culex.R`'s own body) — deferred.

---

## Part C — wnvSurv: add the missing `make_key` test (`../wnv-ss_functions`)

`make_key` is depended on by both repos but has **no test**. Add
`tests/testthat/test-make_key.R` (match `test-key_rename.R` style):
- Recipe: `make_key(df, c("trap_id","spp","year","week"))` on `FC-088GR / Pipiens / 2026 / 23` → `"FC088GR|Pipiens|2026|23"` (dash stripped, `|` separator preserved).
- Values with whitespace/punctuation are sanitised; separator preserved verbatim.
- Key column placed first; original columns unchanged.
- Errors: `< 2` `key_cols`; a `key_col` missing from `df`.

---

## Critical files

**wkly repo**
- `utils/fun_merge_trap_database.R` (new)
- `utils/fun_update_gsheet.R` (route through new fn)
- `wnv-s_weekly_report_pipeline_v2.qmd` (`clean-update-all-spp-culex` else branch + source line)
- `tests/testthat/test-merge_trap_database.R` (new)

**combiner repo** (`../wnv-ss_trap_hx_combiner`)
- `R/fun_dedup_culex.R`, `R/fun_expand_trap.R`, `R/fun_prep_for_skeleton.R` (namespace calls)
- `DESCRIPTION` (Imports + Remotes), `pipelines/pipeline_combine_culex_sheet.qmd` (drop source line)
- delete `R/fun_make_key.R`

**wnvSurv repo** (`../wnv-ss_functions`)
- `tests/testthat/test-make_key.R` (new)

## Verification

1. **wnvSurv first** (combiner will consume it): `Rscript -e 'devtools::test()'` → `test-make_key` green. Reinstall so the combiner sees it: `Rscript -e 'devtools::install("../wnv-ss_functions")'`.
2. **combiner**: `Rscript -e 'devtools::test()'` → `test-expand_trap` and `test-prep_for_skeleton` stay green (they exercise the `make_key` path). Optionally confirm resolution: after dropping the source line, in the pipeline env `environmentName(environment(make_key))` reports `wnvSurv`.
3. **wkly**: `Rscript tests/run_tests.R` → all green incl. new `test-merge_trap_database`. Optional real-data sanity: re-run the keyed-merge check (expect 43,260 rows, 0 duplicate keys) against `sandbox/culex_sheet_database_expand_NEW.csv` + this week's update.

## Risks / notes
- **Ordering**: refresh/install wnvSurv before validating the combiner, or `wnvSurv::make_key` won't resolve.
- **No behavior change intended** in Part A — the extracted function is the existing code moved verbatim; tests pin the invariant.
- The deferred forked functions remain a known SSOT debt (`[[shared-function-ssot-wnvsurv]]`).
