# Unify `wnv_s_clean` + `trap_status` into wnvSurv

## Context

A collaborator audit (`sandbox/Fort_Collins_TrapLevel_VI_Combined v2.xlsx`) found zone-weeks
where a positive pool existed but PIR and VI were 0. Root cause was
`filter(total > 1)` in `calc_pir()`, which dropped real single-mosquito pools —
including positives such as CSU23480 (NW / 2025 / wk26 / Pipiens). That is fixed.

Fixing it exposed that the shared surveillance functions have **forked across three
repos**, and that the pipeline was reinstalling wnvSurv from GitHub on every run —
silently reverting local fixes mid-session. `calc_pir`, `calc_vi`, and the install
behaviour are now resolved (see "Already done"). `wnv_s_clean` is not, because the
package copy is *behind* the local one and the two disagree about what `trap_status`
means.

**Intended outcome:** one implementation of `wnv_s_clean()` and `assign_trap_status()`
living in wnvSurv, consumed by both repos, with a single `trap_status` vocabulary —
and `zone_stats.csv` unchanged by the migration itself.

## Already done (verified, uncommitted)

| Change | Evidence |
|---|---|
| `calc_pir` fix in wnvSurv + this repo; local copy archived | 12 false-zero cells corrected, 0 regressions, tests pass |
| `calc_vi` wired to wnvSurv; local copy archived | 7,380 rows, **0** differing cells |
| `utils/fun_calc_vi_stats.R` archived — it defined a colliding `calc_vi` | survived only by locale collation luck |
| `config/load_packages.R` — install only if missing, else warn when the local checkout is newer | both branches tested |

Tests: `FAIL 0 | WARN 0 | SKIP 0 | PASS 124`.

## Key findings driving this plan

**1. The package `wnv_s_clean` is behind, not merely different.**
`utils/fun_clean_wnv_s.R` (436 lines, 9 params) vs `wnvSurv::wnv_s_clean` (312 lines,
4 params). Swapping directly would break **27 of 49 call sites** here (`silence` ×20,
`force_recompute` ×3, `distinct_col` ×3, `zone_raw_lvls` ×1). Critically, the package
version leaves `zone` and `spp` as **character, not factor** — CLAUDE.md's first gotcha,
which causes silent grouping errors in `calc_pir`/`calc_vi`.

**2. Each version has something the other needs.** The port is a merge:

| | utils | wnvSurv |
|---|---|---|
| zone/spp factor coercion | ✅ | ❌ |
| `silence`, `force_recompute`, `distinct_col`, `rm_dupes` | ✅ | ❌ |
| dedup via `janitor::get_dupes` | ✅ | ❌ |
| `spp0` (raw spp preserved) | ❌ | ✅ |
| `na.rm = TRUE` in `any()` guards | ❌ | ✅ |

**3. The NA that motivates `na.rm`.** `spp` is never NA anywhere. `total` is NA on
7,168 rows — 116 `malfunction` + 7,052 `no trap` — written deliberately
(`utils/fun_trap_complete.R:52`, and trap_hx_combiner's `assign_trap_status()`).
`any(total > 0)` → `any(NA > 0)` → `NA`, the `culex` branch fails to match, and the row
falls through to `other spp`. Latent today only because `wnv_s_clean()` runs on raw
data *before* those NAs are introduced (qmd:508-516).

**4. Recompute on stored data is lossy.** The raw vocabulary (`No Mosquitoes`,
`Aedes vexans`, `Culiseta inornata`) exists only in `1_input/*/all_species/`. The qmd
drops `spp0` at line 539, so `culex_database_update.csv` has `spp` collapsed to imputed
Pipiens/Tarsalis pairs. `no mosquitoes` vs `other spp` cannot be recovered from it.

**5. Two producers write the same column.** Row-level statuses come from
`wnv_s_clean()`; `no trap` comes from `wnv-ss_trap_hx_combiner/R/fun_assign_trap_status.R`
at zone-week level. That is the actual origin of the vocabulary fork.

**6. Palette/vocabulary mismatch.** `wnvSurv::trap_status_colors` has **`no traps`**
(plural); the data contains **`no trap`** (singular) — 7,052 rows currently miss the
palette. `plot_n_trap.R:52-53` also has an unreachable branch and a missing `na.rm`.

## Decisions taken

- **Always recompute**, scoped to raw ingest where `spp0` exists. Stored read-back
  passes `trap_status` through untouched. No schema change; history unchanged.
- **`other spp` replaces `no culex`** — same meaning, more explicit.
- **Port both** `wnv_s_clean()` and `assign_trap_status()` into wnvSurv; update
  trap_hx_combiner to consume them.

---

## Phase 1 — Port `wnv_s_clean` into wnvSurv

Base is `utils/fun_clean_wnv_s.R` (the superset). Merge in the package's improvements.

**`../wnv-ss_functions/R/wnv_s_clean.R`** — replace with the merged implementation:

- Signature: `df, all_cols, zone_lvls, zone_raw_lvls, distinct_col, silence, rm_dupes,
  rm_col, force_recompute`. Keep `verbose` as a deprecated alias for `!silence` so the
  3 existing 4-arg callers keep working.
- Keep from utils: zone/spp **factor coercion** (non-negotiable — CLAUDE.md gotcha),
  `janitor::get_dupes` dedup, `lubridate::parse_date_time` dates,
  `wnvSurv::calc_season_week()` for week, the `clean_summary()` reporter.
- Adopt from package: `spp0 <- spp` before `spp` is overwritten, and **`na.rm = TRUE`**
  on every `any()` inside the `trap_status` `case_when`.
- **Change the `trap_status` gate** to mirror the existing year/week pattern
  (`utils/fun_clean_wnv_s.R:294`, `:324`):
  ```r
  if ("spp" %in% names(df) &&
      (force_recompute || !"trap_status" %in% names(df)) &&
      "trap_status" %in% col_2_clean) { ... }
  ```
  This is the whole of decision 1 — qmd:516 already passes `force_recompute = TRUE`
  (raw), qmd:619 does not (stored).
- Row-level vocabulary emitted: `culex`, `other spp`, `no mosquitoes`, `malfunction`.
  No `no culex` branch.

**`../wnv-ss_functions/tests/testthat/test-wnv_s_clean.R`** — add, written failing first:
- zone and spp come back as factors with `zone_lvls` / spp levels
- `total = NA` on a culex trap-night does **not** yield `other spp` (the `na.rm` guard)
- `force_recompute = FALSE` + existing `trap_status` → passes through unchanged
- `force_recompute = TRUE` → re-derives
- `spp0` retains raw text after `spp` is collapsed

**STOP GATE.** `sandbox/audit_wnv_s_clean.R` (already written) extended to run the merged
function against the archived local one over both real databases. Expect: identical rows,
identical columns except `spp0`, identical factor levels, **0 differing `trap_status`
values** when `force_recompute = FALSE`.

## Phase 2 — Vocabulary: `no culex` → `other spp`

- **`../wnv-ss_functions/R/data.R`** (or wherever `trap_status_colors` is defined):
  rename `no culex` → `other spp`, and fix **`no traps` → `no trap`**.
- **`../wnv-ss_functions/R/plot_n_trap.R:48-54`**: collapse the unreachable
  `no culex` branch into `other spp`, add the missing `na.rm = TRUE` on line 53,
  update the roxygen priority note on lines 4-5.
- **One-time relabel** of the 590 stored `no culex` rows in `culex_sheet_database`.
  This is a pure rename, not a recompute — lossless. Without it the database carries a
  mixed vocabulary and the trap-status plot renders both categories.

**STOP GATE.** Regenerate `trap_status.png` and confirm five categories with correct
colours and no grey "unmapped" band.

## Phase 3 — Port `assign_trap_status` into wnvSurv

- Move `wnv-ss_trap_hx_combiner/R/fun_assign_trap_status.R` → `../wnv-ss_functions/R/assign_trap_status.R`,
  roxygen'd and exported, emitting the Phase 2 vocabulary.
- Update trap_hx_combiner to call `wnvSurv::wnv_s_clean()` and
  `wnvSurv::assign_trap_status()`; archive its two local forks
  (`R/fun_wnv_s_clean.R`, `R/fun_assign_trap_status.R`).

**STOP GATE.** Run trap_hx_combiner end-to-end and diff its output against a frozen
baseline. Its 38 call sites use only `all_cols` / `rm_col`, so the merged defaults must
reproduce them exactly.

## Phase 4 — Wire in and archive

- `git mv utils/fun_clean_wnv_s.R utils/archive/` (also removes the duplicate local
  `clean_summary()`).
- Drop the `source()` line from `tests/testthat/helper-setup.R:27`, mirroring what was
  done for `calc_pir` / `calc_vi`.
- Remove the now-redundant `mutate(spp0 = spp)` at **qmd:515** — the package does it.
- `R CMD INSTALL ../wnv-ss_functions`.

## Verification

```bash
# unit tests, both repos
Rscript tests/run_tests.R                       # expect FAIL 0, PASS >= 124
cd ../wnv-ss_functions && Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'

# resolution: every shared function must come from the package
Rscript -e 'source("config/load_packages.R")
  purrr::walk(list.files("utils","*.R",full.names=TRUE), source)
  for (f in c("calc_pir","calc_vi","wnv_s_clean"))
    cat(f, "->", environmentName(environment(get(f))), "\n")'   # all -> wnvSurv

# end-to-end: zone_stats must be unchanged by the migration
Rscript sandbox/check_csu23480.R    # NW 2025 wk26 Pipiens: pir 0.0658, vi 0.05
Rscript sandbox/audit_wnv_s_clean.R # 0 differing trap_status under force_recompute=FALSE

# full render, touching nothing remote
bash config/run_config.sh --week 29 --year 2026 --download F --update F --push F
quarto render wnv-ss_weekly_report_v2.qmd
```

Compare the regenerated `3_output/2026/w29/zone_stats.csv` against the pre-migration
run. Expected delta: **only** the 339 PIR / 124 VI cells from the already-completed
`calc_pir` fix. Any `wnv_s_clean` row is a regression.

## Risks

- **`no trap` count is large** (7,052 rows). Any change to how it is assigned moves the
  trap-status plot substantially. Phase 3's stop gate is the control.
- **`config/config_weekly.R:252`** lists `BC` twice in `grp_zones`. Currently defused by
  `unique()` in `calc_all()`; worth fixing at source.
- **Uncommitted work in two repos.** wnvSurv is `ahead 1` and unpushed. Since Phase 0
  removed the auto-reinstall, the local install is now authoritative — but any
  collaborator pulling this repo without the wnvSurv checkout gets GitHub `main`, which
  still has the old `calc_pir`. Push before anyone else runs the pipeline.
