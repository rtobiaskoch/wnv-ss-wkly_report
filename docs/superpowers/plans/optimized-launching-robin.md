# Repo Cleanup Plan — docs/TODO.md "architecture" section

## Context

`docs/TODO.md` has an "architecture" checklist of housekeeping items that have
accumulated as the pipeline matured (multi-year refactor from `0_R/` →
`utils/`, and partial migration to the shared `wnvSurv` package). The goal of
this pass is a low-risk cleanup sweep: remove dead code/files, finish a few
safe `wnvSurv` migrations, and tidy `sandbox/`/`3_output/` scratch artifacts —
without touching pipeline logic or output-producing code paths.

Two items from the TODO list (`update functions from wnvSurv`, `delete unused
functions`, `move one-off code to scripts`, `look for dead code/unused data`)
are addressed here. Two larger items — **renaming `utils/` → `R/`** and
**switching to `devtools`/package-style loading** — are *not* included. They
touch every `source()`/library-load call across the whole pipeline and
deserve their own dedicated plan + stop-gates (per
[[phased-plans-with-stop-gates]]). They're listed at the end as a follow-on.

This plan does not change any pipeline output. After each step, a quick
`grep` check confirms no remaining references to anything removed/moved.

---

## Step 1 — Remove unused `utils/` functions (4 files)

Confirmed zero call sites anywhere in the active codebase (`utils/`, `*.qmd`,
`config/`), excluding `0_R/`/`archive/`/`dev/`/`graveyard/` which are legacy
and not searched:

- `utils/fun_add_missing_x.R` (`add_missing_x`)
- `utils/fun_clean_dir.R` (`clean_dir`)
- `utils/fun_diffs_by_col.R` (`diffs_by_col`)
- `utils/fun_rm_grp_na.R` (`rm_grp_na`)

**Action:** `git rm` these 4 files. No call sites to update.

**Verification:** `grep -rn "add_missing_x\|clean_dir\|diffs_by_col\|rm_grp_na" --include="*.R" --include="*.qmd" .` (excluding `0_R/`, `utils/archive`, `utils/dev`, `utils/graveyard`) returns nothing.

---

## Step 2 — Move `fun_gdrive_download.R` to `scripts/`

`gdrive_download()` (`utils/fun_gdrive_download.R`) has zero callers in the
active pipeline — it's a one-off helper with a commented `#usage` example,
which is exactly the "useful one-off code" the TODO asks to move to
`scripts/`.

**Action:** `git mv utils/fun_gdrive_download.R scripts/fun_gdrive_download.R`.

**Verification:** `grep -rn "gdrive_download" --include="*.R" --include="*.qmd" .` confirms no callers in `utils/`/`*.qmd` reference it (so the move doesn't break anything), and `tests/testthat/helper-setup.R` doesn't register it.

---

## Step 3 — Dead-code check on `0_R`-only functions and `pipelines/`

Resolved during exploration:

- `calc_stats_grp` (`utils/fun_calc_stats_grp.R`) and `get_trap_n`
  (`utils/fun_calc_max_trap_n.R`) are called only from `0_R/historical_weekly.R`
  (lines 7/19 and 77/82). `0_R/` is legacy/do-not-modify per `CLAUDE.md`, but
  these two functions are *not unused* — they're still referenced by it.
  **Recommendation: leave both functions in `utils/` as-is.** They're a
  by-product of the `0_R` → `utils` migration and removing them would make
  `0_R/historical_weekly.R` (already legacy, but still readable reference
  code) fail if anyone runs it.

- **New finding:** `pipelines/wnv-s_multiweek_report_pipeline.qmd` sources 14
  files from a `scripts/` directory (`scripts/config.R`,
  `scripts/datasheet_read_clean.R`, `scripts/historical_weekly.R`, etc.) —
  **none of these files exist** (only `scripts/patch_pcr_quantity.R` exists
  today). This qmd is fully broken/dead — it's an old pipeline snapshot that
  predates the `0_R/` → `utils/` migration and was never updated. It is not
  referenced by any other active file (`grep -rln "pipelines/"` returns
  nothing outside `docs/`).

**Action:** `git rm pipelines/wnv-s_multiweek_report_pipeline.qmd`. Leave
`pipelines/calc_indiv_stats.qmd` and `pipelines/config_weekly.RData` — these
weren't part of this audit and need a separate look (flagged below as
follow-on, not blocking this cleanup).

**Verification:** confirm `grep -rln "wnv-s_multiweek_report_pipeline"` (excluding `docs/`) returns nothing before removing.

---

## Step 4 — Low-risk `wnvSurv` migrations

Three functions in `utils/` have equivalents already in the installed
`wnvSurv` package. Migrating means: delete the local `utils/fun_*.R`, and
`wnvSurv::*` is picked up automatically since `wnvSurv` is already
`library()`-loaded in `config/load_packages.R`.

| Local function | wnvSurv status | Action |
|---|---|---|
| `clean_platemap` (`utils/fun_clean_platemap.R`) | **Identical** signature and body (verified via `body()` diff) | Safe to delete local copy. One call site (`wnv-ss_weekly_report_v2.qmd:228`, all named args) — no change needed. |
| `plot_std`, `plot_pcr` (`utils/fun_plot_pcr.R`) | `plot_std` identical; `plot_pcr` has `pattern_2_keep` moved to last position, but both call sites (`wnv-ss_weekly_report_v2.qmd:346,348,356`) use **all named arguments** | Safe to delete local copies. No call site changes needed. |
| `gsheet_pull_prompt` (`utils/fun_gsheet_pull_prompt.R`) | Arg order differs: local is `(filename, sheet, key, force_update)`, wnvSurv is `(filename, key, sheet, force_update, verbose)`. **All 7 call sites use positional args** (`wnv-ss_weekly_report_v2.qmd:75,77,79,81,83,85,87` and `wnv-s_calc_hx.qmd:54,56,58`) — e.g. `gsheet_pull_prompt(fn_gdrive_database, "data", key_database_gsheet)` | **Requires call-site updates.** Either (a) convert all 10 call sites to named args (`sheet = "data", key = ...`), or (b) keep the local wrapper as a thin shim that reorders args and calls `wnvSurv::gsheet_pull_prompt`. Recommend (a) — it's more transparent and matches RSE "explicit over positional" preference, but it's 10 lines across 2 active pipeline files, so do this as its own commit and re-render both qmds to confirm. |

**Action:**
1. Delete `utils/fun_clean_platemap.R` and `utils/fun_plot_pcr.R` (no call-site changes).
2. For `gsheet_pull_prompt`: update the 10 call sites to named args, then delete `utils/fun_gsheet_pull_prompt.R`.
3. Re-render `wnv-ss_weekly_report_v2.qmd` and (if feasible) `wnv-s_calc_hx.qmd` to confirm no regressions.

**Verification:** `Rscript tests/run_tests.R` (in case any of these are covered), plus a `quarto render` of the main pipeline.

---

## Step 5 — `sandbox/` and `3_output/2025/plots/` cleanup

- `3_output/2025/plots/` has 3 un-prefixed legacy files alongside the proper
  `y2025_wXX_*`-prefixed per-week archives: `hx_plot_all.png`,
  `pcr_plot.png`, `missing_weeks_hx.png`, plus a stray `.DS_Store`. These
  predate the per-week naming convention and are superseded by the
  `y2025_wXX_*` versions.

  **Action:** `git rm 3_output/2025/plots/{hx_plot_all.png,pcr_plot.png,missing_weeks_hx.png,.DS_Store}`.

- `sandbox/` has accumulated ~12MB of one-off diagnostic scripts and large
  intermediate CSVs from past debugging sessions (`culex_sheet_database_*.csv`,
  `week_baseline_diff_*`, `diag_*.R`, `check_*.R`, `verify_*.R`, etc.), plus
  this session's test artifacts (`check_trap_status.R`, `test_trap_status*.png`,
  `test_2026_*.png`).

  **Action:** Remove this session's test PNGs/scripts
  (`sandbox/check_trap_status.R`, `sandbox/test_trap_status.png`,
  `sandbox/test_trap_status2.png`, `sandbox/test_2026_only.png`,
  `sandbox/test_2026_zoom.png`, `sandbox/test_2026_fixedlimits.png`) since
  they were purely for this session's debugging and the underlying bug fix
  has been applied upstream in `wnvSurv`.

  For the older ~12MB of scratch CSVs/scripts: **flag for user review rather
  than auto-delete** — `sandbox/` is explicitly the "exploratory scripts"
  staging area per `CLAUDE.md`, and some of these (e.g.
  `culex_sheet_database_expand_NEW.csv`) may still be referenced by ongoing
  work ([[db-update-blank-key-duplicates]]). Recommend the user does a quick
  pass and tells Claude which specific files are safe to remove in a follow-up
  turn, rather than bulk-deleting now.

**Verification:** `git status` after each `git rm` to confirm only the intended files are staged.

---

## Out of scope / follow-on items (not part of this plan)

1. **Rename `utils/` → `R/` + switch to `devtools`/package-style loading**
   (TODO items: "rename utils to proper naming /R", "keep weekly report
   specific functions in R/ and call using more standard methods like
   devtools"). This requires a `DESCRIPTION` file, restructuring
   `config/load_packages.R`, and updating every `source()`/function-loading
   call across `*.qmd` and `tests/`. Needs its own plan with stop-gates.

2. **`pipelines/calc_indiv_stats.qmd` and `pipelines/config_weekly.RData`** —
   not audited in this pass; worth a follow-up check for staleness similar to
   the multiweek pipeline.

3. **Bug #4 from this session (trap_status "other spp" vs "no culex" naming
   mismatch in `plot_n_trap`)** — still pending a decision between extending
   `wnvSurv::plot_n_trap()`'s case_when vs. renaming `"other spp"` →
   `"no culex"` in `utils/fun_clean_wnv_s.R:354`. Unrelated to this cleanup;
   should be picked up separately.

---

## Suggested commit grouping

- Commit 1: Step 1 (delete 4 unused functions) + Step 2 (move gdrive_download)
- Commit 2: Step 3 (remove dead `pipelines/wnv-s_multiweek_report_pipeline.qmd`)
- Commit 3: Step 4 (wnvSurv migrations — clean_platemap/plot_pcr/plot_std first,
  then gsheet_pull_prompt as a separate sub-commit since it touches call sites)
- Commit 4: Step 5 (3_output/2025 + sandbox session-artifact cleanup)

Each commit should be followed by `Rscript tests/run_tests.R` and, for Step 4,
a `quarto render` of the main pipeline.
