# Plan — Merge / Join Silent-Failure & Duplication Remediation

**Author:** Claude (navigator) + Toby (driver)
**Date:** 2026-05-31
**Scope:** Silent failures and code duplication in the merge / `natural_join` paths of the WNV-S weekly pipeline.

## How this plan runs

Each finding is its **own phase**. Every phase ends with a **HARD STOP**: I run the
stated verification, report the result to you, and **wait for your explicit sign-off**
before touching the next phase. Nothing in a later phase is started early.

Global rules (from CLAUDE.md):
- Any change to a function in `utils/` gets a matching test added/updated in `tests/testthat/`.
- After each phase, run `Rscript tests/run_tests.R` and confirm green before the hard stop.
- Smallest change that fixes the finding — no unrelated refactors inside a phase.
- I explain *why* before editing; you approve before I write.

Phases are ordered low-risk → high-risk so the isolated, unit-testable fixes land
first and the render-path refactor lands last.

---

## Code-review note (carried alongside this plan)

The only working-tree code change (`0_R/generate_report.R`, the `if(update)` gate)
reviewed **clean** — no correctness bugs. One architectural inconsistency surfaced:
`0_R/` is documented as *legacy, do not modify*, but `generate_report.R` and `tables.R`
are `source()`-d in production (`qmd:762`, `qmd:768`). **Decision needed from you**
(tracked in Phase 0): update the CLAUDE.md doc, or migrate these two files out of `0_R/`.

---

## Phase 0 — Documentation reconciliation (no code)

**Finding:** CLAUDE.md says `0_R/` is legacy/do-not-touch; the pipeline sources two
files from it.

**Change:** Either (a) amend CLAUDE.md "Active vs Legacy" table to carve out the two
live files, or (b) move them to an active dir (e.g. `utils/report/`) and update the two
`source()` paths. No behavior change either way.

**Verification:** `grep` confirms the two `source()` paths resolve; render still finds them.

**HARD STOP:** You choose (a) or (b). Nothing else proceeds until decided.

---

## Phase 1 — `check_data()` bug fixes  (lowest risk, isolated)

**Findings:**
1. `utils/fun_check_data.R:183` — `neg = cq_data %>% …` reads the **global** `cq_data`
   instead of the parameter `df` (the parallel `pos` block at `:174` correctly uses `df`).
   Pure-function violation + wrong-data check.
2. `utils/fun_check_data.R:56-58` — the per-zone missing-trap tally is computed then
   **discarded** (never assigned/printed/returned).

**Change:**
- Line 183: `cq_data` → `df`.
- Lines 56-58: assign the tally and `print()` it (or `cat()` a formatted summary), so the
  zone breakdown actually surfaces.

**Verification:**
- New test `tests/testthat/test-check_data.R`: call `check_data()` with a `df` that has
  known negative-control rows and assert the negative-control branch reads from `df`
  (e.g. it does **not** error when no global `cq_data` exists). Add a fixture with a
  missing trap and assert the zone tally is emitted.
- `Rscript tests/run_tests.R` green.

**HARD STOP:** Show you the diff + passing tests. Confirm the negative-control logic still
matches your intended QC semantics before continuing.

---

## Phase 2 — Surface silent row drops in joins  (low risk)

**Findings:** `merge_pcr_platemap.R:8` (`filter(!is.na(csu_id))`) and
`merge_4_database.R:24` (`filter(!is.na(trap_id))`) drop unmatched rows with **no count
or warning** — PCR results for samples missing upstream vanish silently.

**Change:** Before each `filter`, compute the dropped set (`anti_join` or
`filter(is.na(...))`), and `message()`/`warning()` the count + the offending IDs. Mirror
the existing good pattern in `check_data.R:45`. No rows are *kept* that weren't before —
this only adds reporting.

**Verification:**
- New tests `test-merge_pcr_platemap.R` / extend a merge test: feed a PCR row with no
  platemap match, assert (a) output row count unchanged from current behavior, (b) a
  message/warning is emitted naming the dropped `csu_id`.
- `Rscript tests/run_tests.R` green.

**HARD STOP:** Confirm the warning channel (message vs warning vs a written CSV like
`check_data` does) matches how you want to be alerted during a weekly run.

---

## Phase 3 — Lift magic numbers into config  (low risk)

**Findings:** `merge_4_database.R:20-21` hardcodes the missing-`cq` sentinel `55.55` and
the missing-`copies` fill `0`. Violates RSE principle #3 (parameters in `config/`, not `R/`),
and the silent `copies→0` collides with your "NA ≠ 0" gotcha.

**Change:** Add `cq_missing_sentinel` (and a documented copies-fill) to `config/config_weekly.R`,
thread through `merge_4_database()` as arguments with the current values as defaults.
No numeric behavior change at default settings.

**Verification:**
- Extend merge test: assert default sentinel is `55.55`, and that overriding the arg
  changes the fill — proves it's parameter-driven.
- `Rscript tests/run_tests.R` green.

**HARD STOP:** Confirm the config key names and whether `copies` NA should stay `0` or
become a tracked sentinel (this is a **methods decision** — I will not pick for you).

---

## Phase 4 — Investigate the `full_join` duplication  (needs real data; diagnostic first)

**Finding:** `merge_4_database.R:19` joins on only `c("csu_id","year","week")`; call site
comments `#causing duplicates` (`qmd:400`). Replicate PCR wells → many-to-many → row
inflation, masked by a downstream `distinct()` that arbitrarily keeps one replicate.

**Change (staged):**
1. **Diagnose only** — write a throwaway script against the current `w33` inputs that
   reports join cardinality: how many `csu_id` have >1 row in `cq` after the virus filter,
   and how many output rows the `distinct()` collapses. **No edits yet.** Present numbers.
2. **Then** decide jointly: add the missing key column(s) to the `by=` (e.g. `plate`/`spp`),
   switch to `relationship = "many-to-one"` to make dplyr error on violation, or formalize
   replicate-collapsing as an explicit pre-aggregation with a documented rule.

**Verification:** Diagnostic numbers reviewed with you **before** any code change; then a
test encoding the agreed cardinality rule.

**HARD STOP (two of them):** (1) after diagnosis, before choosing a fix; (2) after the fix,
before continuing. This is the highest-correctness-risk item — slowest, most checkpoints.

---

## Phase 5 — Dedup the 4× coalesce-merge  (highest blast radius; render-path)

**Finding:** The same coalesce-merge (natural_join FULL → `select(all_of)` → re-cast
`trap_date` → `arrange(desc)` → write) is written **four times**:
`qmd:421-428`, `qmd:456-463`, `qmd:564-573`, and inside `update_gsheet.R:34-48`.

**Change:** Extract one function `coalesce_to_database(new, old, by, cols)` in `utils/`.
`update_gsheet()` calls it for the push path; the three offline `else` branches call it for
the local path. Single source of truth for merge semantics. (Optional, separate decision:
replace `rquery::natural_join` with `dplyr::rows_upsert()` to drop the fragile factor→integer
coercion and the `rquery` dependency — **methods change, your call**, kept out of this phase
unless you opt in.)

**Verification:**
- Extend `test-update_gsheet.R` to cover the extracted helper directly.
- **Full pipeline render** in offline mode (`--update F`) per CLAUDE.md "run end-to-end
  after refactoring." Diff the three output CSVs (`fn_database_update`,
  `fn_database_slev_update`, `fn_database_culex_update`) against a pre-refactor run — must
  be **byte-identical**.
- `Rscript tests/run_tests.R` green.

**HARD STOP:** Show byte-identical CSV diffs + passing tests before declaring done. If the
optional `rows_upsert` swap is on the table, that's a **separate** hard stop with its own
before/after comparison.

---

## Phase 6 (optional) — Cross-file duplicate  (trivial; only if you want it)

**Finding:** `df_all = hx_wide %>% left_join(ytd_wide, by = "week")` is verbatim in
`qmd:735` and `wnv-s_calc_hx.qmd:116`.

**Change:** Factor into a tiny named helper or confirm one is the dead copy and remove it.

**Verification:** Render both docs; outputs unchanged.

**HARD STOP:** Confirm which file is authoritative.

---

## Sequencing summary

| Phase | Item | Risk | Verification gate |
|------|------|------|------------------|
| 0 | Doc reconciliation | none | grep paths resolve |
| 1 | `check_data()` bugs | low | unit tests |
| 2 | Join drop-counts | low | unit tests |
| 3 | Magic numbers → config | low | unit tests |
| 4 | `full_join` key | **high** | diagnostic + tests (2 stops) |
| 5 | Dedup 4× merge | **high** | end-to-end render, byte-identical CSVs |
| 6 | Cross-file dup | trivial | render unchanged |

Each row is a hard stop. We do not cross a row boundary without your sign-off.
