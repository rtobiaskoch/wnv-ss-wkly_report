# Plan: Formal Testing Environment (Phase 1)

## Context

The pipeline has 36 utility functions and zero formal tests. The only testing that exists is `0_R/pir_test.R`, which is exploratory code — not a test suite. Before the pipeline can be safely refactored or extended, the three highest-risk epidemiological calculation functions need unit tests: `calc_abund()`, `calc_pir()`, and `calc_vi()`. These functions produce the numbers that go into public health reports. A wrong abundance or PIR value could go undetected without automated checks.

The existing `test/` directory contains real w23 surveillance data (Excel/XLS files). Per the user's decision, these are kept as-is and labeled "integration fixtures only." All new unit tests use synthetic inline data — no real data, no Excel files.

## Scope (this session)

- Fix one hard blocker in `calc_pir()` that prevents it from running in a test context
- Create `tests/` directory structure with testthat infrastructure
- Write 3 test files: `test-calc_abund.R`, `test-calc_pir.R`, `test-calc_vi.R`
- Verify: `testthat::test_dir("tests/testthat/")` runs and all tests pass

Google Sheets mocking and tests for `wnv_s_clean()`, `clean_pcr()`, `key_rename()` are Phase 2.

---

## Step 0 — Fix the blocker in `calc_pir()`

**File:** `utils/fun_calc_pir.R` lines 9–18

**Problem:** The function calls `install.packages("tidyverse")` and `devtools::install_github()` inside the function body. This causes any test runner (or CI environment) to attempt package installation mid-test. It must be removed.

**Fix:** Replace the `!require()` / `install.packages()` block with a clean dependency check:

```r
# Remove lines 9–18 entirely. Replace with:
if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
  stop("PooledInfRate is required. Install with: devtools::install_github('CDCgov/PooledInfRate')")
}
```

The tidyverse check is unnecessary — it is loaded by the pipeline config. The PooledInfRate check becomes a clear error message rather than a silent install attempt.

---

## Step 1 — Create directory structure

```
tests/
  run_tests.R                    <- one-command test runner
  fixtures/
    generate_fixtures.R          <- stub; populated in Phase 2 for CSV-based tests
  testthat/
    helper-setup.R               <- loads packages + sources calc utils
    test-calc_abund.R
    test-calc_pir.R
    test-calc_vi.R
```

The existing `test/` (real w23 data) is untouched.

---

## Step 2 — Write `tests/testthat/helper-setup.R`

This file is auto-sourced by testthat before any `test-*.R` file runs.

**Responsibilities:**
1. Load all packages needed by the three calc functions
2. Source only the utility files needed (not gdrive/gsheet — those require OAuth)
3. Define `fixture_dir` path for future CSV-based tests

**Packages to load:** `dplyr`, `tidyr`, `rlang`, `tibble`, `lubridate`, `PooledInfRate`

**Utils to source (in dependency order):**
- `utils/fun_add_missing_x.R` — used by `calc_abund()` and `calc_vi()`
- `utils/fun_rm_grp_na.R` — used by `calc_vi()`
- `utils/fun_trap_complete.R` — used by `calc_pir()` for zone completion
- `utils/fun_calc_abund.R`
- `utils/fun_calc_pir.R` (after blocker fix)
- `utils/fun_calc_vi.R`

Use `here::here()` for all paths so tests can be run from any working directory.

---

## Step 3 — Write `tests/run_tests.R`

```r
testthat::test_dir(here::here("tests/testthat/"), reporter = "progress")
```

Runnable interactively (`source("tests/run_tests.R")`) or via `Rscript tests/run_tests.R`.

---

## Step 4 — Write `tests/testthat/test-calc_abund.R`

All test data defined inline as `tibble::tribble()` — no CSV fixture needed.

**Minimal input schema required by `calc_abund()`:**
`trap_id, zone, zone2, year, week, spp, method, trap_status, total`

**Test cases:**

| Test | What it checks |
|------|---------------|
| Returns expected zones and species groups | Basic grouping is correct |
| `abund = mosq_L / trap_L` | Core arithmetic is correct |
| Traps with `method != "L"` are excluded | Gravid/CDC traps don't inflate count |
| Traps with `trap_status == "malfunction"` are excluded | Malfunctions don't contribute |
| `abund_lci` is never negative | CI floor is enforced |

**Synthetic data structure:**
- 2 zones (NW, SE), 2 species (Tarsalis, Pipiens), 2 traps per zone
- One malfunction row to verify exclusion
- One non-L method row to verify exclusion

---

## Step 5 — Write `tests/testthat/test-calc_pir.R`

**Minimal input schema required by `calc_pir()`:**
`csu_id, trap_id, zone, zone2, method, spp, total, test_code, year, week`
(`zone2` must be present as a column even if not in `grp_var`)

**Test cases:**

| Test | What it checks |
|------|---------------|
| All-negative pools → PIR = 0 | Zero-infection baseline works |
| One positive pool → PIR > 0 | Positive detection registers |
| Single-mosquito pool (`total == 1`) doesn't error | Edge case handled without stop() |
| Input with `csu_id` in `grp_var` throws error | Validation guard works |
| Missing required column throws error | Input validation catches schema mismatch |

**Synthetic data:** 3 pools — 1 positive (test_code=1, total=15), 2 negatives (test_code=0, total=10 and 8). All same zone/week/year/spp for simplicity.

---

## Step 6 — Write `tests/testthat/test-calc_vi.R`

`calc_vi()` takes the outputs of `calc_abund()` and `calc_pir()` as inputs — test it by composing the two upstream functions on the same synthetic data, then passing their outputs to `calc_vi()`. This tests the composition, not just the arithmetic.

**Test cases:**

| Test | What it checks |
|------|---------------|
| `vi = pir * abund` for matching groups | Core arithmetic is correct |
| Groups present in abund but not pir produce vi = 0 | Missing PIR groups don't cause NA propagation |
| Output contains vi, vi_lci, vi_uci columns | Schema is correct |

---

## Step 7 — Verification

Run from the R console at the project root:

```r
source("tests/run_tests.R")
```

Expected output:
- 3 test files detected
- All tests pass (no failures, no errors)
- `calc_pir()` does not attempt to call `install.packages()` during the run

Also verify the blocker fix directly:
```r
source("utils/fun_calc_pir.R")   # should source without installing anything
```

---

## Files Modified

| File | Change |
|------|--------|
| `utils/fun_calc_pir.R` | Remove `install.packages()` block (lines 9–18); add `requireNamespace()` guard |

## Files Created

| File | Purpose |
|------|---------|
| `tests/run_tests.R` | One-command test runner |
| `tests/fixtures/generate_fixtures.R` | Stub for Phase 2 CSV fixtures |
| `tests/testthat/helper-setup.R` | Package loading + utils sourcing |
| `tests/testthat/test-calc_abund.R` | Tests for `calc_abund()` |
| `tests/testthat/test-calc_pir.R` | Tests for `calc_pir()` |
| `tests/testthat/test-calc_vi.R` | Tests for `calc_vi()` |

## Not in Scope (Phase 2)

- `wnv_s_clean()`, `key_rename()`, `clean_pcr()` tests (need CSV fixtures)
- Google Sheets mock (needs dependency injection refactor of `gsheet_pull_prompt()`)
- `tests/fixtures/generate_fixtures.R` fully populated with synthetic CSVs
- testthat integration with CI
