---
name: run-tests
description: Run the WNV pipeline unit tests. Use when asked to run tests, check if tests pass, or verify changes to calc functions in utils/.
---

Run all unit tests:

```bash
Rscript tests/run_tests.R
```

## What's covered

| Test file | Functions tested |
|---|---|
| `test-calc_abund.R` | `calc_abund()` — 9 tests for abundance (count/trap-night ± 95% CI) |
| `test-calc_pir.R` | `calc_pir()` — pooled infection rate via CDC MLE (Firth + bootstrap) |
| `test-calc_vi.R` | `calc_vi()` — virus index (PIR × abundance) |

Fixtures use synthetic data from `tests/fixtures/generate_fixtures.R`.

## Interpreting failures

- **`calc_pir` errors or missing `PooledInfRate`**: Install with `devtools::install_github("CDCgov/PooledInfRate")`.
- **Zone-related failures**: Usually a factor level mismatch — verify `zone_lvls` is defined and `wnv_s_clean()` was called on the input before the calc function.
- **NA vs 0 mismatches in `calc_abund`**: `NA` means trap not run; `0` means trap ran with zero catch. These are intentionally different. Check test fixture expectations before changing function behavior.

## Extending tests

When adding or modifying a function in `utils/`, add or update a test file at `tests/testthat/test-{function_name}.R`. Use the existing test files as templates. Fixtures should use `tests/fixtures/generate_fixtures.R` as the pattern for synthetic data generation.
