# Path Restructure Design — Year/Week-Scoped Directories

**Date:** 2026-05-28  
**Scope:** `config/config_weekly.R`, `config/run_config.sh`  
**TODOs addressed:** architecture #1, #2, #3 (plus `2_mid/` for consistency)

---

## Problem

All four pipeline directories (`1_input/`, `2_mid/`, `3_output/`, and the config snapshot) are either flat or rely on a manually-passed full path. As the pipeline enters its second year (2026), week numbers repeat across years, creating risk of overwrite and making it hard to isolate a run's artefacts.

---

## Goal

Each pipeline run for week W of year Y writes to and reads from isolated directories:

```
1_input/YYYY/wXX/     ← raw inputs (user-populated before run)
2_mid/YYYY/wXX/       ← intermediate RData checkpoints
3_output/YYYY/wXX/    ← final outputs: xlsx, CSVs, PNGs
```

A copy of the config snapshot is saved into `1_input/YYYY/wXX/` so inputs and the parameters that describe them are co-located.

---

## Approach: Option B — auto-construct, auto-create outputs

- `dir_input` hard-stops if missing or empty — user must populate before running.
- `dir_mid` and `dir_output` are created automatically with `dir.create(..., recursive = TRUE)` if absent.
- `dir_input` subdirs (`datasheet/`, `pcr/`, `platemap/`, `all_species/`) retain the existing emptiness check.

---

## Changes

### `config/config_weekly.R`

**Path construction (lines 43, 85–86):**

```r
# before
dir_input  <- args$input             # full path passed from shell, e.g. "1_input/w28"
dir_mid    <- "2_mid"
dir_output <- "3_output"

# after
dir_input  <- file.path(args$input, year_filter, paste0("w", week_filter))
dir_mid    <- file.path("2_mid",    year_filter, paste0("w", week_filter))
dir_output <- file.path("3_output", year_filter, paste0("w", week_filter))
```

`--input` default stays `"1_input"` (the base dir); year/week are appended automatically.

**Directory existence check (lines 93–126):**

Replace the single unified loop with split logic:
1. Check `dir_input` — hard-stop if missing.
2. Check `dir_mid` and `dir_output` — auto-create with `dir.create(..., recursive = TRUE)` if missing, print a message.

**Config snapshot export (lines 336–343):**

```r
# existing archive (kept)
config_fn <- paste0("config/config_weekly_settings/", file_prefix, "config_weekly_", Sys.time(), ".RData")
saveRDS(all_params_list, config_fn)

# new: co-locate config with the inputs it describes
saveRDS(all_params_list, file.path(dir_input, paste0(file_prefix, "config_weekly.RData")))
```

### `config/run_config.sh`

Remove the explicit `--input 1_input/w28` flag — week path is now auto-constructed:

```bash
# before
Rscript config/config_weekly.R --input 1_input/w28 --year 2025 --week 28 --download T --update F --push F

# after
Rscript config/config_weekly.R --year 2026 --week 22 --download T --update F --push F
```

---

## Files Changed

| File | Change |
|------|--------|
| `config/config_weekly.R` | Path construction, dir check logic, config export |
| `config/run_config.sh` | Remove `--input` flag |

No changes to the QMD or `utils/` — they consume config variables, not paths directly.

---

## Testing

After implementation, run:
```bash
bash config/run_config.sh --year 2026 --week 22 --download F --update F --push F
```
Verify:
- Config loads without error
- `1_input/2026/w22/` is found and populated
- `2_mid/2026/w22/` is created automatically
- `3_output/2026/w22/` is created automatically
- Config snapshot appears in both `config/config_weekly_settings/` and `1_input/2026/w22/`
