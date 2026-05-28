---
name: run-pipeline
description: Run the WNV weekly pipeline. Use when asked to run the pipeline, generate the weekly report, or render the Quarto document.
---

The WNV pipeline has two required steps that must run in order.

## Step 1: Generate config

```bash
bash config/run_config.sh --week <WEEK> --year <YEAR> --download T --update F --push F
```

- Replace `<WEEK>` with the ISO week number and `<YEAR>` with the 4-digit year.
- `--download T` fetches fresh files from Google Drive. Use `--download F` for offline runs or re-runs on existing files.
- Leave `--update F` until the output has been validated — `T` writes to the master Google Sheet.
- Config saves a timestamped `.RData` to `config/config_weekly_settings/`. The QMD loads the most recent file; verify the timestamp matches the current week before rendering.

## Step 2: Render the pipeline

```bash
quarto render wnv-s_weekly_report_pipeline_v2.qmd
```

- Final outputs go to `3_output/` (Excel report, CSVs, PNGs).
- Intermediate checkpoints go to `2_mid/` (named `y{year}_w{week}_*`).

## Troubleshooting

| Symptom | Fix |
|---|---|
| Pipeline loads wrong week's parameters | Re-run `run_config.sh` with the correct `--week`/`--year` |
| Google auth failure | Run `googledrive::drive_auth()` interactively in RStudio first |
| `calc_pir()` error or missing function | `devtools::install_github("CDCgov/PooledInfRate")` |
| Config stops with "directories are empty" | Verify `1_input/w{week}/datasheet/`, `pcr/`, `platemap/`, and `all_species/` are populated |
| Unexpected grouping results | Confirm zone columns were passed through `wnv_s_clean()` to enforce `zone_lvls` factor |
