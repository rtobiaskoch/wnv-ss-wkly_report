---
name: new-week
description: Walk through the weekly WNV report production workflow. Use at the start of a new surveillance week or when asked to process a new week of data.
disable-model-invocation: true
---

Weekly production checklist for the WNV surveillance pipeline.

## 1. Upload input data to `1_input/wXX/`

Verify these four subdirectories are populated for the new week before running anything:

- `1_input/wXX/datasheet/` — pool-level surveillance sheet (.csv or .xlsx)
- `1_input/wXX/pcr/` — Quantstudio3 `.xls` export
- `1_input/wXX/platemap/` — well-to-pool-ID lookup
- `1_input/wXX/all_species/` — trap-level species counts

## 2. Handle PCR edge cases before running

Manually review inconclusive PCR results in Thermofisher Quantstudio software. These must be resolved before pipeline execution — the pipeline expects clean inputs and will not handle ambiguous results gracefully.

Check for upstream data entry errors in the datasheet. These are the most common source of unexpected pipeline results.

## 3. Generate config

```bash
bash config/run_config.sh --week <N> --year <YYYY> --download T --update F --push F
```

- Adjust `--week` and `--year` to the current week.
- `--update F` prevents writing to the master Google Sheet until output is validated.

## 4. Render the pipeline

```bash
quarto render wnv-s_weekly_report_pipeline_v2.qmd
```

## 5. Validate outputs

Check `3_output/` for the expected files:
- Excel report: `y{year}_w{week}_weekly_report_output.xlsx`
- Plots: `3_output/plots/y{year}_w{week}_*.png`

Key checks:
- **Trap zeros vs. NAs**: Traps that ran with zero catch should show `0` in abundance, not `NA`.
- **Zone coverage**: All zones in `zone_lvls` (`NW`, `NE`, `SE`, `SW`, `FC`, `LV`, `BE`, `BC`) should appear in outputs.
- **Virus Index**: Values above `vi_threshold` (default 0.75) indicate elevated WNV risk and should be interpretable in context.
- **Historical comparison**: Current-week values should appear alongside historical range in plots.

## 6. Push to Google Sheet (after validation)

Re-run config with `--update T` to write validated results back to the master Google Sheet:

```bash
bash config/run_config.sh --week <N> --year <YYYY> --download F --update T --push F
quarto render wnv-s_weekly_report_pipeline_v2.qmd
```

## 7. Commit

```bash
git add .
git commit -m "weekly commit after report generation for the week"
```
