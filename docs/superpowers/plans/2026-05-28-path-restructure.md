# Path Restructure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add year/week layers to `1_input/`, `2_mid/`, and `3_output/` so pipeline runs are isolated by year and week, and co-locate the config snapshot with the input data it describes.

**Architecture:** Auto-construct all four directory paths from `--year` and `--week` CLI args in `config_weekly.R`. Hard-stop if `dir_input` is missing (user must populate it). Auto-create `dir_mid` and `dir_output` if absent. Save a second config snapshot into `dir_input`.

**Tech Stack:** R (base), argparse, `config/config_weekly.R`, `config/run_config.sh`

---

## Files Changed

| File | Change |
|------|--------|
| `config/config_weekly.R` | Path construction (lines 43, 85–86), dir check (lines 93–171), config export (lines 336–343) |
| `config/run_config.sh` | Remove `--input 1_input/w28` flag |

---

### Task 1: Restructure path construction in `config_weekly.R`

**Files:**
- Modify: `config/config_weekly.R:43-86`

Context: `dir_input` is currently set to `args$input` on line 43 (before `week_filter` and `year_filter` are defined on lines 53–54). `dir_mid` and `dir_output` are hardcoded strings on lines 85–86. The fix: rename line 43 to store the base input dir, then construct all four dirs from year/week after the filters are defined, just before the subdir definitions on lines 79–82.

- [ ] **Step 1: Replace line 43 — rename the raw arg to `dir_base_input`**

In `config/config_weekly.R`, change line 43 from:
```r
dir_input <- args$input
```
to:
```r
dir_base_input <- args$input
```

- [ ] **Step 2: Update the `cat()` on line 63 to use the new variable name**

Change line 63 from:
```r
cat("Input folder set to:", dir_input, "\n")
```
to:
```r
cat("Input base folder set to:", dir_base_input, "\n")
```

- [ ] **Step 3: Reconstruct all four dirs from year/week in the DEFINE DIRECTORIES section**

Replace lines 79–86 (the full DEFINE DIRECTORIES block):
```r
dir_datasheet <- file.path(dir_input, "datasheet")
dir_pcr <- file.path(dir_input, "pcr")
dir_platemap <- file.path(dir_input, "platemap")
dir_all_spp = file.path(dir_input, "all_species")

#OUTPUT
dir_mid <- "2_mid"
dir_output <- "3_output"
```
with:
```r
# construct week-scoped paths from year and week args
dir_input  <- file.path(dir_base_input, year_filter, paste0("w", week_filter))
dir_datasheet <- file.path(dir_input, "datasheet")
dir_pcr       <- file.path(dir_input, "pcr")
dir_platemap  <- file.path(dir_input, "platemap")
dir_all_spp   <- file.path(dir_input, "all_species")

# output dirs — auto-created later if absent
dir_mid    <- file.path("2_mid",    year_filter, paste0("w", week_filter))
dir_output <- file.path("3_output", year_filter, paste0("w", week_filter))
```

- [ ] **Step 4: Verify syntax with `Rscript --vanilla -e`**

```bash
Rscript --vanilla -e "source('config/config_weekly.R')" --args --year 2026 --week 22 --download F --update F --push F 2>&1 | head -20
```

Expected: prints path values then stops with a "directory missing" error (since `1_input/2026/w22` doesn't exist yet — that's correct behaviour). No syntax errors.

- [ ] **Step 5: Commit**

```bash
git add config/config_weekly.R
git commit -m "refactor: auto-construct year/week paths from CLI args"
```

---

### Task 2: Split directory existence check — hard-stop on input, auto-create outputs

**Files:**
- Modify: `config/config_weekly.R:93-171`

Context: The current code (lines 93–171) runs two loops — one that hard-stops on any missing directory (including `dir_mid` and `dir_output`) and one that hard-stops on empty input subdirs. Replace both loops with: (a) a hard-stop only for `dir_input`; (b) the existing empty-subdir check unchanged; (c) `dir.create(recursive = TRUE)` for `dir_mid` and `dir_output`.

- [ ] **Step 1: Replace the directory check block (lines 93–171)**

Delete the entire block from the `# Initialize a vector...` comment through the closing `}` of the empty-dir check, and replace with:

```r
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# C H E C K   F O R   M I S S I N G   D I R E C T O R I E S 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# --- Input: hard stop if missing (user must populate before running) ---
if (!dir.exists(dir_input)) {
  stop(paste0(
    "Input directory missing: ", dir_input,
    "\nCreate and populate it with datasheet/, pcr/, platemap/, all_species/ before running."
  ))
}

# --- Input subdirs: hard stop if empty or missing ---
input_subdirs <- list(
  datasheet = dir_datasheet,
  pcr       = dir_pcr,
  platemap  = dir_platemap,
  spp       = dir_all_spp
)

empty_dirs <- c()
for (dir_name in names(input_subdirs)) {
  d <- input_subdirs[[dir_name]]
  if (!dir.exists(d) || length(list.files(d)) == 0) {
    empty_dirs <- c(empty_dirs, d)
  }
}

if (length(empty_dirs) > 0) {
  stop(paste(
    "The following input subdirectories are empty or missing:\n",
    paste(empty_dirs, collapse = "\n"),
    "\nPlease populate them before running."
  ))
} else {
  print("All input subdirectories are populated.")
}

# --- Output dirs: auto-create if missing ---
for (d in c(dir_mid, dir_output)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("Created directory:", d, "\n")
  }
}
```

- [ ] **Step 2: Verify syntax**

```bash
Rscript --vanilla -e "source('config/config_weekly.R')" --args --year 2026 --week 22 --download F --update F --push F 2>&1 | head -20
```

Expected: hard-stops with "Input directory missing: 1_input/2026/w22". No syntax errors.

- [ ] **Step 3: Commit**

```bash
git add config/config_weekly.R
git commit -m "refactor: hard-stop on missing input, auto-create mid and output dirs"
```

---

### Task 3: Save config snapshot into `dir_input`

**Files:**
- Modify: `config/config_weekly.R:336-343`

Context: The config is currently exported once to `config/config_weekly_settings/`. Add a second `saveRDS()` call that writes the same object to `dir_input` so the config snapshot is co-located with the input data it describes. The archive copy is kept unchanged.

- [ ] **Step 1: Add second `saveRDS()` call after the existing one**

The current export block (lines 336–343) looks like:

```r
config_fn = paste0("config/config_weekly_settings/", file_prefix, "config_weekly_",Sys.time(), ".RData")

all_params <- ls(envir = .GlobalEnv)

# Create a list containing all the values of these objects
all_params_list <- mget(all_params, envir = .GlobalEnv)

saveRDS(all_params_list, config_fn)
```

Add one line immediately after `saveRDS(all_params_list, config_fn)`:

```r
# co-locate config with the input data it describes (traceability)
saveRDS(all_params_list, file.path(dir_input, paste0(file_prefix, "config_weekly.RData")))
cat("Config snapshot saved to:", file.path(dir_input, paste0(file_prefix, "config_weekly.RData")), "\n")
```

- [ ] **Step 2: Verify syntax**

```bash
Rscript --vanilla -e "source('config/config_weekly.R')" --args --year 2026 --week 22 --download F --update F --push F 2>&1 | head -20
```

Expected: same hard-stop on missing input dir — no syntax error. The save will execute only once the full script runs successfully.

- [ ] **Step 3: Commit**

```bash
git add config/config_weekly.R
git commit -m "feat: save config snapshot into dir_input for traceability"
```

---

### Task 4: Update `run_config.sh` — remove explicit `--input` flag

**Files:**
- Modify: `config/run_config.sh`

Context: `run_config.sh` currently passes `--input 1_input/w28` which hard-codes both the base dir and the week. Since `config_weekly.R` now constructs the full path from `--year` and `--week`, the `--input` flag should be dropped (the default `"1_input"` base dir is correct).

- [ ] **Step 1: Update the Rscript call**

Replace the contents of `config/run_config.sh`:

```bash
#!/bin/sh

Rscript config/config_weekly.R --year 2026 --week 22 --download T --update F --push F
```

Update `--year` and `--week` to the current week before each run. (This file is a template — exact values are set manually each week.)

- [ ] **Step 2: Commit**

```bash
git add config/run_config.sh
git commit -m "refactor: remove explicit --input flag from run_config.sh"
```

---

### Task 5: Integration test — run the full config against a real input directory

**Files:**
- Read: `1_input/2026/wXX/` (must exist and be populated before this task)

- [ ] **Step 1: Confirm the current week's input dir is populated**

```bash
ls 1_input/2026/w22/datasheet/ 1_input/2026/w22/pcr/ 1_input/2026/w22/platemap/ 1_input/2026/w22/all_species/
```

Expected: each subdir contains at least one file.

- [ ] **Step 2: Run the config script**

```bash
bash config/run_config.sh
```

Expected output (in order):
```
Input base folder set to: 1_input
Week filter set to: 22
Year filter set to: 2026
...
[1] "All input subdirectories are populated."
[1] "All required directories exist or were created."
Config snapshot saved to: 1_input/2026/w22/y2026_w22_config_weekly.RData
```

- [ ] **Step 3: Verify `2_mid/2026/w22/` and `3_output/2026/w22/` were created**

```bash
ls -d 2_mid/2026/w22 3_output/2026/w22
```

Expected: both directories exist.

- [ ] **Step 4: Verify config snapshot in `dir_input`**

```bash
ls 1_input/2026/w22/*config_weekly*.RData
```

Expected: one `.RData` file matching `y2026_w22_config_weekly.RData`.

- [ ] **Step 5: Verify config snapshot still in archive**

```bash
ls config/config_weekly_settings/ | grep "y2026_w22"
```

Expected: at least one timestamped `.RData` file.

- [ ] **Step 6: Commit any incidental changes (e.g. run_config.sh week update)**

```bash
git add config/run_config.sh
git commit -m "chore: set run_config.sh to week 22 2026"
```
