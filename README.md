---
editor_options: 
  markdown: 
    wrap: 80
---

# wnv-ss-wkly_report


Workflow Diagrams can be found here:
<https://docs.google.com/drawings/d/1UVtAzJrcSCYUQgDfj5BYL3X9ZKuU0JnDyr0SHTLvAag/edit?usp=sharing>\

# TL;DR Run the Pipeline

```bash
#download data from googledrive and drop it into 1_input/YYYY/
# Keep --update F / --push F until the run is validated
Rscript config/config_weekly.R --year 2026 --week 29 --download T --update F --push F
quarto render wnv-ss_weekly_report_v2.qmd
#VALIDATE RESULTS VISUALLY
Rscript config/config_weekly.R --year 2026 --week 29 --download F --update T --push T
```



## NAVIGATING THE REPOSITORY

| Path | Description |
|------|-------------|
| **wnv-ss_weekly_report_v2.qmd** | Where the magic happens. Calls all functions in `utils/` |
| **utils/** | Contains all functions called in the qmd file |
| **config/** | Contains `config_weekly.R` and `load_packages.R` |
| **pipelines/** | Secondary qmd pipelines (`wnv-s_calc_hx.qmd`, `calc_indiv_stats.qmd`) |
| **scripts/** | Standalone helper scripts (`fun_gdrive_download.R`, `patch_pcr_quantity.R`) |
| **tests/** | testthat unit tests; run with `Rscript tests/run_tests.R` |
| **docs/** | Project documentation, including `docs/claude/` context files |
| **1_input/** | Raw weekly input files downloaded from Google Drive |
| **2_mid/** | Intermediary files that are useful for debugging |
| **3_output/** | Final output files used for reporting |

---
# S E T - U P 
---

## STEP I: Install Software and Clone Repo

-   install R if you do not already have it installed:
    <https://cran.r-project.org/bin/macosx/>

-   install quarto if you do not already have it installed
    <https://quarto.org/docs/get-started/>

-   install repository open up your terminal and navigate to a place you would
    like to install the repository and run code below

``` bash
git clone https://github.com/rtobiaskoch/wnv-ss-wkly_report.git
```

## STEP II: cd to repo that you just installed

``` bash
cd path/to/wnv-ss-wkly_report
```

## STEP III: Install/Load R packages

-   in repo directory wnv-ss-weekly_report install packages

``` r
source("config/load_packages.R")
```


## STEP IV: Authorize Googledrive API


-   The pipline utilizes googledrive API for R to download & update databases
    related to the pipeline.

-   You will need to authorize your email account by installing and running the
    following:\

``` r
install.packages(googledrive)
library(googledrive)
drive_auth()
```



## STEP V: Run Tests


Before trusting a run (and after changing any function in `utils/`), verify the
pipeline still works:

``` bash
# Unit tests — calc functions, cleaning, report building (fast, no data needed)
Rscript tests/run_tests.R

# Smoke test — renders the full pipeline for a known prior week (SMOKE_WEEK/YEAR,
# currently w33 2025) and asserts all output files are produced and VI values
# match the committed golden reference. Exit code 0 = pass, 1 = fail.
Rscript tests/smoke_test.R
```

-   The smoke test requires `1_input/2025/w33/` to be populated and `quarto` on
    your PATH. It writes to `tests/smoke_out/` and `tests/smoke_mid/`, leaving
    the live `3_output/` and `2_mid/` untouched.

-   Only re-run `Rscript tests/smoke_test.R --setup` to regenerate the golden
    reference after you *intentionally* change expected outputs, then commit the
    updated golden file.


---

# R U N N I N G  - T H E -  P I P E L I N E 
---


## STEP 1: Upload Weekly Files to Google Drive


-   Using the link below navigate to the year and week you are trying to
    process:
    <https://drive.google.com/drive/folders/1VC1i8D-2_8WEapmW2hrBqMzdHz3vsbqW?usp=sharing>

-   Download the w## folder into the 1_input/<current_year>

-   If the files aren't present for the week message Greg to have him foward you
    the email.

-   files should contain the following folders:

    -   **all_species:** trap level data we use to calculate the abundance

    -   **datasheet:** pool level data from traps we use to calculate PIR and
        test

    -   **pcr:** Quantstudio3 results that are the output from the qPCR test. needs to be named ss_y<YEAR>_w<WEEK>_p<PLATE #> example ss_y2026_w27_p1.xslx
    -   **platemap:** a completed version of the template below that provides
        the platemap that links pcr results to our csu_id in the datasheets. The
        pcr name MUST CONTAIN A 4 DIGIT YEAR AND TWO DIGIT WEEK AND A 1 DIGIT
        PLATE #: example: SS_y2025_w28_p1_rna_extract_and_pcr_notebook
        [[template]
        SS_y####\_w##\_p#\_rna_extract_and_pcr_notebook](https://docs.google.com/spreadsheets/d/1Uzqa316NGH-0WLR6WBUmLDND4-BEZ_Ga9O1PJlmxoNY/edit?usp=drive_link){.uri}

    -   eds file from the qPCR output

-   note: you may need to change Boulders week to match the year to date week
    #\*

-   if all files are present in google drive download it into 1_input/<YEAR>/ and unzip
    the file.


## STEP 2: Check the Input Data


-   You may need to change Boulders week to the current reporting week. They
    start at 1 instead of week 23.

-   **Check Inconclusive Amp Status:**

    -   upload the .eds file output from the qPCR to thermofishers online tool:
        [Quantstudio
        Software](https://www.thermofisher.com/us/en/home/technical-resources/software-downloads/quantstudio-3-5-real-time-pcr-systems.html){.uri}

        -   You may need to create an account. The software is free

    -   Find samples with inconclusive amp status. Check the multicomponent plot

    -   If the multicomponent plot shows an increasing curve the sample is
        likely positive.

    -   if it is flat, and the sample has a copy number of greater than 500 you
        will need to manually change the test_code result by adding that csu_id
        to
        [amp_inconclusive_negatives](https://docs.google.com/spreadsheets/d/1l90THbcNgUgdO6dUbFXYo6IxVc3VwutKVlx3Cu9cgEQ/edit?usp=drive_link)


## STEP 3: Run Configuration


run script below to generate a configuration file

-   change the name of the input folder and the week \# in run_config.sh

``` bash
bash config/run_config.sh 

#OR you can edit the arguments without editing any files
Rscript config/config_weekly.R --year 2026 --week 24 --download T --update F --push F
```

-   settings are saved to a dynamically named file to the
    config/config_weekly_settings/ Chunk: load_config in the qmd file will read
    in the latest config file




## STEP 4: Run the Pipeline
---------------------

-   run the following command in your terminal after running your config.

``` bash
quarto render wnv-ss_weekly_report_v2.qmd
```


## STEP 5: Check the Report


- Check the final report: 3_output/<YEAR>/w<WEEK>/y<YEAR>_w<WEEK>_weekly_report_output.xlsx
- Navigate to the googlesheet: 2026 example https://drive.google.com/drive/u/0/folders/1Z2SdJeMVYsWIjIedUakYqUo5DAEtKzlx

-   TELL GREG "I DID A GOOD JOB" AND GIVE HIM THE LINK TO THE REPORT 



## STEP *: COMMIT TO GITHUB (OPTIONAL)



-   In order to help track the state of the pipeline that produced the results
    for each week. save the changed to github\

```{bash}
git add --all
git commit -m "weekly commit after report generation for the week"
git push
```
