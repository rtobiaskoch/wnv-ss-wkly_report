---
editor_options: 
  markdown: 
    wrap: 80
---

# wnv-ss-wkly_report

## ----------------------------------------------------------------------

Version Notes:\
\##
---------------------------------------------------------------------------------------

V2 config file now run separately from qmd pipeline file. This allows all
options to be set once. Then user can run pipeline and make any adjustments
without having to specify the commands each time.\
\
V2 also now runs code as functions instead of scripts. This allows for more
flexibility and reusability of code. For example wnv-s_clean() is designed as a
universal cleaning function to ensure all data follows the same formatting
rules.

Indepth notes can be found here:
<https://docs.google.com/document/d/1gUdxBmV-fIB8R1mVgvBGPy5UIVws_OQNCkH_ia1f8vQ/edit?tab=t.0>\

## ---------------------------------------------------------------------------------------

## NAVIGATING THE REPOSITORY

## ---------------------------------------------------------------------------------------

wnv-s_weekly_reportpipeline_v2.qmd: where the magic happens. calls all function
in utils/ utils/: contains all functions called in qmd file config/: contains
config_weekly.R file and load_packages\_.R 2_mid/: contains intermediary files
that are useful for debugging 3_output/: contains the final output files are
used for reporting

# V2

## -----------------------------------------------------------------------------

## STEP 1: Installation

## ----------------------------------------------------------------------------

-   install R if you do not already have it installed:
    <https://cran.r-project.org/bin/macosx/>

-   install quarto if you do not already have it installed
    <https://quarto.org/docs/get-started/>

-   install repository open up your terminal and navigate to a place you would
    like to install the repository and run code below

``` bash
git clone https://github.com/rtobiaskoch/wnv-ss-wkly_report.git
```

## STEP 2: cd to repo that you just installed

``` bash
cd path/to/repo
```

## -------------------------------------------------------------------------

## STEP 2: Install R packages

## -------------------------------------------------------------------------

-   in repo directory wnv-ss-weekly_report install packages

``` bash
source("config/install_packages.R")
```

## ---------------------------------------------------------------------------

## STEP 3: Download Weekly Files

## ---------------------------------------------------------------------------

-   Using the link below navigate to the year and week you are trying to
    process:
    <https://drive.google.com/drive/folders/1VC1i8D-2_8WEapmW2hrBqMzdHz3vsbqW?usp=sharing>

-   files should contain the following folders all_species: trap level data we
    use to calculate the abundance datasheet: pool level data from traps we use
    to calculate PIR and test pcr: Quantstudio3 results that are the output from
    the qPCR test platemap: a completed version of the template below that
    provides the platemap that links pcr: results to our csu_id in the
    datasheets. the pcr name MUST CONTAIN A 4 DIGIT YEAR AND TWO DIGIT WEEK AND
    A 1 DIGIT PLATE #: example: SS_y2025_w28_p1_rna_extract_and_pcr_notebook
    <https://docs.google.com/spreadsheets/d/1KOq6ARqPM9J1V7lNUnsB07vC-UEVcEIyl9dVLWEPpas/edit?usp=drive_link>

-   note: you may need to change Boulders week to match the year to date week
    #\*

-   if all files are present download to your repository root folder and unzip
    the file.

## --------------------------------------------------------------------------

## STEP 4: Run Configuration

## ------------------------------------------------------------------------

-   run script below to generate a configuration file
-   change the name of the input folder and the week \#

``` bash
Rscript config/config_weekly.R \
--input <w##> \          #input directory where weekly input data is located
--year <####> \          #year filter to ensure correct data
--week <##> \          #week filter to ensure correct data
--year_hx 2019 \       #how far back hx data is calculated
--cp_threshold 500 \   #threshold for calling positives
--download T \         #logical to download files from google drive or not
--update T           #logical whether to update gsheet databases with weekly data
```

-   settings are saved to a dynamically named file to the
    config/config_weekly_settings/ Chunk: load_config in the qmd file will read
    in the latest config file

## ---------------------------------------------------------------------------

## STEP 4: Authorize Googledrive API

## --------------------------------------------------------------------------

The pipline utilizes googledrive API for R to download & update databases
related to the pipeline.

You will need to authorize your email account by installing and running the
following:\

``` r
install.packages(googledrive)
library(googledrive)
drive_auth()
```

## ---------------------------------------------------------------------------

## STEP 5: Run the Pipeline

## --------------------------------------------------------------------------

-   run the following command in your terminal after running your config.

``` bash
quarto render wnv-ss_weekly_pipline.qmd
```

or

``` bash
Rscript -e "rmarkdown::render('pipeline/wnv-s_multiweek_report_pipeline.qmd')"
```

## --------------------------------------------------------------------------------

## STEP 5: Edit the Report

## ----------------------------------------------------------------------------------

Navigate to the YY_weekly_report where the output file is generated:
<https://drive.google.com/drive/folders/1VC1i8D-2_8WEapmW2hrBqMzdHz3vsbqW?usp=drive_link>

YYYY -\> YY_weekly_report -\> weekly_report_R_output

add this silly graphs sheet to the report that the city used and copy and paste
the appropriate table to the corresponding table in the graph tab.

<https://docs.google.com/spreadsheets/d/1E-imgI_WWU5Pnw9gDPtitONWqbG0yLKeQqIlHNCLtLI/edit?usp=drive_link>

TELL GREG I DID A GOOD JOB HERE IS THE REPORT.