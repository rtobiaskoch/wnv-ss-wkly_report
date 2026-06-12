

## Create Testing Environment
- [x] create simple synthetic test dataset and pipeline that won't impact the live data (in process)
- [x] create unit tests to verify pipeline that is isolated from live environment
- [x] fix generate_report.R not making the xlsx file in the smoke_test.R
- [x] have smoke_test use pipeline but have a special I/O folders to isolate from the pipeline

## architecture
- [x] to prevent any overwriting between years paths include the year eg. 1_input/2026/w25
- [x] save the config in the 1_input/YYYY/wXX folder
- [x] make a matching output directory for the week 3_output/YYYY/wXX
- [x] change the argument for historical to be the previous 5 year rolling window as the default not setting a specific year.
- [ ] update functions from R wnv-ss-functions (wnvSurv)
- [ ] keep weekly report specific functions in R/ and call using more standard methods like devtools
- [ ] delete any unused function in utils/ or 0_R 
- [ ] move any useful one off code to scripts 
- [ ] look for any dead code and unnused data
- [x] only pull xlsx or xls and ignore other in the pcr folder **
- [x] have week be calculated each week from trap_date drop the provided week in wnv-s_clean so that the pipeline isn't dependent on user input. ** (week + year now re-derived from trap_date via force_recompute on raw input; pools path Year filter also derives from Trap Date)

## pipeline
- [x] making functions
    - [x] make generate_report.R a function
    - [x] have generated report also make the graph sheet. see @
    - [x] make tables.R a function
    - [x] put bird chunk into functions
    - [x] make bird plot output
- [ ] have culex_sheet apply same expand grid logic as in ../wnv-ss_trap_hx_combiner
- [x] set email as an argument for the googleAuth
- [x] in wnvSurv make trap_status plot that is in in ../wnv-s_trap_hx_combiner
- [x] Change the historical vs current plot to be broken up by culex and pipiens as stacked geom_area using pal_mozzy from config for colors
- [ ] save pools plot to googledrive plots


## future directions
- [ ] develop plan for using duckdb to manage dataflow
- [ ] host this repo on a cloude (shiny or rstudio cloud?) so users do not have to download and install re