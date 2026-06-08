

## Create Testing Environment
- [ ] create simple synthetic test dataset and pipeline that won't impact the live data (in process)
- [ ] create unit tests to verify pipeline that is isolated from live environment
- [ ] fix generate_report.R not making the xlsx file in the smoke_test.R
- [ ] have smoke_test use pipeline but have a special I/O folders to isolate from the pipeline

## architecture
- [ ] to prevent any overwriting between years paths include the year eg. 1_input/2026/w25
- [ ] save the config in the 1_input/YYYY/wXX folder
- [ ] make a matching output directory for the week 3_output/YYYY/wXX
- [ ] update functions from R wnv-ss-foco package (in developement)
- [ ] keep weekly report specific functions in R/ and call using more standard methods like devtools
- [ ] change the argument for historical to be the previous 5 year rolling window as the default not setting a specific year.

## pipeline
- [ ] preflight section to ensure googledrive connected all packages installed and all files available
- [ ] put bird chunk into functions
- [ ] consider dropping week check in check_data because I pull it from the date anyway.
- [ ] make generate_report.R a function
- [ ] rename utils to proper naming /R
- [ ] delete any unused function in utils/ or 0_R 
- [ ] move any potentially useful one of code to scripts 
- [ ] set email as an argument for the googleAuth

## output
- [ ] Change the historical vs current plot to be broken up by culex and pipiens
- [ ] fix generate report

## future directions
- [ ] develop plan for using duckdb to manage dataflow
- [ ] host this repo on a cloude (shiny or rstudio cloud?) so users do not have to download and install re