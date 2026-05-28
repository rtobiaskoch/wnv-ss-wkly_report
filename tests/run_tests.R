# Run all unit tests for the WNV surveillance pipeline
# Usage (interactive): source("tests/run_tests.R")
# Usage (terminal):    Rscript tests/run_tests.R

library(testthat)
library(here)

testthat::test_dir(here::here("tests/testthat/"), reporter = "progress")
