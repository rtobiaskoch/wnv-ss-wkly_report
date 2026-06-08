# Local verification runner: shims the not-yet-installed wnvSurv std_curve
# functions into the global env, then runs the report fixture test.
suppressMessages({
  library(testthat); library(here); library(tibble)
  library(dplyr); library(stringr); library(readxl); library(writexl)
})
# shim: source new wnvSurv functions until the user reinstalls the package
source("/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_functions/R/std_curve.R")
testthat::test_file(here::here("tests/testthat/test-patch-pcr.R"), reporter = "summary")
