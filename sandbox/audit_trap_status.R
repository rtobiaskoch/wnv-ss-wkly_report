# Where exactly do the two trap_status implementations disagree?
suppressMessages({library(dplyr); library(tidyr); library(stringr); library(purrr)
                  library(lubridate); library(janitor); library(rlang); library(cli); library(wnvSurv)})
e <- new.env(); suppressMessages(sys.source("utils/fun_clean_wnv_s.R", envir = e))
raw <- read.csv("2_mid/2026/w29/culex_database_update.csv")
invisible(capture.output(A <- suppressWarnings(e$wnv_s_clean(raw, silence = TRUE))))
invisible(capture.output(B <- suppressWarnings(wnvSurv::wnv_s_clean(raw, verbose = FALSE))))
cat("\ncross-tab: utils (rows) vs wnvSurv (cols)\n")
print(table(utils = A$trap_status, wnvSurv = B$trap_status, useNA = "ifany"))
