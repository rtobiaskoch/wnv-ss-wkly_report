suppressMessages({library(dplyr); library(stringr); library(purrr); library(readxl)
  library(lubridate); library(tidyr); library(janitor)})
# week function: prefer installed wnvSurv, else source from package
csw <- tryCatch(wnvSurv::calc_season_week, error = function(e) NULL)
if (is.null(csw)) {
  source("/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_functions/R/calc_week.R")
  csw <- calc_season_week
}
source("utils/fun_read_list.R")

week_filter <- 23; year_filter <- 2026
tid <- "Collection Site       (Trap ID)"
ds <- read_list("1_input/2026/w23/datasheet", "*")
ds <- ds[!is.na(ds[[tid]]), ]

cat("total datasheet rows:", nrow(ds), "\n")
cat("columns:\n"); print(names(ds))

pool <- ds[["CSU Pool Number (CMC Enters)"]]
src  <- if_else(str_detect(str_to_upper(pool), "^BOU"), "BOU", "CSU")
wk_derived <- tryCatch(csw(ds[["Trap Date"]]),
                       error = function(e) paste("ERR:", conditionMessage(e)))

cat("\n--- source counts ---\n"); print(table(src, useNA = "always"))
cat("Trap Date class:", class(ds[["Trap Date"]]), " | Year class:", class(ds[["Year"]]), "\n")

info <- tibble(pool = pool, src = src,
               trap_date = as.character(ds[["Trap Date"]]),
               year = ds[["Year"]], week_typed = ds[["Week"]],
               week_derived = wk_derived)
cat("\n--- BOU rows ---\n")
print(as.data.frame(info[info$src == "BOU", ]))
cat("\n--- pass filter (week_derived == 23 & year == 2026)? ---\n")
passes <- (suppressWarnings(csw(ds[["Trap Date"]])) == week_filter) & (ds[["Year"]] == year_filter)
print(table(src, passes, useNA = "always"))
