# How often do isoweek (counts path) and epiweek (pool path) disagree
# for the SAME trap_date? Run from repo root.
suppressMessages({library(dplyr); library(lubridate); library(readr)})
df   <- suppressMessages(read_csv("sandbox/culex_sheet_database_expand.csv", show_col_types = FALSE))
real <- df %>% filter(!is.na(trap_date)) %>% distinct(trap_date)
real <- real %>% mutate(epi = epiweek(trap_date), iso = isoweek(trap_date))
cat("distinct real trap_dates:", nrow(real), "\n")
cat("trap_dates where epiweek != isoweek:",
    sum(real$epi != real$iso), "(",
    round(mean(real$epi != real$iso) * 100, 1), "% )\n")
