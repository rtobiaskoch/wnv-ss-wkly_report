# week_audit.R — EXPLORATORY (sandbox)
#
# Purpose: Determine how the `week` column in the master culex database was
#          keyed historically, to confirm whether year-over-year comparisons
#          in the report are internally consistent.
#
# Question: Do the stored `week` values equal isoweek(trap_date) (what the
#           report cleaning path computes) or epiweek(trap_date) (what the
#           master-sheet write-back uses), or neither (manual entry)?
#
# This is a one-off diagnostic. Do not wire into the pipeline.
# Run from repo root: Rscript sandbox/week_audit.R

suppressMessages(library(lubridate))

df_path <- "2_mid/culex_database_update.csv"   # most recent merged master snapshot
d <- read.csv(df_path, stringsAsFactors = FALSE)
d$trap_date <- as.Date(d$trap_date)
d <- d[which(!is.na(d$trap_date) & !is.na(d$week)), ]

# Recompute both week conventions from the trap date for comparison
d$iso <- isoweek(d$trap_date)   # ISO 8601, Monday-start  (report path)
d$epi <- epiweek(d$trap_date)   # MMWR/CDC, Sunday-start   (master-sheet path)

cat("file:", df_path, "\n")

cat("rows with date+week:", nrow(d), "\n")
cat("year range:", paste(range(year(d$trap_date)), collapse = "-"), "\n\n")

cat("stored week == isoweek :", sum(d$week == d$iso), sprintf("(%.2f%%)\n", 100 * mean(d$week == d$iso)))
cat("stored week == epiweek :", sum(d$week == d$epi), sprintf("(%.2f%%)\n", 100 * mean(d$week == d$epi)))
cat("matches NEITHER        :", sum(d$week != d$iso & d$week != d$epi), "\n\n")

mm <- d[which(d$week != d$iso), ]
cat("=== rows where stored week != isoweek, by year ===\n")
print(table(year(mm$trap_date)))

cat("\n=== sample mismatches (earliest dates) ===\n")
print(head(mm[order(mm$trap_date), c("trap_date", "year", "week", "iso", "epi")], 12))

# Focus: how is the season start (early June) labeled historically?
cat("\n=== distinct (trap_date, stored week) for June 1-10 each year ===\n")
jun <- d[which(month(d$trap_date) == 6 & day(d$trap_date) <= 10), ]
ju <- unique(jun[order(jun$trap_date), c("trap_date", "week", "iso", "epi")])
print(ju, row.names = FALSE)
