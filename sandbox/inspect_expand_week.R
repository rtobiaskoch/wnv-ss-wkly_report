# Inspect how `week` is keyed in the rebuilt combiner output.
# Run from repo root: Rscript sandbox/inspect_expand_week.R
suppressMessages({library(dplyr); library(lubridate); library(readr)})

df <- suppressMessages(read_csv("sandbox/culex_sheet_database_expand.csv", show_col_types = FALSE))
real <- dplyr::filter(df, !is.na(trap_date))

cat("=== rows with real trap_date:", nrow(real), "of", nrow(df), "===\n\n")

chk <- real %>% mutate(epi = epiweek(trap_date), iso = isoweek(trap_date))
cat("stored week == epiweek(trap_date):", round(mean(chk$week == chk$epi, na.rm = TRUE) * 100, 1), "%\n")
cat("stored week == isoweek(trap_date):", round(mean(chk$week == chk$iso, na.rm = TRUE) * 100, 1), "%\n\n")

cat("=== 2026 real-trap_date rows: date -> stored week vs epiweek vs isoweek ===\n")
chk %>%
  filter(year == 2026) %>%
  distinct(trap_date, week, epi, iso) %>%
  arrange(trap_date) %>%
  head(20) %>%
  as.data.frame() %>%
  print()

cat("\n=== mismatch rows (stored week != epiweek) by year ===\n")
chk %>%
  filter(week != epi) %>%
  count(year, name = "n_mismatch") %>%
  as.data.frame() %>%
  print()
