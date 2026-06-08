# Matching invariant on the real w37/2025 render artifacts:
# a pool and the trap-count it came from must share the same week.
suppressMessages({library(dplyr); library(readr)})

dn  <- readRDS("2_mid/2025/w37/y2025_w37_database_new.RData")        # pools / pcr database
asp <- readRDS("2_mid/2025/w37/y2025_w37_all_species_active_trap.RData") # culex counts

cat("=== database_new cols ===\n"); print(names(dn))
cat("\n=== all_species cols ===\n"); print(names(asp))

# normalise key columns if present in both
keys <- intersect(intersect(names(dn), names(asp)), c("trap_id","trap_date","spp","zone","year"))
cat("\nshared key cols:", paste(keys, collapse=", "), "\n")

if ("week" %in% names(dn) && "week" %in% names(asp) && length(keys) >= 2) {
  dn2  <- dn  %>% distinct(across(all_of(keys)), week_pool = week)
  asp2 <- asp %>% distinct(across(all_of(keys)), week_count = week)
  j <- inner_join(dn2, asp2, by = keys)
  n_mismatch <- sum(j$week_pool != j$week_count, na.rm = TRUE)
  cat("\njoined rows:", nrow(j), " | week mismatches:", n_mismatch, "\n")
  if (n_mismatch > 0) print(head(j %>% filter(week_pool != week_count), 10))
} else {
  cat("\n(could not auto-run join — inspect cols above)\n")
}
