# audit_calc_vi.R --------------------------------------------------------------
# Behavioural diff: utils/fun_calc_vi.R vs wnvSurv::calc_vi on real data.
source("config/load_packages.R")
purrr::walk(list.files("utils", pattern = "*.R", full.names = TRUE), source)
read_latest("config/config_weekly_settings")
local_calc_vi <- calc_vi   # utils version (masks the package one)

pir_input   <- read.csv("2_mid/2026/w29/wnv-s_database_update.csv") %>% wnv_s_clean(silence = TRUE)
abund_input <- read.csv("2_mid/2026/w29/culex_database_update.csv") %>% wnv_s_clean(silence = TRUE)

mk <- function(f, ...) bind_rows(
  f(..., grp_var = c("zone","year","week","spp")), f(..., grp_var = c("zone","year","week")),
  f(..., grp_var = c("zone2","year","week","spp")), f(..., grp_var = c("zone2","year","week"))
) %>% distinct_all() %>%
  wnv_s_clean(all_cols = names(.), zone_raw_lvls = zone_lvls, silence = TRUE) %>%
  arrange(year, week, zone, spp)

abund <- mk(calc_abund, abund_input)
pir   <- mk(calc_pir,   pir_input)

a <- local_calc_vi(abund, pir, by = grp_vars)                       # what runs today
b <- wnvSurv::calc_vi(abund, pir, by = grp_vars)                    # package DEFAULTS
c <- wnvSurv::calc_vi(abund, pir, by = grp_vars,                    # args matched to utils
                      complete = TRUE, zone_complete = grp_zones)

cat("\n--- row counts ---\n")
cat("utils (complete=TRUE, grp_zones):      ", nrow(a), "\n")
cat("wnvSurv DEFAULTS (complete=FALSE):     ", nrow(b), "\n")
cat("wnvSurv complete=TRUE + grp_zones:     ", nrow(c), "\n")

key <- c("zone","year","week","spp")
same <- function(x, y, lab) {
  m <- full_join(x %>% select(all_of(key), vi), y %>% select(all_of(key), vi),
                 by = key, suffix = c("_1","_2"))
  d <- m %>% filter(!(is.na(vi_1) & is.na(vi_2))) %>%
             filter(is.na(vi_1) | is.na(vi_2) | abs(vi_1 - vi_2) > 1e-9)
  cat(sprintf("%-38s differing cells: %d  (only-in-1: %d, only-in-2: %d)\n",
      lab, nrow(d), sum(is.na(d$vi_2)), sum(is.na(d$vi_1))))
}
same(a, b, "utils vs wnvSurv defaults")
same(a, c, "utils vs wnvSurv matched args")

cat("\n--- zones present ---\n")
cat("utils :", paste(sort(unique(as.character(a$zone))), collapse=", "), "\n")
cat("wnvSurv default:", paste(sort(unique(as.character(b$zone))), collapse=", "), "\n")
cat("\ngrp_zones (config) :", paste(grp_zones, collapse=", "), "\n")
cat("duplicated in grp_zones:", paste(grp_zones[duplicated(grp_zones)], collapse=", "), "\n")
