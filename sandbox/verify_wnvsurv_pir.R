# verify_wnvsurv_pir.R ---------------------------------------------------------
# Confirms the fix as implemented in wnvSurv reproduces the sandbox prototype
# exactly (12 recoveries, 339 changed cells, no dupes) on the real database.
source("config/load_packages.R")
purrr::walk(list.files("utils", pattern = "*.R", full.names = TRUE), source)
read_latest("config/config_weekly_settings")
local_calc_pir <- calc_pir                      # utils/ version, captured before load_all
devtools::load_all("../wnv-ss_functions", quiet = TRUE)

run_pir_block <- function(d, f) {
  bind_rows(
    f(d, grp_var = c("zone", "year", "week", "spp")),
    f(d, grp_var = c("zone", "year", "week")),
    f(d, grp_var = c("zone2", "year", "spp", "week")),
    f(d, grp_var = c("zone2", "year", "week"))
  ) %>% distinct_all() %>%
    wnv_s_clean(all_cols = names(.), zone_raw_lvls = zone_lvls, silence = TRUE) %>%
    arrange(year, week, zone, spp)
}

pir_input <- read.csv("2_mid/2026/w29/wnv-s_database_update.csv") %>% wnv_s_clean(silence = TRUE)
key <- c("zone", "year", "week", "spp")

old <- run_pir_block(pir_input, local_calc_pir)
new <- run_pir_block(pir_input, wnvSurv::calc_pir)

cat("rows old/new:", nrow(old), "/", nrow(new),
    "| dup keys new:", sum(duplicated(new[key])), "\n")
d <- full_join(old %>% select(all_of(key), pir), new %>% select(all_of(key), pir),
               by = key, suffix = c("_old", "_new")) %>%
  filter(abs(pir_old - pir_new) > 1e-9)
cat("changed:", nrow(d),
    "| 0 -> >0:", sum(d$pir_old == 0 & d$pir_new > 0),
    "| >0 -> 0:", sum(d$pir_old > 0 & d$pir_new == 0), "\n")
