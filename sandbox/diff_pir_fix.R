# diff_pir_fix.R ---------------------------------------------------------------
# End-to-end impact test for the proposed calc_pir() fix.
#
# Runs the FULL pir block of calc_all() twice on the real database -- once with
# the current utils/fun_calc_pir.R, once with a patched copy -- and diffs every
# zone/year/week/spp cell so the blast radius is visible BEFORE any code changes.
#
# The patch under test (two changes, nothing else):
#   1. filter(total > 1)  ->  filter(!is.na(test_code))
#      Drops only the imputed rows (total == 0, test_code == NA), keeping real
#      single-mosquito pools -- including positives like CSU23480.
#   2. Delete the lines 73-93 "add back single pools" fallback. With size-1
#      pools kept, those groups now come out of pIR() directly, so the rbind
#      would duplicate them.
#
# NOTHING IN utils/ IS MODIFIED. The patched function is defined here only.
#
# Run: Rscript sandbox/diff_pir_fix.R

source("config/load_packages.R")
purrr::walk(list.files("utils", pattern = "*.R", full.names = TRUE), source)
read_latest("config/config_weekly_settings")   # supplies zone_lvls, grp_vars

# calc_pir_fixed(): patched copy of utils/fun_calc_pir.R -----------------------
# Deliberately a near-verbatim copy so the diff isolates the two changes above.
calc_pir_fixed <- function(df,
                           grp_var = c("zone", "year", "week", "spp"),
                           zone_complete = c("NW", "NE", "SE", "SW", "LV", "BE", "BC"),
                           rm_zone = NULL) {

  grp_var_sym <- syms(grp_var)
  zone_complete <- setdiff(zone_complete, rm_zone)

  df_pir <- df %>%
    dplyr::filter(!zone %in% rm_zone) %>%
    # CHANGE 1: was filter(total > 1). Drop imputed rows only -- a tested pool
    # of one mosquito is real data and pIR() handles it (see
    # sandbox/test_pIR_single_pool.R cases 3, 4, 7).
    dplyr::filter(!is.na(test_code)) %>%
    tidyr::unite(col = "grp", all_of(grp_var), sep = "_", remove = FALSE)

  mle <- PooledInfRate::pIR(test_code ~ total | grp, data = df_pir, pt.method = "firth")

  df_pir <- as.data.frame(mle) %>%
    separate(grp, into = {{ grp_var }}, sep = "_") %>%
    mutate(year = as.numeric(year), week = as.numeric(week)) %>%
    mutate(pir = round(P, 4), pir_lci = round(Lower, 4), pir_uci = round(Upper, 4)) %>%
    select(-P, -Upper, -Lower)

  # CHANGE 2: the lines 73-93 single-pool fallback is gone. Groups whose only
  # pool is one mosquito are now estimated by pIR() itself.

  df_pir <- df_pir %>% complete(!!!grp_var_sym)
  df_pir[is.na(df_pir)] <- 0

  if ("zone2" %in% names(df_pir)) df_pir <- df_pir %>% rename(zone = zone2)
  if (!"spp" %in% names(df_pir)) df_pir <- df_pir %>% mutate(spp = "All")

  df_pir
}

# run_pir_block(): the pir half of calc_all(), parameterised by which function --
# to use. Pure -- takes data + a function, returns a data frame.
run_pir_block <- function(pir_input, pir_fun) {
  bind_rows(
    pir_fun(pir_input, grp_var = c("zone", "year", "week", "spp")),
    pir_fun(pir_input, grp_var = c("zone", "year", "week")),
    pir_fun(pir_input, grp_var = c("zone2", "year", "spp", "week")),
    pir_fun(pir_input, grp_var = c("zone2", "year", "week"))
  ) %>%
    distinct_all() %>%
    wnv_s_clean(all_cols = names(.), zone_raw_lvls = zone_lvls, silence = TRUE) %>%
    arrange(year, week, zone, spp)
}

# INPUT ------------------------------------------------------------------------
fn <- "2_mid/2026/w29/wnv-s_database_update.csv"
pir_input <- read.csv(fn) %>% wnv_s_clean(silence = TRUE)
cat("input:", fn, "|", nrow(pir_input), "pool rows\n\n")

old <- run_pir_block(pir_input, calc_pir)
new <- run_pir_block(pir_input, calc_pir_fixed)

# DIFF -------------------------------------------------------------------------
key <- c("zone", "year", "week", "spp")

cat("rows: old =", nrow(old), "| new =", nrow(new), "\n")
cat("duplicate keys: old =", sum(duplicated(old[key])),
    "| new =", sum(duplicated(new[key])), "\n\n")

cmp <- full_join(
  old %>% select(all_of(key), pir, pir_lci, pir_uci),
  new %>% select(all_of(key), pir, pir_lci, pir_uci),
  by = key, suffix = c("_old", "_new")
)

changed <- cmp %>%
  filter(is.na(pir_old) | is.na(pir_new) | abs(pir_old - pir_new) > 1e-9) %>%
  arrange(desc(abs(coalesce(pir_new, 0) - coalesce(pir_old, 0))))

cat("cells compared:", nrow(cmp), "| pir changed:", nrow(changed), "\n")
cat("  0 -> >0 (positive recovered):", sum(changed$pir_old == 0 & changed$pir_new > 0, na.rm = TRUE), "\n")
cat("  >0 -> 0                     :", sum(changed$pir_old > 0 & changed$pir_new == 0, na.rm = TRUE), "\n")
cat("  only in old / only in new   :", sum(is.na(changed$pir_new)), "/", sum(is.na(changed$pir_old)), "\n\n")

cat("--- all changed cells ---\n")
print(as.data.frame(changed), row.names = FALSE)

write.csv(changed, "sandbox/pir_fix_diff.csv", row.names = FALSE)
cat("\nwrote sandbox/pir_fix_diff.csv\n")

# VI IMPACT --------------------------------------------------------------------
# vi = abund x pir, so every pir shift propagates. Re-run the full calc_all()
# both ways to see the change in the number epidemiologists actually report.
abund_input <- read.csv("2_mid/2026/w29/culex_database_update.csv") %>% wnv_s_clean(silence = TRUE)

calc_all_with <- function(pir_fun) {
  abund <- bind_rows(
    calc_abund(abund_input, grp_var = c("zone", "year", "week", "spp")),
    calc_abund(abund_input, grp_var = c("zone", "year", "week")),
    calc_abund(abund_input, grp_var = c("zone2", "year", "week", "spp")),
    calc_abund(abund_input, grp_var = c("zone2", "year", "week"))
  ) %>%
    distinct_all() %>%
    wnv_s_clean(all_cols = names(.), zone_raw_lvls = zone_lvls, silence = TRUE) %>%
    arrange(year, week, zone, spp)

  calc_vi(abund, run_pir_block(pir_input, pir_fun), by = grp_vars) %>%
    wnv_s_clean(rm_col = c("trap_status"), zone_raw_lvls = zone_lvls, silence = TRUE)
}

vi_old <- calc_all_with(calc_pir)
vi_new <- calc_all_with(calc_pir_fixed)

vi_cmp <- full_join(
  vi_old %>% select(all_of(key), vi, abund),
  vi_new %>% select(all_of(key), vi),
  by = key, suffix = c("_old", "_new")
) %>%
  # 2,610 cells are NA on BOTH sides (abund rows with no matching pir group --
  # pre-existing, unrelated to this fix). Exclude them or they masquerade as diffs.
  filter(!(is.na(vi_old) & is.na(vi_new))) %>%
  filter(is.na(vi_old) | is.na(vi_new) | abs(vi_old - vi_new) > 1e-9)

cat("\n\n=== VI IMPACT ===\n")
cat("rows: old =", nrow(vi_old), "| new =", nrow(vi_new), "| vi changed:", nrow(vi_cmp), "\n")
cat("  0 -> >0:", sum(vi_cmp$vi_old == 0 & vi_cmp$vi_new > 0, na.rm = TRUE),
    "| >0 -> 0:", sum(vi_cmp$vi_old > 0 & vi_cmp$vi_new == 0, na.rm = TRUE), "\n\n")
print(as.data.frame(vi_cmp %>% arrange(desc(abs(vi_new - vi_old))) %>% head(25)), row.names = FALSE)

write.csv(vi_cmp, "sandbox/vi_fix_diff.csv", row.names = FALSE)
cat("\nwrote sandbox/vi_fix_diff.csv\n")
