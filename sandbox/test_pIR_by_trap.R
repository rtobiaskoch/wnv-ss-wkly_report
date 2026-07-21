# test_pIR_by_trap.R -----------------------------------------------------------
# Your original comment says the total > 1 filter was added because size-1 pools
# "break the pIR for vi by trap". calc_vi_stats() groups by trap_id, which makes
# groups TINY -- very often a single pool of one mosquito. That is a different
# regime from calc_pir(), which groups by zone/year/week/spp. Tested on real data,
# one week at a time (whole-database trap-level pIR is too slow to be useful here).
source("config/load_packages.R")
purrr::walk(list.files("utils", pattern = "*.R", full.names = TRUE), source)
read_latest("config/config_weekly_settings")

d <- read.csv("2_mid/2026/w29/wnv-s_database_update.csv") %>% wnv_s_clean(silence = TRUE)

by_trap <- function(df, label) {
  x <- df %>% tidyr::unite("grp", c("trap_id","year","week","spp"), sep="_", remove=FALSE)
  cat("\n---", label, "---\n  rows:", nrow(x), " groups:", dplyr::n_distinct(x$grp),
      " size-1 pools:", sum(x$total == 1), "\n")
  out <- try(PooledInfRate::pIR(test_code ~ total|grp, data = x, pt.method = "firth"),
             silent = TRUE)
  if (inherits(out, "try-error")) cat("  ERROR:", conditionMessage(attr(out, "condition")), "\n")
  else cat("  OK -- estimated", nrow(as.data.frame(out)), "groups\n")
  invisible(NULL)
}

wk <- d %>% filter(year == 2025, week == 26)
by_trap(wk %>% filter(!is.na(test_code)), "2025 wk26, all tested pools (proposed)")
by_trap(wk %>% filter(total > 1),         "2025 wk26, total > 1 (original filter)")

# A trap-week-spp group whose only pool is a single mosquito -- the exact shape
# that is common at trap level and rare at zone level.
one <- d %>% filter(!is.na(test_code), total == 1) %>% slice(1)
by_trap(one, "one trap, one pool, one mosquito")
