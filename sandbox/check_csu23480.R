# Confirms the original audit finding is resolved with the shipped utils/ code.
source("config/load_packages.R")
purrr::walk(list.files("utils", pattern = "*.R", full.names = TRUE), source)
read_latest("config/config_weekly_settings")

pir_input   <- read.csv("2_mid/2026/w29/wnv-s_database_update.csv") %>% wnv_s_clean(silence = TRUE)
abund_input <- read.csv("2_mid/2026/w29/culex_database_update.csv") %>% wnv_s_clean(silence = TRUE)

out <- calc_all(abund_input, pir_input) %>%
  filter(zone == "NW", year == 2025, week == 26) %>%
  select(zone, year, week, spp, abund, pir, pir_lci, pir_uci, vi)
print(as.data.frame(out), row.names = FALSE)
