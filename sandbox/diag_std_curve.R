## Can we rebuild the standard curve from platemap labels + PCR Cq alone,
## without QuantStudio's Quantity column?
suppressMessages({
  library(dplyr); library(stringr); library(readxl); library(purrr)
  library(janitor); library(tidyr)
})
# source the report's util functions
invisible(lapply(list.files("utils", pattern = "\\.R$", full.names = TRUE),
                 function(f) try(source(f), silent = TRUE)))
copy_threshold <- 500  # global used inside clean_pcr

# ---- PCR (Cq only; Quantity is all NA in this broken export) ----
fn_pcr <- list.files("1_input/2026/w23/pcr", full.names = TRUE)
pcr_clean <- clean_pcr(read_pcr(fn_pcr, sheet = "Results"))

# ---- Platemap (where the std labels live) ----
fn_pm <- list.files("1_input/2026/w23/platemap", full.names = TRUE)
pm_input <- read_platemap(fn_pm, sheet = "pcr", range = "A1:M9",
                          col_pattern = "(?<=x)\\d+", val_to = "csu_id")
pm_clean <- clean_platemap(pm_input)

cat("=== platemap sample_type table ===\n")
print(table(pm_clean$sample_type, useNA = "always"))
cat("\n=== platemap std csu_id values ===\n")
print(pm_clean %>% filter(str_detect(str_to_lower(csu_id), "1e|std")) %>%
        distinct(csu_id, sample_type))

cq <- merge_pcr_platemap(pcr_clean, pm_clean)
cat("\n=== standard wells after merge ===\n")
print(cq %>% filter(str_detect(sample_type, "std")) %>%
        select(target_name, well_position, sample_type, cq, copies) %>%
        arrange(target_name, sample_type) %>% as.data.frame())
