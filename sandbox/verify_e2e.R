# End-to-end verification on the REAL w23 data, in a temp copy (1_input untouched).
suppressMessages({
  library(dplyr); library(stringr); library(readxl); library(writexl)
  library(purrr); library(janitor); library(tidyr); library(tibble)
})
source("/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_functions/R/std_curve.R")  # shim
invisible(lapply(list.files("utils", pattern = "\\.R$", full.names = TRUE),
                 function(f) try(source(f), silent = TRUE)))
source("scripts/patch_pcr_quantity.R")   # functions only; main() not run
copy_threshold <- 500                    # global used by clean_pcr

# --- temp input tree mirroring 1_input/2026/w23 ---
root <- file.path(tempdir(), paste0("e2e_", Sys.getpid()))
dir_pcr <- file.path(root, "2026", "w23", "pcr")
dir_pm  <- file.path(root, "2026", "w23", "platemap")
dir.create(dir_pcr, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_pm,  recursive = TRUE, showWarnings = FALSE)
file.copy(list.files("1_input/2026/w23/pcr", full.names = TRUE,
                     pattern = "\\.xls$"), dir_pcr)
file.copy(list.files("1_input/2026/w23/platemap", full.names = TRUE), dir_pm)

# --- run the patch (the exact main() steps) ---
pm_clean <- clean_platemap(read_platemap(list.files(dir_pm, full.names = TRUE)))
dir_archive <- file.path(root, "2026", "w23", "pcr_raw_uncorrected")
raw <- list.files(dir_pcr, pattern = "\\.xls$", full.names = TRUE)
res <- patch_pcr_file(raw, pm_clean, dir_archive)

cat("\n--- dir_pcr after patch (should be ONLY the corrected .xlsx) ---\n")
print(list.files(dir_pcr))
cat("archive holds:", list.files(dir_archive), "\n")

# --- now run the pipeline's reader on the corrected file ---
fn <- list.files(dir_pcr, full.names = TRUE)
pcr_clean <- clean_pcr(read_pcr(fn, sheet = "Results"))

cat("\n--- copies summary after correction ---\n")
cat("rows:", nrow(pcr_clean),
    "| copies > 0:", sum(pcr_clean$copies > 0),
    "| max copies:", format(max(pcr_clean$copies), big.mark = ","), "\n")
cat("test_code (positives) table:\n"); print(table(pcr_clean$test_code))

# --- standards specifically: log_copies should now span, not be all 0 ---
cq <- merge_pcr_platemap(pcr_clean, pm_clean)
std <- clean_standards(cq)
cat("\n--- standards log_copies (was all 0 before) ---\n")
print(std %>% filter(str_detect(sample_type, "std")) %>%
        select(target, sample_type, csu_id, cq, log_copies) %>%
        arrange(target, sample_type) %>% as.data.frame())
