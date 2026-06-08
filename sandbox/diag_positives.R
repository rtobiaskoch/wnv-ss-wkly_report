suppressMessages({library(dplyr); library(stringr)})
pcr <- readRDS("2_mid/2026/w23/y2026_w23_pcr_clean.RData")
cat("=== positives by target ===\n")
print(pcr %>% filter(test_code == 1) %>% count(target_name))
cat("\n=== positive wells detail ===\n")
print(as.data.frame(pcr %>% filter(test_code == 1) %>%
  select(well_position, target_name, cq, copies) %>% arrange(target_name, desc(copies))))

cat("\n=== real input pcr dir state ===\n")
cat("files in 1_input/2026/w23/pcr:\n")
print(list.files("1_input/2026/w23/pcr"))
cat("uncorrected_raw sibling exists:",
    dir.exists("1_input/2026/w23/pcr_raw_uncorrected"),
    "->", paste(list.files("1_input/2026/w23/pcr_raw_uncorrected"), collapse=", "), "\n")
