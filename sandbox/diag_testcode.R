suppressMessages({library(dplyr); library(stringr); library(readxl); library(readr)})

# 1) Does the BC datasheet already carry Test Code / Test Result for BOU pools?
bc <- read_excel("1_input/2026/w23/datasheet/02JuneWNV 2026 BC Data Sheet.xlsx")
cat("=== BC datasheet: pool, Test Code, Test Result ===\n")
print(as.data.frame(bc[, c("CSU Pool Number (CMC Enters)",
                           "Test Code (CSU Enters)", "Test Result (CSU Enters)")]))

# 2) Are BOU pools present in cq_data (the PCR side that supplies test_code)?
cq <- readRDS("2_mid/2026/w23/y2026_w23_cq_data.RData")
cat("\n=== cq_data: any BOU csu_id? ===\n")
cat("BOU in cq_data:", sum(str_detect(str_to_upper(cq$csu_id), "BOU")), "\n")
cat("sample csu_ids in cq_data (first 10):",
    paste(head(unique(cq$csu_id), 10), collapse = ", "), "\n")

# 3) What is BOU's test_code in database_new (the merge output)?
dbn <- readRDS("2_mid/2026/w23/y2026_w23_database_new.RData")
bou <- dbn[str_detect(str_to_upper(dbn$csu_id), "BOU"), ]
cat("\n=== database_new BOU rows ===\n")
print(as.data.frame(bou[, intersect(c("csu_id","trap_id","week","test_code","copies","cq"), names(bou))]))
