suppressMessages({library(dplyr); library(stringr); library(readr); library(readxl)})

cat("===== weekly_data_input_format.csv: Boulder pools =====\n")
wk <- read_csv("3_output/2026/w23/weekly_data_input_format.csv", show_col_types = FALSE)
bou <- wk[str_detect(str_to_upper(wk[["CSU Pool Number (CMC Enters)"]]), "BOU"), ]
print(as.data.frame(bou[, intersect(c("CSU Pool Number (CMC Enters)","Collection Site       (Trap ID)",
  "Zone","Test Code (CSU Enters)","Test Result (CSU Enters)","Week"), names(bou))]))
cat("n BOU rows in weekly output:", nrow(bou),
    "| NA test codes:", sum(is.na(bou[["Test Code (CSU Enters)"]])), "\n")

cat("\n===== zone_stats.csv: zones present =====\n")
zs <- read_csv("3_output/2026/w23/zone_stats.csv", show_col_types = FALSE)
print(zs %>% count(zone))

cat("\n===== PCR copies (did the quantification issue get fixed?) =====\n")
pcr <- readRDS("2_mid/2026/w23/y2026_w23_pcr_clean.RData")
cat("rows:", nrow(pcr), "| copies > 0:", sum(pcr$copies > 0),
    "| max copies:", format(max(pcr$copies), big.mark=","),
    "| positives (test_code==1):", sum(pcr$test_code == 1, na.rm=TRUE), "\n")
