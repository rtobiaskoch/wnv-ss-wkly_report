# Prove the rewritten key_rename recovers trap_id from the real 2026 datasheet,
# where the header has 7 spaces and the lookup has 1.
suppressMessages({library(dplyr); library(readxl)})

# source the NEW wnvSurv implementation directly (not the installed one)
source("/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_functions/R/key_rename.R")

ds  <- readxl::read_excel("1_input/2026/w23/datasheet/Week 22 WNV 2026 CSU Datasheet .xlsx")
key <- read.csv("1_input/2026/w23/key_rename.csv", stringsAsFactors = FALSE)

out <- key_rename(ds, rename_df = key, drop_extra = TRUE)

cat("trap_id present after fix?  ", "trap_id" %in% names(out), "\n")
cat("output columns:\n"); print(names(out))
cat("\nfirst few trap_id values:\n"); print(head(out$trap_id))
