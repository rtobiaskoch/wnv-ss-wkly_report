# Byte-compare the 2026 w23 datasheet trap_id header vs the 2026 key `old` value.
suppressMessages({library(dplyr); library(readxl)})

xlsx <- "1_input/2026/w23/datasheet/Week 22 WNV 2026 CSU Datasheet .xlsx"
key  <- read.csv("1_input/2026/w23/key_rename.csv", stringsAsFactors = FALSE)

ds <- readxl::read_excel(xlsx)
cat("=== 2026 datasheet column names ===\n"); print(names(ds))

ds_trap <- names(ds)[grepl("Trap ID|Collection", names(ds), ignore.case = TRUE)]
key_trap <- key$old[key$new == "trap_id"]

cat("\n=== datasheet trap col(s) ===\n"); print(ds_trap)
cat("=== key old value(s) for trap_id ===\n"); print(key_trap)

cat("\n=== EXACT match? old %in% names(ds) ===\n")
for (o in key_trap) cat(sprintf("  [%s] in datasheet? %s\n", o, o %in% names(ds)))

dump <- function(s) paste(sprintf("%02x", as.integer(charToRaw(s))), collapse=" ")
cat("\n=== byte dump: datasheet trap col(s) ===\n")
for (c in ds_trap) cat(sprintf("  [%s]\n    %s\n", c, dump(c)))
cat("\n=== byte dump: key old value(s) for trap_id ===\n")
for (o in key_trap) cat(sprintf("  [%s]\n    %s\n", o, dump(o)))

# Whitespace-collapsed comparison (what a robust matcher would do)
squish <- function(s) gsub("\\s+", " ", trimws(s))
cat("\n=== after squish (collapse whitespace) do they match? ===\n")
for (c in ds_trap) for (o in key_trap)
  cat(sprintf("  ds[%s] vs key[%s] -> %s\n", squish(c), squish(o), squish(c) == squish(o)))
