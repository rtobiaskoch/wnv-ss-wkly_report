# Why does trap_id get dropped by key_rename(drop_extra=T) for the datasheet?
# Compare the ACTUAL datasheet column header against the `old` value in the key,
# byte-for-byte (spaces, case, non-breaking spaces, trailing whitespace).
suppressMessages({library(dplyr)})

ds  <- readRDS("2_mid/2025/w37/weekly_data_input_format_mid.RData")  # raw datasheet_input (pre-rename)
key <- read.csv("1_input/2025/w37/key_rename.csv", stringsAsFactors = FALSE)

cat("=== datasheet_input column names (quoted) ===\n")
print(names(ds))

cat("\n=== key columns ===\n"); print(names(key))

cat("\n=== key rows whose `new` == 'trap_id' (or `old` mentions Trap ID / Collection) ===\n")
hit <- key %>% filter(new == "trap_id" |
                        grepl("Trap ID|Collection", old, ignore.case = TRUE))
print(hit)

# Find the actual datasheet column that looks like the trap id
ds_trap_cols <- names(ds)[grepl("Trap ID|Collection", names(ds), ignore.case = TRUE)]
cat("\n=== datasheet column(s) matching 'Trap ID|Collection' ===\n")
print(ds_trap_cols)

cat("\n=== EXACT match test (old %in% names(df)) for the trap_id mapping ===\n")
for (o in hit$old) {
  cat(sprintf("  key old = [%s]  -> in datasheet names? %s\n",
              o, o %in% names(ds)))
}

# Byte-level dump to expose hidden whitespace / unicode differences
dump_bytes <- function(s) paste(sprintf("%02x", as.integer(charToRaw(s))), collapse=" ")
cat("\n=== byte dump: datasheet trap col(s) ===\n")
for (c in ds_trap_cols) cat(sprintf("  [%s]\n    %s\n", c, dump_bytes(c)))
cat("\n=== byte dump: key old value(s) ===\n")
for (o in hit$old) cat(sprintf("  [%s]\n    %s\n", o, dump_bytes(o)))
