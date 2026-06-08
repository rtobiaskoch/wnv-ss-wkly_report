# Reproduce the exact datasheet key_rename step and see whether trap_id survives.
suppressMessages({library(dplyr); library(rlang)})
source("utils/fun_rename_key.R")  # the local key_rename actually used by the qmd

ds  <- readRDS("2_mid/2025/w37/weekly_data_input_format_mid.RData")
key <- read.csv("1_input/2025/w37/key_rename.csv", stringsAsFactors = FALSE)

# Mirror key_rename internals
valid_map <- key %>% filter(old %in% names(ds))
cat("=== valid_map (rows whose old matches a datasheet column) ===\n")
print(valid_map)

cat("\n=== duplicate `new` values in valid_map? ===\n")
print(valid_map %>% count(new) %>% filter(n > 1))

cat("\n=== rename_pairs = setNames(old, new) ===\n")
rename_pairs <- setNames(valid_map$old, valid_map$new)
print(rename_pairs)

cat("\n=== does any `new` collide with an existing ds column that is NOT being renamed? ===\n")
collide <- intersect(valid_map$new, names(ds))
collide <- setdiff(collide, valid_map$new[valid_map$old != valid_map$new])
print(collide)

cat("\n=== run key_rename(drop_extra=TRUE) and check for trap_id ===\n")
out <- tryCatch(
  key_rename(ds, rename_df = key, drop_extra = TRUE),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(out)) {
  cat("trap_id present in output? ", "trap_id" %in% names(out), "\n")
  cat("output columns:\n"); print(names(out))
}
