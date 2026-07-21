#!/usr/bin/env Rscript
# relabel_trap_status.R --------------------------------------------------------
#
# One-time migration of the legacy trap_status vocabulary in the stored culex
# sheet database:
#
#   "no culex"  -> "other spp"   same meaning (trap ran, caught mosquitoes, none
#                                of them Culex); "other spp" is the more explicit
#                                label and is what wnv_s_clean() now emits
#   "No Traps"  -> "no trap"     legacy capitalisation/plural; the palette and
#                                the data have always disagreed on this one
#
# This is a pure RELABEL, not a recompute -- no row is reclassified, only
# renamed. It is therefore lossless and reversible.
#
# WRITES LOCALLY ONLY. Nothing is pushed to the master Google Sheet. Review the
# output and the diff report, then decide separately whether to upload.
#
# Usage:
#   Rscript scripts/relabel_trap_status.R                 # dry run, report only
#   Rscript scripts/relabel_trap_status.R --write         # write _relabelled.csv

suppressMessages({
  library(dplyr)
})

args  <- commandArgs(trailingOnly = TRUE)
write <- "--write" %in% args

# Files carrying stored trap_status. The 1_input copy is the gdrive-synced
# mirror of the master sheet (key_database_culex_sheet in config_weekly.R:224);
# the 2_mid copy is this week's working update.
targets <- c(
  "1_input/2026/w29/culex_sheet_database.csv",
  "2_mid/2026/w29/culex_database_update.csv"
)

# relabel_status(): pure -- takes a character vector, returns a new one.
relabel_status <- function(x) {
  dplyr::case_when(
    grepl("^\\s*no\\s+traps?\\s*$", x, ignore.case = TRUE) ~ "no trap",
    grepl("^\\s*no\\s+culex\\s*$",  x, ignore.case = TRUE) ~ "other spp",
    TRUE                                                   ~ x
  )
}

for (f in targets) {
  if (!file.exists(f)) {
    cat("SKIP (not found):", f, "\n")
    next
  }

  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"trap_status" %in% names(d)) {
    cat("SKIP (no trap_status column):", f, "\n")
    next
  }

  old <- d$trap_status
  new <- relabel_status(old)
  changed <- which(old != new)

  cat("\n===", f, "|", nrow(d), "rows ===\n")
  if (length(changed) == 0) {
    cat("  nothing to relabel\n")
    next
  }
  print(as.data.frame(table(from = old[changed], to = new[changed])) |>
          dplyr::filter(Freq > 0), row.names = FALSE)

  # Invariant: a relabel must never change the number of rows in each status
  # bucket beyond the renames themselves.
  stopifnot(length(old) == length(new),
            sum(old != new) == length(changed))

  if (write) {
    d$trap_status <- new
    out <- sub("\\.csv$", "_relabelled.csv", f)
    write.csv(d, out, row.names = FALSE)
    cat("  wrote:", out, "\n")
  } else {
    cat("  (dry run -- pass --write to produce a _relabelled.csv)\n")
  }
}

cat("\nNothing was uploaded. Review the _relabelled.csv files, then decide\n",
    "whether to push to the master sheet.\n", sep = "")
