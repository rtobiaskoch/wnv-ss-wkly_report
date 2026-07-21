# audit_wnv_s_clean.R ----------------------------------------------------------
# Behavioural diff: utils/fun_clean_wnv_s.R vs wnvSurv::wnv_s_clean on real data.
# Only the 4 params the package accepts are used, so the comparison is apples-to-
# apples on the shared surface.
suppressMessages({library(dplyr); library(tidyr); library(stringr); library(purrr)
                  library(lubridate); library(janitor); library(rlang); library(cli)
                  library(wnvSurv)})

pkg_clean <- wnvSurv::wnv_s_clean
e <- new.env(); suppressMessages(sys.source("utils/fun_clean_wnv_s.R", envir = e))
loc_clean <- e$wnv_s_clean

cmp <- function(path, label) {
  raw <- read.csv(path)
  a <- suppressWarnings(suppressMessages(capture.output(
        A <- loc_clean(raw, silence = TRUE))))
  b <- suppressWarnings(suppressMessages(capture.output(
        B <- try(pkg_clean(raw), silent = TRUE))))
  cat("\n===", label, "| input rows:", nrow(raw), "===\n")
  if (inherits(B, "try-error")) {
    cat("  wnvSurv ERRORED:", conditionMessage(attr(B, "condition")), "\n"); return(invisible())
  }
  cat("  rows   utils:", nrow(A), " wnvSurv:", nrow(B), "\n")
  cat("  cols   utils:", ncol(A), " wnvSurv:", ncol(B), "\n")
  onlyA <- setdiff(names(A), names(B)); onlyB <- setdiff(names(B), names(A))
  if (length(onlyA)) cat("  cols only in utils  :", paste(onlyA, collapse=", "), "\n")
  if (length(onlyB)) cat("  cols only in wnvSurv:", paste(onlyB, collapse=", "), "\n")

  shared <- intersect(names(A), names(B))
  if (nrow(A) == nrow(B)) {
    for (cl in shared) {
      x <- as.character(A[[cl]]); y <- as.character(B[[cl]])
      d <- sum(x != y | xor(is.na(x), is.na(y)), na.rm = TRUE)
      if (d > 0) cat(sprintf("  DIFF %-12s %d/%d values differ\n", cl, d, nrow(A)))
    }
    # factor levels matter: CLAUDE.md flags zone level drift as a silent bug source
    for (cl in intersect(c("zone","zone2","spp"), shared)) {
      la <- levels(A[[cl]]); lb <- levels(B[[cl]])
      if (!identical(la, lb))
        cat(sprintf("  LEVELS %-6s utils=[%s] wnvSurv=[%s]\n", cl,
            paste(la, collapse=","), paste(lb, collapse=",")))
    }
  } else cat("  (row counts differ - per-column diff skipped)\n")
}

cmp("2_mid/2026/w29/wnv-s_database_update.csv",   "pool database")
cmp("2_mid/2026/w29/culex_database_update.csv",   "culex trap database")
