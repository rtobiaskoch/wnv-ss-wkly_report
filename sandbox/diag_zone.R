suppressMessages({library(readxl); library(dplyr)})
ds_dir <- "1_input/2026/w23/datasheet"
fs <- list.files(ds_dir, pattern = "xlsx$", full.names = TRUE)
for (f in fs) {
  cat("\n=====", basename(f), "=====\n")
  hdr <- names(read_excel(f, n_max = 0))
  cat(paste(hdr, collapse = " | "), "\n")
  cat("has zone-like col:",
      paste(grep("zone", hdr, ignore.case = TRUE, value = TRUE), collapse = ", "), "\n")
}
