suppressMessages({library(readxl); library(dplyr); library(stringr)})

inspect <- function(f) {
  raw <- read_excel(f, col_names = TRUE, sheet = "Results")
  fc <- names(raw)[1]
  filt <- raw %>% filter(.data[[fc]] %in% c("Well", as.character(1:96)))
  colnames(filt) <- as.character(unlist(filt[1, ]))
  filt <- filt[-1, ]
  sub <- filt %>%
    select(`Sample Name`, Task, Quantity, `Y-Intercept`, Slope) %>%
    as.data.frame()
  cat("\n=====", f, "=====\n")
  cat("Quantity non-NA:", sum(is.na(sub$Quantity) == FALSE), "/", nrow(sub), "\n")
  cat("Slope unique:", paste(head(unique(sub$Slope), 3), collapse = ", "), "\n")
  cat("Task table: "); print(table(sub$Task, useNA = "always"))
  cat("Sample Name (first 6 unique):", paste(head(unique(sub$`Sample Name`), 6), collapse = " | "), "\n")
}

inspect("1_input/2026/w23/pcr/ss_y2026_w23_p1_pcr.xls")
inspect("1_input/2025/w35/pcr/SS-y2025-w35-p1-pcr.xls")
