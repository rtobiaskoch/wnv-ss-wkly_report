suppressMessages({library(readxl); library(dplyr)})
f <- "1_input/2026/w23/pcr/ss_y2026_w23_p1_pcr.xls"
g <- read_excel(f, sheet = "Results", col_names = FALSE,
                .name_repair = "minimal")
cat("dim:", dim(g)[1], "x", dim(g)[2], "\n")
col1 <- g[[1]]
hdr_row <- which(col1 == "Well")
cat("header row index:", hdr_row, "\n")
cat("preamble rows (before header):", hdr_row - 1, "\n")
hdr <- as.character(unlist(g[hdr_row, ]))
qcol <- which(hdr == "Quantity")
cat("Quantity col index:", qcol, "\n")
cat("Target Name col:", which(hdr == "Target Name"),
    " | Well Position col:", which(hdr == "Well Position"),
    " | CT col:", which(hdr == "CT"), "\n")
# data rows: col1 in 1..96 after header
data_rows <- which(col1 %in% as.character(1:96))
data_rows <- data_rows[data_rows > hdr_row]
cat("n data rows:", length(data_rows),
    " span:", min(data_rows), "-", max(data_rows), "\n")
cat("any rows AFTER last data row?:", nrow(g) - max(data_rows), "\n")
cat("\nQuantity cells in data rows (unique):\n")
print(unique(unlist(g[data_rows, qcol])))
