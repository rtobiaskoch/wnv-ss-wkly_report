suppressMessages({library(dplyr); library(stringr); library(readr); library(readxl)
  library(purrr); library(lubridate); library(tidyr); library(janitor)})
cq <- readRDS("2_mid/2026/w23/y2026_w23_cq_data.RData")
source("utils/fun_read_list.R")
df_key_rename <- read.csv("1_input/2026/w23/key_rename.csv")
tid <- "Collection Site       (Trap ID)"
dsi <- read_list("1_input/2026/w23/datasheet", "*")
dsi <- dsi[!is.na(dsi[[tid]]), ]
dsc <- dsi %>% wnvSurv::key_rename(rename_df = df_key_rename, drop_extra = TRUE) %>%
  wnvSurv::wnv_s_clean()

cq_ids  <- sort(unique(cq$csu_id[str_detect(cq$csu_id, "^(CSU|BOU)")]))
dsc_ids <- sort(unique(dsc$csu_id))

cat("CSU pools: in datasheet AND cq (matched):",
    length(intersect(dsc_ids[str_detect(dsc_ids,"^CSU")], cq_ids)),
    "/ datasheet CSU:", sum(str_detect(dsc_ids,"^CSU")), "\n")
cat("BOU pools: in datasheet AND cq (matched):",
    length(intersect(dsc_ids[str_detect(dsc_ids,"^BOU")], cq_ids)),
    "/ datasheet BOU:", sum(str_detect(dsc_ids,"^BOU")), "\n")
cat("\ndatasheet BOU ids:", paste(dsc_ids[str_detect(dsc_ids,"^BOU")], collapse=", "), "\n")
cat("cq       BOU ids:", paste(cq_ids[str_detect(cq_ids,"^BOU")], collapse=", "), "\n")
