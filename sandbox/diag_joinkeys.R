suppressMessages({library(dplyr); library(stringr); library(readr); library(readxl)
  library(purrr); library(lubridate); library(tidyr); library(janitor)})

# ---- cq side (BOU) ----
cq <- readRDS("2_mid/2026/w23/y2026_w23_cq_data.RData")
cqb <- cq[str_detect(str_to_upper(cq$csu_id), "BOU"), ]
cat("=== cq_data BOU join keys ===\n")
print(as.data.frame(cqb[, intersect(c("csu_id","year","week","target_name","test_code"), names(cqb))]))
cat("cq types: csu_id", class(cq$csu_id), "| year", class(cq$year), "| week", class(cq$week), "\n")

# ---- datasheet_clean side (reconstruct exactly as the qmd does) ----
csw <- tryCatch(wnvSurv::calc_season_week, error=function(e) NULL)
source("utils/fun_read_list.R")
df_key_rename <- read.csv("1_input/2026/w23/key_rename.csv")
kr <- tryCatch(wnvSurv::key_rename, error=function(e) NULL)
wsc <- tryCatch(wnvSurv::wnv_s_clean, error=function(e) NULL)
cat("\nkey_rename from pkg:", !is.null(kr), " | wnv_s_clean from pkg:", !is.null(wsc), "\n")

tid <- "Collection Site       (Trap ID)"
dsi <- read_list("1_input/2026/w23/datasheet", "*")
dsi <- dsi[!is.na(dsi[[tid]]), ]
dsc <- dsi %>% kr(rename_df = df_key_rename, drop_extra = TRUE) %>% wsc()
cat("\ndatasheet_clean cols:", paste(names(dsc), collapse=", "), "\n")
dcb <- dsc[str_detect(str_to_upper(dsc$csu_id), "BOU"), ]
cat("\n=== datasheet_clean BOU join keys ===\n")
print(as.data.frame(dcb[, intersect(c("csu_id","year","week","trap_id","test_code"), names(dcb))]))
cat("dsc types: csu_id", class(dsc$csu_id), "| year", class(dsc$year), "| week", class(dsc$week), "\n")
