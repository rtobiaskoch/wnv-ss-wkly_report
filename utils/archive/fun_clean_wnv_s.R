#' Clean Summary
#'
#' Report the outcome of a per-column cleaning step in a single semantic line.
#' Each call classifies the column's transition from `df0` -> `df` into one of:
#'   - added              (column did not exist in input)
#'   - no-op              (column existed, no values changed, no new NAs)
#'   - transformed        (values changed, no new NAs introduced)
#'   - cleaned (warning)  (values changed AND new NAs introduced)
#'
#' Ported from wnv-ss_trap_hx_combiner so the two pipelines log cleaning steps
#' identically (tidy cli alerts instead of noisy cat()). Behaviour is reporting
#' only — it never modifies data.
#'
#' @param df0 The input data frame, before the cleaning step.
#' @param df  The output data frame, after the cleaning step.
#' @param col_name Unquoted column name to summarise.
#' @param label Optional display label; defaults to the deparsed `col_name`.
#'
#' @return Invisibly NULL. Called for the side effect of printing a cli alert.
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo as_name
#' @export
clean_summary <- function(df0, df, col_name, label = deparse(substitute(col_name))) {
  col        <- rlang::enquo(col_name)
  col_string <- rlang::as_name(col)
  n_rows     <- nrow(df)

  # Case 1: column did not exist in input — it was added by the cleaning step.
  if (!col_string %in% names(df0)) {
    cli::cli_alert_info("{.field {label}} added ({n_rows} rows)")
    return(invisible(NULL))
  }

  # Coerce to character so type changes (e.g. character -> Date) compare
  # correctly without triggering charToDate. Comparison uses na.rm = TRUE
  # so NA-vs-NA rows contribute to neither changed nor unchanged.
  old_vals <- as.character(dplyr::pull(df0, !!col))
  new_vals <- as.character(dplyr::pull(df,  !!col))

  changed       <- sum(old_vals != new_vals, na.rm = TRUE)
  na_in         <- sum(is.na(old_vals))
  na_out        <- sum(is.na(new_vals))
  na_introduced <- max(0L, na_out - na_in)

  # Case 2: nothing happened to this column — quiet bullet.
  if (changed == 0 && na_introduced == 0) {
    cli::cli_alert("{.field {label}} no-op")
    return(invisible(NULL))
  }

  # Case 3: values changed AND new NAs appeared — surface as warning so the
  # user notices that some rows were nullified (e.g. failed regex, bad parse).
  if (na_introduced > 0) {
    cli::cli_alert_warning(
      "{.field {label}} cleaned ({changed} changed, {na_introduced} new NA)"
    )
    return(invisible(NULL))
  }

  # Case 4: clean transformation — values changed, no NA introduced.
  cli::cli_alert_success(
    "{.field {label}} transformed ({changed}/{n_rows} rows changed)"
  )
  invisible(NULL)
}





#POTENTIALLY A CLEAN COLUMN FUNCTION
# clean_column_with <- function(df0, df, col_2_clean, col_name, fn) {
#   if (col_name %in% names(df) && col_name %in% col_2_clean) {
#     col_sym <- rlang::sym(col_name)
#     
#     df <- df %>%
#       mutate(!!col_sym := fn(!!col_sym))
#     
#     clean_summary(df0, df, !!col_sym)
#   }
# }


#' Clean a Culex Surveillance Data Sheet
#'
#' Processes and standardizes raw Culex mosquito surveillance data.
#' Trims whitespace from character columns, parses collection dates using
#' parse_flexible_date(), assigns species and method categories, and
#' returns a cleaned data frame with selected columns.
#'
#' @param df A data frame containing raw Culex surveillance data. Must include the columns:
#' trap_name, date_trap_set, mosquito_species, trap_type, mosquito_count, and zone.
#'
#' @return A cleaned and standardized data frame with columns:
#' trap_id, trap_date, year, week, zone, spp, method, and total.
#'
#' @details This function depends on parse_flexible_date() which must be defined elsewhere
#' in the package or user's environment.
#'
#' @examples
#' \dontrun{
#' clean_df <- culex_sheet_clean(raw_df)
#' }
#'
#' @importFrom dplyr mutate across transmute case_when
#' @importFrom stringr str_detect str_remove
#' @importFrom purrr map_chr
#' @importFrom anytime anydate
#' @importFrom lubridate year week
#' @export

library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(janitor)

wnv_s_clean <- function(df,
                        all_cols = c("csu_id", "trap_id", "zone", "zone2",
                                     "trap_date", "year", "week",
                                     "spp","spp0", "method",
                                     "trap_status", "total"),
                        zone_lvls = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC"),
                        # zone_raw_lvls: codes actually present on a raw trap record.
                        # "FC" is excluded because it is never a real per-trap zone -
                        # it's the FC-subzone rollup built later in the zone2 step.
                        # Kept separate from zone_lvls (factor level order for
                        # downstream plots/tables) so extraction and ordering can
                        # diverge without one silently breaking the other.
                        zone_raw_lvls = setdiff(zone_lvls, "FC"),
                        distinct_col = names(df),
                        silence = F,
                        rm_dupes = T,
                        rm_col = c(),
                        force_recompute = FALSE # if TRUE, re-derive year/week from trap_date even when those columns already exist (use on RAW provider input; leave FALSE for stored/historical data)
                        ) {
  
  #save original input for comparison
  df0 = df
  
  df_name <- deparse(substitute(df))
  
  cat("\n CLEANING DATAFRAME ", df_name, "\n")
  
  # Check required cleaned columns
  col_2_clean = setdiff(all_cols, rm_col)
  present_cols <- intersect(all_cols, names(df))
  missing_cols <- setdiff(all_cols, names(df))
  
  
  
  cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
      "\n        C H E C K I N G   C O L U M N S                 \n",
      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  
  if (length(missing_cols) > 0) {
    cat("\n Notice. Following are not present for cleaning: ", paste(missing_cols, collapse = ", "), "\n")
    cat("Run key_rename to convert columns to standard naming convention.")
  }
  
  if (length(present_cols) > 0) {
    cat("\n The Following columns are being cleaned: ", paste(present_cols, collapse = ", "), "\n")
  }
  
  # Trim whitespace from all character columns
  df <- df %>%
    mutate(across(where(is.character), trimws))
  

  
  #REMOVE DUPLICATES
  dupes = janitor::get_dupes(df, !!!rlang::syms(distinct_col))
  n_dupes = nrow(dupes)
  
  cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
      "\n        C H E C K I N G   D U P L I C A T E S           \n",
      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  
  if(n_dupes > 0) {
    cat("\n", n_dupes, " Duplicates found.\n")
    #if rm_dupes is T remove them
    if(rm_dupes) {
      df = df %>% distinct(across(all_of(distinct_col)), .keep_all = T)
      cat("\n", nrow(df0)-nrow(df), " Duplicates removed using distinct on", paste(distinct_col, collapse = ","), "\n")
      dupes = janitor::get_dupes(df, !!!rlang::syms(distinct_col))
      cat("\n", nrow(dupes), " Duplicates remain\n")
      
    }#end if rm dupes
  } else {
    cat("\n No Duplicates found using", paste(distinct_col, collapse = ","), "\n")
  }
  
  df0 = df
  
  cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
      "\n        C L E A N I N G  C O L U M N S                 \n",
      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  
  
  # CLEAN csu_id
  if("csu_id" %in% names(df) & "csu_id" %in% col_2_clean) {
    
    df <- df %>%
      mutate(csu_id = str_remove(csu_id, "-"))
    if(!silence) {
      clean_summary(df0, df, csu_id) 
    }

  }
  
  
  # CLEAN ZONE
  if("zone" %in% names(df) & "zone" %in% col_2_clean) {
    
    #fix Berthoud to BE
    df <- df %>%
      mutate(zone = if_else(str_detect(zone, "Berthoud"), "BE", zone))
    
    # Create regex pattern like "NE|NW|SE|SW|LV|BE|BC" (excludes "FC" - see
    # zone_raw_lvls comment above; a raw record's zone string is never just
    # "FC", but providers do sometimes prefix subzones with it, e.g. "FC NE").
    zone_pattern <- str_c(zone_raw_lvls, collapse = "|")

    df <- df %>%
      mutate(zone = str_extract(zone, zone_pattern)) %>%
      mutate(zone = factor(zone, levels = zone_lvls))
    if(!silence) {
    clean_summary(df0, df, zone) 
    }
  }
  
  
  # CLEAN/GET ZONE2
  if("zone" %in% names(df) & "zone2" %in% col_2_clean) {
    
    # Create regex pattern like "NE|NW|SE|SW|LV|BE|BC"
    fc_zones <- c("NE", "NW", "SE", "SW")
    zone_pattern <- str_c(fc_zones, collapse = "|")
    
    df <- df %>%
      mutate(zone2 = if_else(zone %in% fc_zones, "FC", zone))
    if(!silence) {
    clean_summary(df0, df, zone2) }
  }

  
  # #CLEAN DATE
  # if ("trap_date" %in% names(df) & "trap_date" %in% col_2_clean) {
  #   if (!exists("parse_flexible_date", mode = "function")) {
  #   }
  #   
  #   df <- df %>%
  #     mutate(
  #       trap_date = purrr::map_chr(trap_date, ~ as.character(parse_flexible_date(.x))),
  #       trap_date = as.Date(trap_date)
  #     )
  #   
  #   clean_summary(df0, df, trap_date) 
  # }
  
  # CLEAN DATE (lubridate version)
  if ("trap_date" %in% names(df) & "trap_date" %in% col_2_clean) {
    
    # Convert to character and trim whitespace
    df <- df %>%
      mutate(trap_date = trimws(as.character(trap_date)))
    
  #CLEAN TRAP_DATE
    df <- df %>%
      mutate(
        trap_date = case_when(
          is.na(trap_date) | trap_date == "" ~ NA_character_,
          TRUE ~ {
            parsed <- lubridate::parse_date_time(trap_date, 
                                                 orders = c("ymd", "mdy", "dmy", "Ymd"),
                                                 quiet = TRUE)
            ifelse(is.na(parsed), NA_character_, as.character(parsed))
          }
        ),
        trap_date = as.Date(trap_date)
      )
    
    if(!silence) {
      clean_summary(df0, df, trap_date) 
    }
  }#end if trap_date
  
  # ADD YEAR
  # Derive year from trap_date when it is absent OR when force_recompute = TRUE.
  # force_recompute is used on RAW provider input (counts/datasheet) where a
  # collaborator may have typed a stale year (e.g. VDCI). isoyear() matches the
  # value stored historically; for summer surveillance dates isoyear == calendar year.
  if ("trap_date" %in% names(df) & (force_recompute | !"year" %in% names(df)) & "year" %in% col_2_clean) {
    df <- df %>%
      mutate(
        year = lubridate::isoyear(trap_date)
            )
    if(!silence) {
    clean_summary(df0, df, year) }
  }
  
  
  # CLEAN YEAR
  if ("year" %in% names(df) & "year" %in% col_2_clean) {
    df <- df %>%
      mutate(
        year = as.double(year)
      )
    if(!silence) {
    clean_summary(df0, df, year) }
  }
  
  
  # ADD WEEK
  # Seasonal/reported week from trap_date is the SINGLE week authority
  # (wnvSurv::calc_season_week): first full week of June is always week 23,
  # leap-week-stable. This is the IDENTICAL rule the pools path applies, so a
  # pool and the trap-count it came from always land in the same week.
  # The !"week" %in% names(df) gate keeps stored/historical data from being
  # recomputed on read-back, which is what holds the frozen baseline stable
  # mid-season. force_recompute = TRUE overrides that gate for RAW provider
  # input (e.g. BC types a running submission count, not a calendar week).
  if ("trap_date" %in% names(df) & (force_recompute | !"week" %in% names(df)) & "week" %in% col_2_clean) {
    df <- df %>%
    mutate(
      week = wnvSurv::calc_season_week(trap_date)
    )
    if(!silence) {
    clean_summary(df0, df, week)}
  }
  
  
  # CLEAN WEEK
  if ("week" %in% names(df) & "week" %in% col_2_clean) {
    df <- df %>%
      mutate(week = as.double(week))
    if(!silence) {
    clean_summary(df0, df, week)}
  }
  
  # Standardize trap_id and assign method
  
  #GET METHOD
  if ("trap_id" %in% names(df) & "trap_id" %in% col_2_clean) {
    df <- df %>%
      mutate(
        trap_id = toupper(trap_id),
        method = case_when(
          str_detect(tolower(trap_id), "gr") ~ "G",
          TRUE ~ "L"
        )
      )
    if(!silence) {
    clean_summary(df0, df, method) }
  }
  

  
  # CREATE TRAP_STATUS
  if ("spp" %in% names(df) & !"trap_status" %in% names(df) & "trap_status" %in% col_2_clean) {
    
    # Define grouped status logic
    df <- df %>%
      group_by(trap_id, trap_date) %>%
      mutate(
        trap_status = case_when(
          any(str_detect(spp, "(?i)malfunction|stolen")) ~ "malfunction",
          any(str_detect(spp, "(?i)no mosquitoes")) ~ "no mosquitoes",
          any(str_detect(spp, "(?i)tar|pip|Culex pipiens|Culex tarsalis")) & any(total > 0) ~ "culex",
          TRUE ~ "other spp"
        )
      ) %>%
      ungroup()
    if(!silence) {
    clean_summary(df0, df, trap_status) }
    
  }
  
  
  # CLEAN SPP
  if ("spp" %in% names(df) & "spp" %in% col_2_clean) {
    df <- df %>%
      mutate(spp = case_when(
                      str_detect(spp, "(?i)tar") ~ "Tarsalis",
                      str_detect(spp, "(?i)pip") ~ "Pipiens",
                      str_detect(spp,  "(?i)^all$") ~ "All",
                      str_detect(spp, "(?i)malfunction|stolen|no mosquitoes") ~ "none",
                      TRUE ~ "other spp"
                           )
          ) %>%
    mutate(spp = factor(spp, levels = c("Pipiens", "Tarsalis", "All", "other spp", "none")))
    if(!silence) {
    clean_summary(df0, df, spp) }
  }
  
  
  # CLEAN TOTAL 
  if ("total" %in% names(df) & "total" %in% col_2_clean) {
    df <- df %>%
      mutate(total = as.numeric(total))
    if(!silence) {
    clean_summary(df0, df, total) }
  }
  
  
  # CLEAN cq 
  if ("cq" %in% names(df) & "cq" %in% col_2_clean) {
    df <- df %>%
      mutate(cq = as.numeric(cq))
    if(!silence) {
    clean_summary(df0, df, cq) }
  }
  
  # CLEAN COPIES
  if ("copies" %in% names(df) & "copies" %in% col_2_clean) {
    df <- df %>%
      mutate(copies = as.numeric(copies))
    if(!silence) {
    clean_summary(df0, df, copies) }
  }
  
  if("trap_date" %in% names(df) & "trap_id" %in% names(df)) {
    df <- df %>%
      select(any_of(all_cols), everything()) %>%
      arrange(desc(trap_date), trap_id)
  } else {
    df <- df %>%
      select(any_of(all_cols), everything())
  }
  

  
  
  return(df)
}
