# utils/fun_bird_report.R
#
# DESCRIPTION
# Bird (dead-bird WNV surveillance) helpers. Pure: extract and shape only;
# the GSheet archive push (update_gsheet / sheet_write side effects) remains
# in the QMD bird chunk and is NOT part of this function.
# Extracted from the `birds` chunk of wnv-s_weekly_report_pipeline_v2.qmd.
#
# INPUTS (build_bird_report)
#   cq_data      merged qPCR results data frame / tibble. Must contain columns:
#                  sample_type, csu_id, year, week, target_name,
#                  test_code, cq, copies, amp_status
#   target       character — the target_name to summarise in bird_report
#                (default "WNV")
#   year_filter  integer/double, optional — current surveillance year. When
#                supplied with week_filter, bird_report_current is filtered to
#                just this year/week (see OUTPUT).
#   week_filter  integer/double, optional — current epiweek. See year_filter.
#
# OUTPUT (build_bird_report)
#   Named list with three elements:
#     birds                all bird records (sample_type == "bird"), tidy;
#                          year/week cast to double for natural merge with
#                          the culex database
#     bird_report          WNV positive/negative status per bird csu_id
#                          spanning the full archive (birds_hx + current week)
#                          — used for the season-long plot
#     bird_report_current  bird_report filtered to year_filter/week_filter —
#                          used for the per-week GSheet tab so it doesn't
#                          leak prior weeks' results into this week's sheet.
#                          NULL if year_filter/week_filter not supplied.
#
# DEPENDENCIES
#   dplyr  (explicit namespace — no library() call here)
#   ggplot2 for plot_birds()

# ---------------------------------------------------------------------------
# build_bird_report()
# ---------------------------------------------------------------------------
build_bird_report <- function(cq_data, target = "WNV", birds_hx = NULL,
                               year_filter = NULL, week_filter = NULL) {
  # -- birds: current-week bird rows, year/week cast to double for merge -------
  # sample_type == "bird" excludes mosquito and other sample types present in
  # cq_data; year/week stored as character from the GSheet export so we cast
  # them here (mirrors the QMD: mutate(year = as.double(year), ...)).
  birds <- cq_data %>%
    dplyr::filter(sample_type == "bird") %>%
    dplyr::mutate(
      year = as.double(year),
      week = as.double(week)
    ) %>%
    dplyr::select(
      csu_id,
      year,
      week,
      target_name,
      test_code,
      cq,
      copies,
      amp_status
    ) %>%
    dplyr::arrange(year, week, csu_id)

  # -- birds_all: combine archive + current week for the plot ------------------
  # birds_hx (GSheet archive) is bound first so that the current week's values
  # win on dedup. Dedup key mirrors update_gsheet: csu_id + year + week + target.
  birds_all <- if (!is.null(birds_hx)) {
    dplyr::bind_rows(
      dplyr::mutate(birds_hx,
                    year = as.double(year),
                    week = as.double(week)),
      birds
    ) %>%
      dplyr::distinct(csu_id, year, week, target_name, .keep_all = TRUE)
  } else {
    birds
  }

  # -- bird_report: WNV result per bird csu_id --------------------------------
  # Derived from birds_all so the plot spans all archived weeks, not just this
  # week's PCR run. arrange(desc(test_code)) surfaces positives first.
  # NOTE: not deduplicated beyond the csu_id+year+week+target key above — a
  # bird run on multiple plates within the same week is still collapsed by the
  # distinct() above; revisit only if that assumption breaks.
  bird_report <- birds_all %>%
    dplyr::filter(target_name == target) %>%
    dplyr::arrange(dplyr::desc(test_code)) %>%
    dplyr::mutate(
      wnv_result = dplyr::if_else(test_code == 1, "positive", "negative")
    ) %>%
    dplyr::select(csu_id, year, week, wnv_result)

  # -- bird_report_current: this week's results only, for the per-week sheet --
  # bird_report spans the full archive (needed for plot_birds' season trend);
  # the per-week GSheet tab must not inherit those prior years/weeks.
  bird_report_current <- if (!is.null(year_filter) && !is.null(week_filter)) {
    dplyr::filter(bird_report, year == year_filter, week == week_filter)
  } else {
    NULL
  }

  list(birds = birds, bird_report = bird_report, bird_report_current = bird_report_current)
}

# ---------------------------------------------------------------------------
# plot_birds()
# ---------------------------------------------------------------------------
# Bar chart of WNV result counts by epiweek. Categorical fill uses a manual
# palette with red for positives and grey for negatives (project convention:
# theme_classic(); no library() calls; explicit ggplot2:: namespace).
#
# INPUTS
#   bird_report  data frame with columns: year, week, wnv_result (and any others)
#   year         integer — filter to this surveillance year before plotting;
#                NULL (default) plots all years (weeks from different years collapse)
#   pal          named character vector mapping wnv_result values to colours
plot_birds <- function(
  bird_report,
  year = NULL,
  pal = c("positive" = "#c5283d", "negative" = "grey70")
) {
  if (!is.null(year)) {
    bird_report <- dplyr::filter(bird_report, .data$year == .env$year)
  }
  bird_report %>%
    dplyr::mutate(week = as.factor(week)) %>%
    dplyr::count(week, wnv_result) %>%
    ggplot2::ggplot(ggplot2::aes(x = week, y = n, fill = wnv_result)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      x = "Week",
      y = "Birds tested",
      fill = "WNV result",
      title = "RMRP Bird WNV Surveillance"
    ) +
    ggplot2::theme_classic()
}
