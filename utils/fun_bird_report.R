# utils/fun_bird_report.R
#
# DESCRIPTION
# Bird (dead-bird WNV surveillance) helpers. Pure: extract and shape only;
# the GSheet archive push (update_gsheet / sheet_write side effects) remains
# in the QMD bird chunk and is NOT part of this function.
# Extracted from the `birds` chunk of wnv-s_weekly_report_pipeline_v2.qmd.
#
# INPUTS (build_bird_report)
#   cq_data  merged qPCR results data frame / tibble. Must contain columns:
#              sample_type, csu_id, year, week, target_name,
#              test_code, cq, copies, amp_status
#   target   character — the target_name to summarise in bird_report
#            (default "WNV")
#
# OUTPUT (build_bird_report)
#   Named list with two elements:
#     birds        all bird records (sample_type == "bird"), tidy; year/week
#                  cast to double for natural merge with the culex database
#     bird_report  WNV positive/negative status per bird csu_id for the
#                  current run (filtered to `target`, one row per csu_id)
#
# DEPENDENCIES
#   dplyr  (explicit namespace — no library() call here)
#   ggplot2 for plot_birds()

# ---------------------------------------------------------------------------
# build_bird_report()
# ---------------------------------------------------------------------------
build_bird_report <- function(cq_data, target = "WNV") {

  # -- birds: all bird rows, year/week cast to double for merge ---------------
  # sample_type == "bird" excludes mosquito and other sample types present in
  # cq_data; year/week stored as character from the GSheet export so we cast
  # them here (mirrors the QMD: mutate(year = as.double(year), ...)).
  birds <- cq_data %>%
    dplyr::filter(sample_type == "bird") %>%
    dplyr::mutate(
      year = as.double(year),
      week = as.double(week)
    ) %>%
    dplyr::select(csu_id, year, week, target_name, test_code,
                  cq, copies, amp_status) %>%
    dplyr::arrange(year, week, csu_id)

  # -- bird_report: WNV result per bird csu_id --------------------------------
  # Filter to the requested target (default "WNV"), derive a readable result
  # label from test_code (1 = positive, 0 = negative). arrange(desc(test_code))
  # surfaces positives first, matching the QMD ordering.
  # NOTE: not deduplicated — a bird run on multiple plates yields multiple rows.
  # This is faithful to the QMD chunk (which also does not dedup); revisit only
  # if upstream data legitimately produces duplicate csu_id rows.
  bird_report <- birds %>%
    dplyr::filter(target_name == target) %>%
    dplyr::arrange(dplyr::desc(test_code)) %>%
    dplyr::mutate(
      wnv_result = dplyr::if_else(test_code == 1, "positive", "negative")
    ) %>%
    dplyr::select(csu_id, year, week, wnv_result)

  list(birds = birds, bird_report = bird_report)
}

# ---------------------------------------------------------------------------
# plot_birds()
# ---------------------------------------------------------------------------
# Bar chart of WNV result counts by epiweek. Categorical fill uses a manual
# palette with red for positives and grey for negatives (project convention:
# theme_classic(); no library() calls; explicit ggplot2:: namespace).
#
# INPUTS
#   bird_report  data frame with columns: week, wnv_result (and any others)
#   pal          named character vector mapping wnv_result values to colours
plot_birds <- function(bird_report,
                       pal = c("positive" = "#c5283d", "negative" = "grey70")) {
  bird_report %>%
    dplyr::count(week, wnv_result) %>%
    ggplot2::ggplot(ggplot2::aes(x = week, y = n, fill = wnv_result)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      x     = "Week",
      y     = "Birds tested",
      fill  = "WNV result",
      title = "Dead bird WNV surveillance"
    ) +
    ggplot2::theme_classic()
}
