# diff_week_baseline.R
# Phase-2 STOP-gate check: quantify how the week-keying correction (calc_season_week
# replacing coalesce(submitter, isoweek)) shifts (a) per-row week labels and (b) the
# historical baseline the weekly report shows.
#
# Run from the report repo root:
#   Rscript sandbox/diff_week_baseline.R
#
# Outputs (written to sandbox/):
#   - week_baseline_diff_cells.csv   per (week, zone, spp) abundance: old vs new + delta
#   - week_baseline_diff_rows.csv    per dated row: old week vs new week (only changed)
#   - week_baseline_diff_plot.png    old vs new baseline overlay, faceted by zone
# Plus a console summary.

suppressMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
})

# --- paths: OLD = current pre-fix output; NEW = freshly re-rendered combiner output ---
# Optional override: Rscript sandbox/diff_week_baseline.R <old_path> <new_path>
.args <- commandArgs(trailingOnly = TRUE)
old_path <- if (length(.args) >= 1) {
  .args[1]
} else {
  "sandbox/culex_sheet_database_expand_OLD.csv"
}
new_path <- if (length(.args) >= 2) {
  .args[2]
} else {
  "sandbox/culex_sheet_database_expand_NEW.csv"
}

# Rolling historical window for the current (2026) report = 5 prior seasons.
window_years <- 2021:2025

stopifnot("OLD expand file not found" = file.exists(old_path))
stopifnot(
  "NEW expand file not found — re-render the combiner first" = file.exists(
    new_path
  )
)

old <- read_csv(old_path, show_col_types = FALSE)
new <- read_csv(new_path, show_col_types = FALSE)

cat("\n=== files ===\n")
cat("OLD:", old_path, "-", nrow(old), "rows\n")
cat("NEW:", new_path, "-", nrow(new), "rows\n")
cat("row-count delta (new - old):", nrow(new) - nrow(old), "\n")

# --- (A) per-trap-date week-label change (one week per trap-date, flag duplicates) ---
# The expand file can carry the SAME (trap_id, spp, year, trap_date) under more than
# one week (legacy duplicates / bad dates). Collapse to one week per trap-date and
# flag the ambiguous ones, so the old<->new comparison is not polluted by a
# many-to-many self-match.
key_cols <- c("trap_id", "spp", "year", "trap_date")
collapse <- function(df) {
  df %>%
    filter(!is.na(trap_date)) %>%
    distinct(trap_id, spp, year, trap_date, week) %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(
      week = dplyr::first(week),
      ambiguous = dplyr::n_distinct(week) > 1,
      .groups = "drop"
    )
}
old_dated <- collapse(old) %>% rename(week_old = week, ambig_old = ambiguous)
new_dated <- collapse(new) %>% rename(week_new = week, ambig_new = ambiguous)

row_join <- inner_join(old_dated, new_dated, by = key_cols) %>%
  mutate(delta = week_new - week_old)

changed_rows <- row_join %>% filter(delta != 0 | is.na(delta))

cat("\n=== (A) per-trap-date week label changes ===\n")
cat("trap-dates matched old<->new   :", nrow(row_join), "\n")
cat(
  "ambiguous trap-dates (old / new):",
  sum(old_dated$ambig_old),
  "/",
  sum(new_dated$ambig_new),
  "\n"
)
cat(
  "trap-dates whose week changed  :",
  nrow(changed_rows),
  sprintf("(%.1f%%)\n", 100 * nrow(changed_rows) / nrow(row_join))
)
cat("week delta distribution (new - old):\n")
print(row_join %>% count(delta) %>% arrange(delta) %>% as.data.frame())
write_csv(changed_rows, "sandbox/week_baseline_diff_rows.csv")

# --- (B) historical baseline: mean abundance per (week, zone, spp) over the window ---
# The expand file already carries the imputed zeros ("no mosquitoes" = 0) and NA
# malfunctions/no-trap, so mean(total, na.rm = TRUE) approximates the report's
# abundance (mean mosquitoes per trap per night).
baseline <- function(df, lbl) {
  df %>%
    filter(year %in% window_years) %>%
    group_by(week, zone, spp) %>%
    summarise(
      abund = mean(total, na.rm = TRUE),
      n_obs = sum(!is.na(total)),
      .groups = "drop"
    ) %>%
    rename(!!paste0("abund_", lbl) := abund, !!paste0("n_", lbl) := n_obs)
}

cells <- full_join(
  baseline(old, "old"),
  baseline(new, "new"),
  by = c("week", "zone", "spp")
) %>%
  mutate(abund_delta = coalesce(abund_new, 0) - coalesce(abund_old, 0)) %>%
  arrange(desc(abs(abund_delta)))

write_csv(cells, "sandbox/week_baseline_diff_cells.csv")

cat(
  "\n=== (B) historical baseline (",
  min(window_years),
  "-",
  max(window_years),
  ") per (week, zone, spp) ===\n",
  sep = ""
)
cat("cells total              :", nrow(cells), "\n")
cat(
  "cells with any change    :",
  sum(abs(cells$abund_delta) > 1e-9, na.rm = TRUE),
  "\n"
)
cat(
  "max |abundance delta|    :",
  round(max(abs(cells$abund_delta), na.rm = TRUE), 3),
  "\n"
)
cat("top 10 most-changed cells:\n")
print(cells %>% slice_head(n = 10) %>% as.data.frame())

# --- (B) overlay plot: old vs new baseline by week, faceted by zone (All spp) ---
plot_df <- bind_rows(
  baseline(old, "old") %>%
    transmute(week, zone, spp, abund = abund_old, set = "old"),
  baseline(new, "new") %>%
    transmute(week, zone, spp, abund = abund_new, set = "new")
) %>%
  filter(spp == "Tarsalis")

p <- ggplot(plot_df, aes(week, abund, colour = set)) +
  geom_line() +
  facet_wrap(~zone, scales = "free_y") +
  theme_classic() +
  labs(
    title = paste0(
      "Historical baseline old vs new (",
      min(window_years),
      "-",
      max(window_years),
      ")"
    ),
    subtitle = "week-keying correction: calc_season_week vs coalesce(submitter, isoweek)"
  )
ggsave(
  "sandbox/week_baseline_diff_plot.png",
  p,
  width = 11,
  height = 7,
  dpi = 120
)

cat(
  "\nWrote: sandbox/week_baseline_diff_cells.csv, week_baseline_diff_rows.csv, week_baseline_diff_plot.png\n"
)
