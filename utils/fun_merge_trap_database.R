# fun_merge_trap_database.R
#
# Pure database-merge step shared by update_gsheet() (GSheet write path) and the
# clean-update-all-spp-culex chunk (local CSV path). Extracted so the merge can
# be unit-tested in isolation — it has no I/O and touches no global state.
#
# WHY a FULL natural_join on a single composite `key`:
#   rquery::natural_join COALESCES the two tables on the `by` columns, preferring
#   the FIRST table (`new`). The trap-history combiner pre-seeds the database with
#   keyed "no trap" stub rows; joining on `key` (= trap_id|spp|year|week, which
#   EXCLUDES trap_date) lets each week's real row land on its stub instead of
#   duplicating it. Joining on trap_date — as the old code did — failed because
#   stubs carry trap_date = NA, so a real row never matched (see plan/context).

#' Merge new trap data into the existing database via a composite-key join
#'
#' @param new           Data frame of new/this-week rows (preferred on conflict).
#' @param old           Data frame of the existing database (pre-seeded stubs +
#'                      history).
#' @param by            Character vector of join keys. Default "key".
#' @param col_database  Column names (and order) to keep in the result. Defaults
#'                      to the database's columns so the schema is preserved.
#' @param jointype      rquery natural_join type. Default "FULL" (keep unmatched
#'                      rows from both sides).
#' @return A data frame with one row per `by` value, `new` values winning on
#'         conflict, columns ordered as `col_database`.
merge_trap_database <- function(new,
                                old,
                                by = "key",
                                col_database = names(old),
                                jointype = "FULL") {

  # COALESCE-merge on the key; natural_join reorders columns, so re-select to
  # restore the database schema/order.
  out <- rquery::natural_join(new, old, jointype = jointype, by = by) %>%
    dplyr::select(dplyr::all_of(col_database))

  # Restore trap_date to a Date, then sort newest-first. natural_join coerces
  # the join inputs, and the column can arrive in two forms depending on the
  # caller's types:
  #   - numeric: a Date column was coerced to days-since-epoch (e.g. 20605) ->
  #     lubridate::as_date() reads numerics as days, giving the right date.
  #   - character: a "YYYY-MM-DD" string (the production GSheet/CSV path) ->
  #     parse via as_datetime() then as_date(), as the original code did.
  # Using as_datetime() on the numeric form would misread days as SECONDS and
  # silently collapse every date to 1970-01-01 — the bug this branch avoids.
  if ("trap_date" %in% names(out)) {
    out$trap_date <- if (is.numeric(out$trap_date)) {
      lubridate::as_date(out$trap_date)
    } else {
      lubridate::as_date(lubridate::as_datetime(out$trap_date))
    }
    out <- dplyr::arrange(out, dplyr::desc(trap_date))
  }

  out
}
