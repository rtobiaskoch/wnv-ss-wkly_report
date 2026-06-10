suppressMessages(library(dplyr))

old <- tibble::tibble(
  key = c("a", "b"),
  spp = factor(c("Pipiens", "Tarsalis"), levels = c("Pipiens","Tarsalis","All","other spp","none")),
  zone = factor("NW", levels = c("NW","NE")),
  trap_date = as.Date(c("2025-01-01","2025-01-02"))
)

new <- tibble::tibble(
  key = "c",
  spp = factor("All", levels = c("Pipiens","Tarsalis","All","other spp","none")),
  zone = factor("NE", levels = c("NW","NE")),
  trap_date = as.Date("2026-06-01")
)

shared_cols <- setdiff(intersect(names(new), names(old)), "key")

joined <- dplyr::full_join(new, old, by = "key", suffix = c(".x",".y"))

coalesced <- purrr::reduce(shared_cols, function(df, col) {
  dplyr::mutate(df, !!col := dplyr::coalesce(.data[[paste0(col,".x")]], .data[[paste0(col,".y")]]))
}, .init = joined)

out <- coalesced %>% dplyr::select(key, all_of(shared_cols))
print(out)
print(sapply(out, class))
