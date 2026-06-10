suppressMessages({
  library(dplyr)
  library(rquery)
})

old <- tibble::tibble(
  key = c("a", "b"),
  spp = factor(c("Pipiens", "Tarsalis"), levels = c("Pipiens","Tarsalis","All","other spp","none")),
  zone = factor("NW", levels = c("NW","NE"))
)

new <- tibble::tibble(
  key = "c",
  spp = factor("All", levels = c("Pipiens","Tarsalis","All","other spp","none")),
  zone = factor("NE", levels = c("NW","NE"))
)

out <- rquery::natural_join(new, old, jointype = "FULL", by = "key") %>%
  dplyr::select(key, spp, zone)

print(out)
print(sapply(out, class))
