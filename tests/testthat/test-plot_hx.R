# Tests for clean_long_hx_wk() and plot_hx() (utils/fun_plot_hx.R)
# zone_lvls and pal_mozzy are provided by helper-setup.R (globalenv).

test_that("clean_long_hx_wk keeps Pipiens and Tarsalis (not just All)", {
  mk <- function(type) tibble::tibble(
    year = 2026, week = c(23, 23, 23),
    zone = factor("NW", levels = zone_lvls),
    spp  = factor(c("Pipiens", "Tarsalis", "All"),
                  levels = c("Pipiens", "Tarsalis", "All", "other spp", "none")),
    abund = c(1, 2, 3), pir = c(0.1, 0.2, 0), vi = c(0.1, 0.2, 0.3),
    type = type
  )
  ytd <- mk("current"); hx <- mk("hx")

  out <- clean_long_hx_wk(ytd, hx, spp_keep = c("Pipiens", "Tarsalis"))

  expect_setequal(as.character(unique(out$spp)), c("Pipiens", "Tarsalis"))
})

test_that("plot_hx stacks species with a type_spp fill key covered by the palette", {
  # Realistic fixture: a span of weeks so geom_area actually renders (a single
  # week per group makes stat_align hit empty groups and warn spuriously).
  df <- tidyr::expand_grid(
    week = 23:27,
    zone = factor(c("NW", "NE"), levels = zone_lvls),
    spp  = c("Pipiens", "Tarsalis"),
    type = factor(c("hx", "current"), levels = c("hx", "current"))
  ) |>
    dplyr::mutate(abund = seq_len(dplyr::n()) / 10, pir = 0, vi = 0)

  p <- plot_hx(df, abund, "Abundance", pallette = pal_mozzy)

  expect_s3_class(p, "ggplot")
  # combined type_spp key spans all four pal_mozzy entries
  expect_setequal(
    unique(p$data$grp),
    c("current_Pipiens", "current_Tarsalis", "hx_Pipiens", "hx_Tarsalis")
  )
  # every grp value has a matching palette colour (else fill silently drops)
  expect_true(all(unique(p$data$grp) %in% names(pal_mozzy)))
  # builds with no warnings on realistic data (catches palette/key mismatch,
  # which would emit "No shared levels found ...")
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("plot_hx overlays hx and current on the same zone panel (facet by zone only)", {
  df <- tidyr::expand_grid(
    week = 23:27,
    zone = factor(c("NW", "NE"), levels = zone_lvls),
    spp  = c("Pipiens", "Tarsalis"),
    type = factor(c("hx", "current"), levels = c("hx", "current"))
  ) |>
    dplyr::mutate(abund = seq_len(dplyr::n()) / 10, pir = 0, vi = 0)

  p <- plot_hx(df, abund, "Abundance", pallette = pal_mozzy)

  # facets only on zone, not on type -> hx and current share one panel per zone
  facet_vars <- ggplot2::ggplot_build(p)$layout$facet$vars()
  expect_setequal(as.character(facet_vars), "zone")

  # two geom_area layers: hx drawn first (background), current drawn second (overlay)
  area_layers <- Filter(function(l) inherits(l$geom, "GeomArea"), p$layers)
  expect_equal(length(area_layers), 2)
  expect_setequal(unique(area_layers[[1]]$data$type), "hx")
  expect_setequal(unique(area_layers[[2]]$data$type), "current")
})
