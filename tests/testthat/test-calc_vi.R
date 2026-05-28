# test-calc_vi.R
# Tests for calc_vi() — Virus Index = abundance × pooled infection rate.
#
# calc_vi() is called with complete = FALSE throughout.
# The default complete = TRUE branches on the global variable `grp_zones` (from
# config_weekly.R), which is not available in the test environment. Tests that
# need zone completion should mock grp_zones or set complete = FALSE.
#
# Strategy: build abund and pir from the same synthetic data so that all
# groups match, then verify the arithmetic and output schema.

# Culex trap counts — 2 traps in NW, both Tarsalis and Pipiens
culex_for_vi <- tibble::tribble(
  ~trap_id,  ~zone, ~zone2, ~year, ~week, ~spp,        ~method, ~trap_status, ~total,
  "FC-001",  "NW",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",      10,
  "FC-002",  "NW",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",      8,
  "FC-001",  "NW",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",      4,
  "FC-002",  "NW",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",      2
)

# Pool data — one positive Tarsalis pool; all Pipiens pools negative
pools_for_vi <- tibble::tribble(
  ~csu_id,    ~trap_id,  ~zone, ~zone2, ~method, ~spp,        ~year, ~week, ~total, ~test_code,
  "CSU00001", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    10,     1,
  "CSU00002", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    8,      0,
  "CSU00003", "FC-001",  "NW",  "FC",   "L",     "Pipiens",   2025,  35,    4,      0,
  "CSU00004", "FC-001",  "NW",  "FC",   "L",     "Pipiens",   2025,  35,    2,      0
)

abund_res <- calc_abund(culex_for_vi)
pir_res   <- calc_pir(pools_for_vi, zone_complete = "NW")
vi_res    <- calc_vi(abund_res, pir_res, complete = FALSE)

test_that("calc_vi output contains vi, vi_lci, vi_uci columns", {
  expect_true(all(c("vi", "vi_lci", "vi_uci") %in% names(vi_res)))
})

test_that("vi = round(abund * pir, 2) for matching groups", {
  nw_tar_abund <- dplyr::filter(abund_res, zone == "NW", spp == "Tarsalis")
  nw_tar_pir   <- dplyr::filter(pir_res,   zone == "NW", spp == "Tarsalis")
  nw_tar_vi    <- dplyr::filter(vi_res,    zone == "NW", spp == "Tarsalis")
  expect_equal(
    nw_tar_vi$vi,
    round(nw_tar_abund$abund * nw_tar_pir$pir, 2)
  )
})

test_that("vi = 0 when all pools for a species are negative", {
  # All Pipiens pools have test_code = 0, so pir = 0 and vi = abund * 0 = 0
  nw_pip <- dplyr::filter(vi_res, zone == "NW", spp == "Pipiens")
  expect_equal(nw_pip$vi, 0)
})

test_that("Tarsalis vi > 0 when there is a positive pool", {
  nw_tar <- dplyr::filter(vi_res, zone == "NW", spp == "Tarsalis")
  expect_gt(nw_tar$vi, 0)
})
