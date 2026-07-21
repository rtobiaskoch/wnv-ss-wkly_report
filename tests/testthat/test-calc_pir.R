# test-calc_pir.R
# Tests for calc_pir() — Pooled Infection Rate (PIR) via CDC Firth MLE.
#
# Input: pool-level datasheet with test_code (0=negative, 1=positive)
# Output: PIR with 95% CI per zone/species/week
#
# Required columns: csu_id, trap_id, zone, zone2, method, spp, total, test_code, year, week
# zone_complete is set to "NW" to avoid the function completing against unrelated zones.
#
# NOTE: the single-pool edge-case handler this file used to warn about (and its
# sum(total, rm.na = T) typo) has been removed. Every PCR-tested pool, including
# pools of one mosquito, now goes through PooledInfRate::pIR() directly.

# All-negative pools: 3 pools for NW Tarsalis w35, all test_code = 0
pools_neg <- tibble::tribble(
  ~csu_id,    ~trap_id,  ~zone, ~zone2, ~method, ~spp,        ~year, ~week, ~total, ~test_code,
  "CSU00001", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    10,     0,
  "CSU00002", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    12,     0,
  "CSU00003", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    8,      0
)

# One positive pool among negatives
pools_pos <- tibble::tribble(
  ~csu_id,    ~trap_id,  ~zone, ~zone2, ~method, ~spp,        ~year, ~week, ~total, ~test_code,
  "CSU00001", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    15,     1,
  "CSU00002", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    10,     0,
  "CSU00003", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    8,      0
)

# Single-mosquito pool (negative) — the only pool in its group
pools_single <- tibble::tribble(
  ~csu_id,    ~trap_id,  ~zone, ~zone2, ~method, ~spp,        ~year, ~week, ~total, ~test_code,
  "CSU00001", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    1,      0
)

# Mirrors the real NW / 2025 / wk26 / Pipiens group: the group's ONLY positive
# pool (CSU23480) holds a single mosquito. Mixed pool sizes, gravid + light.
pools_single_pos <- tibble::tribble(
  ~csu_id,    ~trap_id,  ~zone, ~zone2, ~method, ~spp,       ~year, ~week, ~total, ~test_code,
  "CSU00001", "FC-073",  "NW",  "FC",   "L",     "Pipiens",  2025,  26,    1,      0,
  "CSU00002", "FC-061",  "NW",  "FC",   "L",     "Pipiens",  2025,  26,    3,      0,
  "CSU00003", "FC-090",  "NW",  "FC",   "G",     "Pipiens",  2025,  26,    6,      0,
  "CSU00004", "FC-011",  "NW",  "FC",   "L",     "Pipiens",  2025,  26,    1,      1
)

# Imputed row: species not caught in that trap, so no pool was tested.
# total == 0 and test_code == NA — the only rows calc_pir() should drop.
pools_imputed <- dplyr::bind_rows(
  pools_pos,
  tibble::tibble(
    csu_id = NA_character_, trap_id = "FC-002", zone = "NW", zone2 = "FC",
    method = "L", spp = "Tarsalis", year = 2025, week = 35,
    total = 0, test_code = NA_real_
  )
)

test_that("all-negative pools produce PIR = 0 and LCI = 0", {
  result <- calc_pir(pools_neg, zone_complete = "NW")
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  expect_equal(nw_tar$pir, 0)
  expect_equal(nw_tar$pir_lci, 0)
  # The Firth MLE correctly returns a positive UCI even when pir = 0:
  # the UCI represents the maximum infection rate consistent with the data.
  expect_gte(nw_tar$pir_uci, 0)
})

test_that("one positive pool produces PIR > 0", {
  result <- calc_pir(pools_pos, zone_complete = "NW")
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  expect_gt(nw_tar$pir, 0)
})

test_that("single-mosquito pool does not raise an error", {
  expect_no_error(calc_pir(pools_single, zone_complete = "NW"))
})

test_that("a positive single-mosquito pool is not dropped", {
  # Regression for CSU23480 (NW / 2025 / wk26 / Pipiens): the old
  # filter(total > 1) removed the group's only positive pool, forcing
  # pir = 0 and vi = 0 even though a positive pool existed.
  result <- calc_pir(pools_single_pos, zone_complete = "NW")
  nw_pip <- dplyr::filter(result, zone == "NW", spp == "Pipiens")
  expect_equal(nrow(nw_pip), 1)   # one row — no duplicate from an edge-case rbind
  expect_gt(nw_pip$pir, 0)
  expect_gt(nw_pip$pir_uci, nw_pip$pir_lci)
})

test_that("a group whose only pool is one mosquito still estimates", {
  one_pos <- dplyr::filter(pools_single_pos, csu_id == "CSU00004")
  nw_pip  <- dplyr::filter(calc_pir(one_pos, zone_complete = "NW"),
                           zone == "NW", spp == "Pipiens")
  expect_equal(nrow(nw_pip), 1)
  expect_equal(nw_pip$pir, 1)
})

test_that("imputed rows (total = 0, test_code = NA) are excluded", {
  # An untested imputed row must not change the estimate.
  expect_equal(
    dplyr::filter(calc_pir(pools_imputed, zone_complete = "NW"),
                  zone == "NW", spp == "Tarsalis")$pir,
    dplyr::filter(calc_pir(pools_pos, zone_complete = "NW"),
                  zone == "NW", spp == "Tarsalis")$pir
  )
})

test_that("csu_id in grp_var raises an error", {
  expect_error(
    calc_pir(pools_neg, grp_var = c("csu_id", "zone", "year", "week", "spp")),
    regexp = "Cannot calculate"
  )
})

test_that("missing required column raises an error", {
  # Remove 'total' — a required column — to trigger input validation
  bad_data <- dplyr::select(pools_neg, -total)
  expect_error(calc_pir(bad_data))
})

test_that("total = 0 rows are excluded even when test_code is not NA", {
  # This is the error the original filter(total > 1) was written to prevent:
  # PooledInfRate::pIR() fails with "missing value where TRUE/FALSE needed"
  # when handed a pool of zero mosquitoes carrying a real test_code.
  # Imputed rows normally have test_code = NA, but nothing enforces that --
  # a single data-entry slip would otherwise crash the pipeline.
  bad_impute <- dplyr::bind_rows(
    pools_pos,
    tibble::tibble(
      csu_id = NA_character_, trap_id = "FC-003", zone = "NW", zone2 = "FC",
      method = "L", spp = "Tarsalis", year = 2025, week = 35,
      total = 0, test_code = 0          # <- test_code present, not NA
    )
  )
  expect_no_error(calc_pir(bad_impute, zone_complete = "NW"))
  expect_equal(
    dplyr::filter(calc_pir(bad_impute, zone_complete = "NW"),
                  zone == "NW", spp == "Tarsalis")$pir,
    dplyr::filter(calc_pir(pools_pos, zone_complete = "NW"),
                  zone == "NW", spp == "Tarsalis")$pir
  )
})
