# test-calc_abund.R
# Tests for calc_abund() — mosquito abundance (count/trap-night) calculation.
#
# Input: cleaned all-species culex datasheet (trap-level counts per zone/species)
# Output: grouped abundance with 95% CI per zone/species/week
#
# Synthetic data covers:
#   - 2 base zones (NW, SE), 2 species (Tarsalis, Pipiens), 2 traps per group
#   - BC zone with extreme variance (total=0 vs 50) to test LCI floor enforcement
#   - One non-L method trap (G=gravid) that must be excluded
#   - One malfunction trap that must be excluded

culex_data <- tibble::tribble(
  ~trap_id,  ~zone, ~zone2, ~year, ~week, ~spp,        ~method, ~trap_status,  ~total,
  # NW zone — 2 L-traps, Tarsalis + Pipiens
  "FC-001",  "NW",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       10,
  "FC-002",  "NW",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       8,
  "FC-001",  "NW",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       20,
  "FC-002",  "NW",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       16,
  # SE zone — 2 L-traps, Tarsalis + Pipiens
  "FC-003",  "SE",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       5,
  "FC-004",  "SE",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       3,
  "FC-003",  "SE",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       15,
  "FC-004",  "SE",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       13,
  # BC zone — extreme variance (total 0 vs 50) → raw LCI is -24, clamped to 0
  "BC-001",  "BC",  "BC",   2025,  35,    "Tarsalis",  "L",     "culex",       0,
  "BC-002",  "BC",  "BC",   2025,  35,    "Tarsalis",  "L",     "culex",       50,
  # Non-L method (gravid trap) — must be excluded from abundance
  "FC-005",  "NW",  "FC",   2025,  35,    "Tarsalis",  "G",     "culex",       100,
  # Malfunction trap — must be excluded from abundance
  "FC-006",  "SE",  "FC",   2025,  35,    "Tarsalis",  "L",     "malfunction", 999
)

result <- calc_abund(culex_data)

test_that("calc_abund returns one row per zone-species group", {
  # NW×Tar, NW×Pip, SE×Tar, SE×Pip, BC×Tar = 5 rows
  expect_equal(nrow(result), 5)
})

test_that("calc_abund computes mosq_L, trap_L, and abund correctly", {
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  # NW Tarsalis: 2 traps with 10 and 8 mosquitoes
  expect_equal(nw_tar$trap_L, 2)
  expect_equal(nw_tar$mosq_L, 18)
  expect_equal(nw_tar$abund, 9.0)
})

test_that("calc_abund excludes non-L method traps", {
  # FC-005 is method=G (gravid trap) with 100 mosquitoes
  # If included: NW Tarsalis mosq_L = 10+8+100 = 118 and trap_L = 3
  # If excluded: mosq_L = 18, trap_L = 2
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  expect_equal(nw_tar$mosq_L, 18)
  expect_equal(nw_tar$trap_L, 2)
})

test_that("calc_abund excludes malfunction traps", {
  # FC-006 is trap_status=malfunction with 999 mosquitoes
  # If included: SE Tarsalis mosq_L = 5+3+999 = 1007 and trap_L = 3
  # If excluded: mosq_L = 8, trap_L = 2
  se_tar <- dplyr::filter(result, zone == "SE", spp == "Tarsalis")
  expect_equal(se_tar$mosq_L, 8)
  expect_equal(se_tar$trap_L, 2)
})

test_that("abund_lci is never negative", {
  # BC zone: abund=25, sd=35.36, raw LCI = 25 - 1.96*25 ≈ -24 → clamped to 0
  expect_true(all(result$abund_lci >= 0, na.rm = TRUE))
  bc_tar <- dplyr::filter(result, zone == "BC", spp == "Tarsalis")
  expect_equal(bc_tar$abund_lci, 0)
})
