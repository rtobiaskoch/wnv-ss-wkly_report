# tests/fixtures/generate_fixtures.R
#
# Phase 2 stub — generates synthetic CSV fixtures for tests that require
# file-based inputs (wnv_s_clean, clean_pcr, key_rename, merge_4_database).
#
# Phase 1 tests (calc_abund, calc_pir, calc_vi) use inline tibble definitions
# and do NOT require CSV fixtures.
#
# Run once before implementing Phase 2 tests:
#   source("tests/fixtures/generate_fixtures.R")
#
# Fixtures to generate in Phase 2:
#   foco_trap.csv              — trap list: trap_id, method, zone, zone2, active, start, lat, long
#   key_rename.csv             — column rename lookup: new, old
#   wnv-s_database.csv         — pool database schema (2-4 rows, total mosq < 10)
#   slev-s_database.csv        — same schema as wnv-s_database.csv, empty (headers only)
#   culex_sheet_database.csv   — culex abundance: trap_id, zone, zone2, trap_date, year, week, spp, method, trap_status, total
#   standards_input.csv        — qPCR standards: year, week, well_position, plate, target, csu_id, sample_type, cq, log_copies
#   amp_inconclusive_negatives.csv — csu_id column only, empty

message("generate_fixtures.R: Phase 2 stub. No fixtures written yet.")
