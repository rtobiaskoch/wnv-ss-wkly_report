# test_pIR_single_pool.R -------------------------------------------------------
# Question: does PooledInfRate::pIR() actually fail on pools of total == 1,
# or does it fail on something else that the `total > 1` filter happened to mask?
#
# Context: utils/fun_calc_pir.R:50 drops every pool with total <= 1 before
# calling pIR(). That also drops REAL single-mosquito pools that tested
# positive (e.g. CSU23480, NW 2025 wk26 Pipiens), forcing pir = 0 and vi = 0.
# The lines 73-93 fallback was added because pIR() threw an error -- this
# script isolates WHICH input actually throws.
#
# Run: Rscript sandbox/test_pIR_single_pool.R

suppressMessages(library(PooledInfRate))

# Helper: run pIR the same way calc_pir() does, and report result or error.
# Pure: takes a data frame, returns nothing but prints. No global state.
try_pir <- function(label, df) {
  cat("\n---", label, "---\n")
  print(df)
  out <- try(
    PooledInfRate::pIR(test_code ~ total | grp, data = df, pt.method = "firth"),
    silent = TRUE
  )
  if (inherits(out, "try-error")) {
    cat("ERROR:", conditionMessage(attr(out, "condition")), "\n")
  } else {
    print(out)
  }
  invisible(NULL)
}

# CASE 1 -----------------------------------------------------------------------
# The real data: NW / 2025 / wk26 / Pipiens, exactly as it sits in
# 1_input/2025/w37/wnv-s_database.csv. One positive pool, and it has total == 1.
nw_wk26 <- data.frame(
  csu_id = c(
    "CSU23463",
    "CSU23465",
    "CSU23467",
    "CSU23472",
    "CSU23473",
    "CSU23475",
    "CSU23480"
  ),
  total = c(1, 1, 3, 6, 1, 1, 1),
  test_code = c(0, 0, 0, 0, 0, 0, 1),
  grp = c(rep("A", 6), "B")
)
try_pir("CASE 1: real NW wk26 Pipiens, ALL pools kept", nw_wk26)
try_pir(
  "CASE 2: same group, current filter total > 1",
  subset(nw_wk26, total > 1)
)

# CASE 3/4 ---------------------------------------------------------------------
# Degenerate group: the ONLY pool is a single mosquito. This is the case the
# lines 73-93 fallback was written for.
try_pir(
  "CASE 3: single pool, total = 1, POSITIVE",
  data.frame(total = 1, test_code = 1, grp = "A")
)
try_pir(
  "CASE 4: single pool, total = 1, NEGATIVE",
  data.frame(total = 1, test_code = 0, grp = "B")
)

# CASE 5 -----------------------------------------------------------------------
# Imputed rows: total == 0 and test_code == NA. These are the rows the filter
# was genuinely meant to remove (5,689 of them in the current database).
try_pir(
  "CASE 5: imputed row, total = 0, test_code = NA",
  data.frame(total = c(0, 3), test_code = c(NA, 0), grp = "C")
)

# CASE 6 -----------------------------------------------------------------------
# Empty data frame. HYPOTHESIS: this -- not total == 1 -- is what threw the
# original error. It is what `filter(total > 1)` produces when every pool in
# the data happens to be a single mosquito.
try_pir("CASE 6: EMPTY data frame (what total > 1 leaves behind)", nw_wk26[0, ])

# CASE 7 -----------------------------------------------------------------------
# Many groups at once, mixed pool sizes, including a group that is entirely
# size-1 pools -- closest to how calc_pir() actually calls pIR().
mixed <- data.frame(
  total = c(1, 3, 6, 1, 1, 1, 25, 50, 1),
  test_code = c(0, 0, 0, 1, 0, 1, 0, 1, 0),
  grp = c(rep("NW_Pipiens", 3), rep("SW_Pipiens", 3), rep("SE_Tarsalis", 3))
)
try_pir("CASE 7: multiple groups, mixed pool sizes", mixed)

cat("\n==============================================\n")
cat("Interpretation:\n")
cat("  If CASES 1,3,4,7 all return estimates and only CASE 6 errors,\n")
cat("  then pIR() handles total == 1 fine and the `total > 1` filter is\n")
cat("  removing real positive pools for no reason.\n")
cat("==============================================\n")

