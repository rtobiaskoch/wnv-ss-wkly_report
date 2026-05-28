# Tests for the update_gsheet() write guard.
#
# update_gsheet() must error immediately when called outside an if(update)
# block (i.e., when `update` is FALSE or absent in the calling scope).
# This prevents accidental writes to production Google Sheets.
#
# Note: these tests exercise ONLY the guard, not the GSheet write path.
# No OAuth or network access is required.

source(here::here("utils", "fun_update_gsheet.R"))

test_that("update_gsheet() errors when update is FALSE in calling scope", {
  update <- FALSE
  expect_error(
    update_gsheet(
      new      = tibble::tibble(),
      by       = "id",
      fn_save  = tempfile(),
      gkey     = "fake_key",
      gfolder  = "fake_folder",
      gname    = "fake_name"
    ),
    regexp = "update.*FALSE"
  )
})

test_that("update_gsheet() errors when update is not set in calling scope", {
  # Ensure `update` is absent from the local frame
  rm(list = intersect(ls(), "update"))
  expect_error(
    update_gsheet(
      new      = tibble::tibble(),
      by       = "id",
      fn_save  = tempfile(),
      gkey     = "fake_key",
      gfolder  = "fake_folder",
      gname    = "fake_name"
    ),
    regexp = "update.*FALSE"
  )
})
