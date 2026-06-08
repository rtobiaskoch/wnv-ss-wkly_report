update_gsheet = function(new, #new data to add
                         old = NULL, #old data that you want to update
                         type = "FULL",
                         by, #joining keys
                         fn_save, #filename that you want to save locally
                         gkey,  #google sheet key of file you want to update
                         gfolder, #folder name in google drive save copy
                         gname, #name of file to cp before updating
                         sheet = "data",
                         col_database = c( "csu_id", "trap_id", "zone", "year", "week", "trap_date","method", "spp",
                                           "total", "test_code", "cq", "copies_WNV", "seq", "lat", "long")
                         ) {

  # Safety guard: error immediately if the `update` flag in the calling scope is
  # not TRUE. Prevents silent GSheet writes if an if(update) block is removed.
  update_allowed <- tryCatch(
    get("update", envir = parent.frame(), inherits = TRUE),
    error = function(e) FALSE
  )
  if (!isTRUE(update_allowed)) {
    stop(
      "update_gsheet() was called but `update` is FALSE or not set in the calling scope. ",
      "All calls to update_gsheet() must be inside an `if(update)` block."
    )
  }

  # Ensure googlesheets4 has a WRITE-capable OAuth token. In a non-interactive
  # render gs4 can silently fall back to API-key mode (read-only), which 401s on
  # sheet_write ("CREDENTIALS_MISSING"). googledrive is already authenticated by
  # this point (drive_cp below relies on it), and the full `…/auth/drive` scope
  # is accepted by the Sheets API, so reuse drive's token for gs4.
  if (!googlesheets4::gs4_has_token()) {
    googlesheets4::gs4_auth(token = googledrive::drive_token())
  }

#if old isn't supplied load from google
if(is.null(old)) {
  old = googlesheets4::read_sheet(ss = gkey, 
                            sheet = sheet)  
}
  
#update data — merge logic lives in merge_trap_database() (utils/fun_merge_trap_database.R)
#so it can be unit-tested apart from the GSheet write path below.
update = merge_trap_database(new = new,
                             old = old,
                             by = by,
                             col_database = col_database,
                             jointype = type)

#save local copy
write.csv(update, fn_save, row.names = F)
  
  #make a copy of old standards key 
  drive_cp(file = as_id(gkey), 
           path = gfolder,
           name = paste0(gname, Sys.time()),
           overwrite = T)
  
  #save it to gdrive
  options(googlesheets4.na = "")  # Write NA as blank cells
  googlesheets4::sheet_write(update,
                             ss = gkey,
                             sheet = sheet
  )
}

