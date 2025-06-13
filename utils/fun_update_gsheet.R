update_gsheet = function(new, #new data to add
                         old = NULL, #old data that you want to update
                         type = "FULL", 
                         by, #joining keys
                         fn_save, #filename that you want to save locally
                         gkey,  #google sheet key of file you want to update
                         gfolder, #folder name in google drive save copy
                         gname, #name of file to cp before updating
                         sheet = "data"
                         ) {

#if old isn't supplied load from google
if(is.null(old)) {
  old = googlesheets4::read_sheet(ss = gkey, 
                            sheet = sheet)  
}

  
#update data 
update = natural_join(new, old, jointype = type, 
                            by = by)

#save local copy
write.csv(update, fn_save)
  
  #make a copy of old standards key 
  drive_cp(file = as_id(gkey), 
           path = gfolder,
           name = paste0(gname, Sys.time()),
           overwrite = T)
  
  #save it to gdrive
  googlesheets4::sheet_write(update,
                             ss = gkey
  )
}

