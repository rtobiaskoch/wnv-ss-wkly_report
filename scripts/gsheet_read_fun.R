

gsheet_pull = function(key, sheet, out_fn) {
  
  require(googlesheets4)
  library(googlesheets4)
  
  mdata = read_sheet(key, sheet = sheet)
  
  #authorize token response
 # 1
  
  #mdata = apply(mdata, 2, as.character) #added because error   unimplemented type 'list' in 'EncodeElement' was occuring
  write.csv(mdata, out_fn, row.names = F, na = "")
  
}   