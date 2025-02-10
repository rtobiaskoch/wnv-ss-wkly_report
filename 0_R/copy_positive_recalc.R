# What this code does:
#1 recalculate the copy number and determine positives from previous runs 
# if the standards failed in the specific run 

list2env(readRDS(config_params_file),           envir = .GlobalEnv)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>-------------------------------- R E A D   &   C L E A N -----------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#pull database from googledrive location and read it as a csv
#this will be used to calculate the new copy number.

gsheet_pull(key_database_gsheet, "data", fn_database_input)

database = read.csv(fn_database_input)


#read in previous weeks standards
gsheet_pull(key = key_standards_gsheet, 
            sheet = 'data',
            out_fn = "data_input/standards_input.csv")

std_database = read.csv("data_input/standards_input.csv")   

#read in this weeks pcr data

cq_data = read.csv(fn_cq_out)

