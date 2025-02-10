#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#LOAD PACKAGES FOR PIPELINE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())
#IF THE CONFIG PARAMS FILE EXISTS US THAT TO DEFINE INPUTS 
# ELSE DEFINE THEM HERE
config_params_file = "0_input/config_params.RDS" # <<<<<<<<<<----------------------------------  USER INPUT

if(file.exists(config_params_file)){
  list2env(readRDS(config_params_file), 
           envir = .GlobalEnv)
} else {
  
  
  #LOAD PACKAGES
  suppressMessages({
    if (!require("pacman")) install.packages("pacman")
    pacman::p_unload()
    pacman::p_load(tidyverse, purrr #manipulation
    )
  })
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #--------------------------- I N P U T S --------------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #INPUTS FOR THIS SCRIPT IF CONFIG FILE DOESN'T EXIST
  
  #SUBDIRECTORIES INPUTS
  dir_pcr = "1_input/pcr/" # <<<<<<<<<<-------------------------------------------------------------  USER INPUT
  dir_platemap= "1_input/platemap/"
  
  #TIME FILTERS
  week_filter = 37
  year_filter = 2024
  
  #FILENAMES OUTPUT
  fn_cq_out = paste0("2_mid","/y",year_filter, "_", "w",week_filter, "_platemap.csv")
  
  #USER DEFINED STATIC DATA PARAMETERS:
  copy_threshold = 500
  rn_threshold = 34000

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
} # end of if config_params_file exist statement


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#pcr data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get list of file paths for all files in the pcr output from quantstudio
fn_path = list.files(path = dir_pcr, #
               full.names = T,
               ignore.case = T)  

pcr_input = fn_path %>% 
  map(~ {
        data = read_excel(.x, col_names = TRUE, sheet = "Results") %>% #iteratively read through all file names in path
          filter(`Block Type` %in% c("Well", as.character(1:96))) #keep only the 96 wells as the output as superfluous BS
          
        colnames(data) = data[1,] %>% as.character() #replace the nonsensical colnameswith the actual colnames that are location in the first row
        data %>% 
          filter(`Well Position` != "Well Position") %>% #removes the colnames from the first row
         mutate(file_name = .x) #add a column that is the file name where the observation came from
        }) %>%
  bind_rows() #put everything together


pcr = pcr_input %>%
  clean_names() %>%
  mutate(
    year = str_extract(file_name, "(?<=y)\\d+"),# Extract year as the part after "y" and before "_w"
    week = str_extract(file_name, "(?<=w)\\d+"), # Extract week as the part after "_w" and before "_p"
    plate = str_extract(file_name, "(?<=p)\\d+"),     # Extract plate as the part after "_p" (end of the string)
    plate = paste0("plate_", plate)
        ) %>%
  mutate(ct = if_else(str_detect(ct, "Undetermined"), '55.55', ct)) %>% # convert "undetermined to numeric 55.55 to avoid errors
  mutate(cq = round(as.numeric(ct), 2)) %>%
  mutate(well = as.numeric(well), #convert to numeric to sort
         quantity = replace_na(quantity, "0"),
         copies = if_else(cq >= 55.55, 0, round(as.numeric(quantity),2))
         #ct_threshold = as.numeric(ct_threshold)
         ) %>%
  arrange(plate, well) %>%
 # select(well_position, task, target_name, cq, ct_threshold, copies, plate) %>%
  select(well_position, target_name, cq, ct_threshold, copies, year, week , plate) %>%
  pivot_wider(names_from = target_name, 
              values_from = c(copies,cq)) %>%
  rename(cq = cq_WNV) # rename to match database and rest of code

 #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Platemap
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get the list of platemaps from your 
fn_path = list.files(path = dir_platemap,
                     full.names = T,
                     ignore.case = T)  

platemap = fn_path %>%
  map(~read_excel(.x, col_names = T, 
                      col_types = "text", 
                      range = "A1:M9",
                      sheet = "pcr") %>%
  rename(row = "...1") %>%
  pivot_longer(cols = -row, 
               names_to = "column", 
               values_to = "csu_id") %>%
  mutate(file_name = .x) %>%
  mutate(
      year = str_extract(file_name, "(?<=y)\\d+"),# Extract year as the part after "y" and before "_w"
      week = str_extract(file_name, "(?<=w)\\d+"), # Extract week as the part after "_w" and before "_p"
      plate = str_extract(file_name, "(?<=p)\\d+"),     # Extract plate as the part after "_p" (end of the string)
      plate = paste0("plate_", plate)
    ) %>%
  mutate(well_position = paste0(row, column)) %>%
  mutate(well_position = str_remove(well_position, "\\.0")) %>% #remove the column ##.0 that is getting imported
  select(well_position, csu_id, year, week, plate)
     ) %>%
  bind_rows()

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#merge
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cq_data = left_join(pcr, platemap,
                    by = c("well_position", "year", "week", "plate"))  %>%
  filter(!is.na(csu_id)) %>% # remove wells with no matches
  arrange(year, week, plate)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K   S T D S  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# stds = cq_data %>%
#   filter(task == "STANDARD"|grepl("std", csu_id, ignore.case = T))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K   C O N T R O L S  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pos = cq_data %>% 
  filter(grepl("pos", csu_id, ignore.case = T))

if(any(pos$copies_WNV < copy_threshold|pos$copies_SLEV < copy_threshold)) {
  warning(paste0("one of your positive extraction controls have < ",copy_threshold, " copies"))
}


neg = cq_data %>% 
  filter(grepl("neg", csu_id, ignore.case = T))

if(any(neg$copies_WNV > copy_threshold|neg$copies_SLEV > copy_threshold)) {
  warning(paste0("one of your negative extraction controls have > ",copy_threshold, " copies"))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K   S A M P L E   I D S  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#add check for sample id format

# if(any(!str_detect(data_input$csu_id, "-"))) {
#   stop("you have samples id without a -")
# }


temp = cq_data %>%
  filter(grepl("^CSU|^BOU|^CDC", csu_id, ignore.case = T))
  
if(nrow(get_dupes(temp, csu_id)) >0) {
  stop("Alert! You have duplicate id's in your pcr plate run. Double check your plates to ensure this was intentional. ")
} else(
  print("no duplicate sample ids that start with CSU|BOU|CDC")
)


if(any(cq_data$ct_threshold < rn_threshold)) {
  stop(paste0("Ct baseline thresholds are RN below ", rn_threshold, "check Quantstudio (pcr) output file."))
} else {
  
  write.csv(cq_data, fn_cq_out, row.names = F)
}




