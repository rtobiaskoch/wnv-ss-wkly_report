source("scripts/config.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#QS data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



qs_input = read_xls(fn_qs, sheet = "Results")

qs = qs_input[34:nrow(qs_input),]
colnames(qs) = qs[1,]
qs = qs[-1,]


qs = clean_names(qs) %>%
  filter(!is.na(target_name)) %>% 
  mutate(ct = if_else(str_detect(ct, "Undetermined"), "55.55", ct)) %>% # convert to numeric to avoid errors
  mutate(cq = round(as.numeric(ct), 2),
         ct_threshold = as.numeric(ct_threshold)) %>%
  select(well_position, target_name, cq, `ct_threshold`) %>%
  pivot_wider(names_from = target_name, values_from = cq) %>%
  rename(cq = WNV,
         SLEV_cq = SLEV)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Platemap
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

platemap = read_xlsx(fn_platemap)
platemap = platemap[1:8,1:12] %>%
  rename(row = "...1") %>%
  pivot_longer(cols = -row, 
               names_to = "column", 
               values_to = "csu_id") %>%
  mutate(well_position = paste0(row, column)) %>%
  select(well_position, csu_id)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#merge
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cq_data = left_join(qs, platemap, by = "well_position")


if(any(cq_data$ct_threshold < rn_threshold)) {
  stop(paste0("Ct baseline thresholds are RN below ", rn_threshold, "check Quantstudio (QS) output file."))
} else {
  
  write.csv(cq_data, fn_cq_out, row.names = F)
}




