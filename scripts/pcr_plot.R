source("scripts/config.R")

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>-------------------------------- R E A D   &   C L E A N -----------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

data_input0 = read.csv(fn_cq_out) 

data_input = data_input0 %>%
  mutate(sample_type = case_when(grepl("^CSU|^BOU|^CDC", csu_id, ignore.case = T) ~ "mozzy",
                             grepl("neg|negative", csu_id, ignore.case = T) ~ "neg ctrl",
                             grepl("pos|positive", csu_id, ignore.case = T) ~ "pos ctrl",
                             grepl("1e2", csu_id, ignore.case = T) ~ "std 1e2",
                             grepl("1e4", csu_id, ignore.case = T) ~ "std 1e4",
                             grepl("1e6", csu_id, ignore.case = T) ~ "std 1e6",
                             grepl("RMRP", csu_id, ignore.case = T) ~ "bird",
                             T ~ "undefined" 
                           )
         )


#read in previous weeks standards
gsheet_pull(key = standards_key, 
            sheet = 'data',
            out_fn = "data_input/standards_input.csv")

std_database = read.csv("data_input/standards_input.csv")   

if(nrow(data_input %>% filter(sample_type == "undefined"))) {
  stop("you have undefined sample types in your data")
} else {

 
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>----------------------------------- S T A N D A R D S --------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   

  
  
std_new0 = data_input %>%
  filter(str_detect(pattern = "std|pos ctrl", sample_type))

std_new = std_new0 %>%
  select(-ct_threshold, -well_position) %>%
  rename(cq_WNV = cq) %>%
  pivot_longer(cols = -c(plate, csu_id, sample_type),
               names_to = "type",
               values_to = "value") %>%
  mutate(target = if_else(str_detect(type, "WNV"), "WNV", "SLEV")) %>%
  mutate(type = str_extract(type, "^[^_]*")) %>%
  pivot_wider(names_from = type, 
              values_from = value) %>%
  mutate(year = year_filter, 
         week = week_filter,
         log_copies = if_else(copies == 0, 0, log10(copies))) %>%
  select(year, week, plate, target, csu_id, sample_type, cq, log_copies) 
  

std_update = natural_join(std_new, std_database, jointype = "FULL", 
                          by = c("year", "week", "plate", "target", "csu_id", "sample_type"))

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>-----------------------------S A V E   N E W   S T A N D A R D S --------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


write.csv(std_update, "data_output/standards_new.csv")

#make a copy of old standards key 
drive_cp(file = as_id(standards_key), 
         path = "standards_and_controls",
         name = "standards_archive",
         overwrite = T)

#save it to gdrive
googlesheets4::sheet_write(std_update,
                           ss = standards_key,
                           sheet = "data"
)


# std_wnv = std %>%
#   filter(str_detect(pattern = "wnv", csu_id)) 
# 
# std_slev = std %>%
#   filter(str_detect(pattern = "slev", csu_id)) 

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>-----------------------------P L O T T I N G    S T A N D A R D S --------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

p_std_df = std_update %>% 
  mutate(#week = as.factor(week),
         sample_type = if_else(str_detect(sample_type, "std"), "std", "pos_ctrl")) %>% 
  mutate(grp = paste(year, week, plate, target, sample_type, sep = "-")) %>%
  filter(str_to_lower(target) == str_extract(csu_id, "^[^_]*"))
  

p_std = p_std_df %>%
  ggplot(aes(x = log_copies, y = cq, color = week, group = grp)) +
  geom_point(alpha = 0.4, size = 3) +
  geom_line() + 
  facet_wrap(~target) +
  theme_classic()

plotly::ggplotly(p_std)


p_std2 = p_std_df %>%
  filter(week %in% 24:40) %>%
  filter(week != 33) %>%
  mutate(cq = if_else(cq == 55.55, 40, cq)) %>%
  ggplot(aes(x = log_copies, y = cq, color = week, group = grp)) +
  geom_point(alpha = 0.4, size = 3) +
  geom_line() + 
  scale_y_reverse() +
  facet_wrap(~target) +
  ggtitle("Standards by Week") +
  theme_classic()

p_std2
plotly::ggplotly(p_std2)



#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------------P L O T T I N G   S A M P L E S -----------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 wnv =  data_input %>% 
    filter(!str_detect(pattern = "slev", csu_id)) %>%
    mutate(cq = if_else(cq == 55.55, 40, cq),
           test_code = if_else(copies_WNV > copy_threshold, "1", "0")) %>%
   mutate(test_code = factor(test_code, levels = c("1", "0")))
  
  p_pcr_wnv = ggplot(wnv, aes(x = sample_type, y = cq, 
                                 color = test_code)) +
    geom_jitter(size = 3, alpha = 0.6) +
    scale_y_reverse() +
   # geom_hline(yintercept = log10(copy_threshold), linetype = "dashed", color = "red") +
    theme_minimal() +
    ggtitle(paste0("Week ", week_filter, " WNV")) +
    theme(legend.position = "none")
  
  p_pcr_wnv
  
  
  slev =  data_input %>% 
    filter(!str_detect(pattern = "wnv", csu_id)) %>%
    mutate(cq_SLEV = if_else(cq_SLEV == 55.55, 40, cq_SLEV),
           test_code = if_else(copies_SLEV > copy_threshold, "1", "0")) %>%
    mutate(test_code = factor(test_code, levels = c("1", "0")))
  
  p_pcr_slev = ggplot(slev, aes(x = sample_type, y = cq_SLEV, 
                              color = test_code)) +
    geom_jitter(size = 3, alpha = 0.6) +
    scale_y_reverse() +
    # geom_hline(yintercept = log10(copy_threshold), linetype = "dashed", color = "red") +
    theme_minimal() +
    ggtitle(paste0("Week ", week_filter, " SLEV")) +
    theme(legend.position = "none")
  
  p_pcr_slev

}
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>-----------------------------C O M B I N E    P L O T S ------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


pcr_plot = p_std2 / p_pcr_wnv /  p_pcr_slev
pcr_plot
ggsave("data_output/plots/pcr_plot.png", pcr_plot,
       width = 8, height = 8, units = "in")


