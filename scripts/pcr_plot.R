source("scripts/config.R")

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

if(nrow(data_input %>% filter(sample_type == "undefined"))) {
  stop("you have undefined sample types in your data")
} else {

std = data_input %>%
  filter(str_detect(pattern = "std", sample_type))

std_wnv = std %>%
  filter(str_detect(pattern = "wnv", csu_id)) 

std_slev = std %>%
  filter(str_detect(pattern = "slev", csu_id)) 

p_std = ggplot() +
  geom_point(data = std_wnv, aes(x = log10(copies_WNV), y = cq), 
             color = "#F8766D", size = 3, alpha = 0.6) + #wnv
  geom_line(data = std_wnv, aes(x = log10(copies_WNV), y = cq, group = plate), 
            color = "#F8766D") + #wnv
  geom_point(data = std_slev, aes(x = log10(copies_SLEV), y = cq_SLEV), 
             color = "#00BFC4", size = 3, alpha = 0.6) + #slev
  geom_line(data = std_slev, aes(x = log10(copies_SLEV), y = cq_SLEV, group = plate),
            color = "#00BFC4") + 
  scale_y_reverse() +
  xlab("log10(copies)")+
  ggtitle("Standards") +
  theme_classic()
p_std
  
 wnv =  data_input %>% 
    filter(!str_detect(pattern = "slev", csu_id)) %>%
    filter(cq < 55.55 )
  
  p_pcr_wnv = ggplot(wnv, aes(x = sample_type, y = cq, 
                                 color = sample_type, fill = sample_type)) +
    geom_jitter(size = 3, alpha = 0.6) +
    scale_y_reverse() +
   # geom_hline(yintercept = log10(copy_threshold), linetype = "dashed", color = "red") +
    theme_minimal() +
    ggtitle("WNV") +
    theme(legend.position = "none")
  
  p_pcr_wnv
  
  
  slev =  data_input %>% 
    filter(!str_detect(pattern = "wnv", csu_id)) %>%
    filter(cq_SLEV < 55.55)
  
  p_pcr_slev = ggplot(slev, aes(x = sample_type, y = cq_SLEV, 
                              color = sample_type, fill = sample_type)) +
    geom_jitter(size = 3, alpha = 0.6) +
    scale_y_reverse() +
    # geom_hline(yintercept = log10(copy_threshold), linetype = "dashed", color = "red") +
    theme_minimal() +
    ggtitle("slev") +
    theme(legend.position = "none")
  
  p_pcr_slev

}

p_std / p_pcr_wnv /  p_pcr_slev
