source("scripts/config.R")

#ABUNDANCE
#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = check_read_fun(all_data_fn)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

suppressMessages({
# Your code with group_by and summarize
#get number of traps per night
trap_p_wk = data_input %>%
  filter(method != "G") %>% #remove gravid traps for abundance calculation
  distinct(year,trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools
  group_by(year, zone, week) %>% #get number of traps per week per zone
  summarise(n_trap = n()) %>%
  ungroup()

#get number of mosquitoes per night per trap
m_p_wk = data_input%>%
  filter(method != "G") %>% #remove gravid traps for abundance calculation
  group_by(year, week, zone, spp) %>% #get number of mosquitoes per week per zone per species
  summarize(mosq = sum(total)) %>%
  ungroup()# %>%

#get abundance per trap
abund_zone_wk = left_join(trap_p_wk, m_p_wk, by = c("year","zone", "week")) %>%
  mutate(abund = round(mosq/n_trap,1))
})

write.csv(abund_zone_wk, paste0(abund_out_fn, ".csv"), row.names = F)


if(file.exists(paste0(abund_out_fn, ".csv"))){
  rm(list = ls())
}
  

