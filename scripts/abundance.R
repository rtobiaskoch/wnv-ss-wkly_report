source("scripts/config.R")

#ABUNDANCE
#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = check_read_fun(all_data_fn)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

suppressMessages({
  # Your code with group_by and summarize
#get number of traps per night
trap_p_night = data_input %>%
  distinct(year,trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2+ with multiple pools
  group_by(year,trap_date, zone, week) %>% #get number of traps per night per zone
  summarise(n_trap = n()) %>%
  ungroup()

#get number of mosquitoes per night
m_p_night = data_input%>%
  group_by(year, trap_date, week, zone, spp) %>% #get number of mosquitoes per week per zone per species
  summarize(mosq = sum(total)) %>%
  ungroup()# %>%

#get abundance per trap
abund = left_join(trap_p_night, m_p_night, by = c("year", "trap_date", "zone", "week")) %>%
  mutate(abund = round(mosq/n_trap,1))

#get abundance per week per zone
abund_zone_wk = abund %>%
  group_by(year, week, zone, spp) %>%
  summarise(abund = round(mean(abund),1)) %>%
  ungroup()
})

write.csv(abund_zone_wk, paste0(abund_out_fn, ".csv"), row.names = F)


if(file.exists(paste0(abund_out_fn, ".csv"))){
  rm(list = ls())
}
  

