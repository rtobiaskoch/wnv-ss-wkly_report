
#ABUNDANCE
#total mosquito's per trap per night

if(!exists("data_input")){
  print("your data didn't read in correctly please check your config.R and read_data.R scripts")
}

data_input = data_input


#get number of traps per night
trap_p_night = data_abun %>%
  distinct(trap_date, week, zone, trap_id) %>% #get unique number of traps by removing traps listed 2+ with multiple pools
  group_by(trap_date, zone, week) %>% #get number of traps per night per zone
  summarise(n_trap = n()) %>%
  ungroup

#get number of mosquitoes per night
m_p_night = data_abun %>%
  group_by(trap_date, week, zone, spp) %>% #get number of mosquitoes per week per zone per species
  summarize(mosq = sum(total)) %>%
  ungroup()# %>%

#get abundance per trap
abund = left_join(trap_p_night, m_p_night, by = c("trap_date", "zone", "week")) %>%
  mutate(abund = round(mosq/n_trap,1))

#get abundance per week per zone
abund_zone_wk = abund %>%
  group_by(week, zone, spp) %>%
  summarise(abund = round(mean(abund),1)) %>%
  ungroup()


write.csv(abund_zone_wk, paste0(abund_out_fn, ".csv"), row.names = F)
write_rds(abund_zone_wk, paste0(abund_out_fn, ".RData"))


if(file.exists(paste0(abund_out_fn, ".csv"))){
  rm(trap_p_night, m_p_night, unmatched_traps, data_input, 
     data_abun)
}
  

