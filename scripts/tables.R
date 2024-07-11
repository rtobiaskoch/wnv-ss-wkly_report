source('scripts/config.R')

data_input = check_read_fun(fn_database_update)

data_zone_wk = check_read_fun(fn_data_output)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#TABLE 1A
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

t1a_abund = data_zone_wk %>%
  select(zone, spp, abund) %>%
  pivot_wider(names_from = spp, 
              values_from = abund,
              names_prefix = "abund_")

t1a_pir = data_zone_wk %>%
  select(zone, spp, pir) %>%
  pivot_wider(names_from = spp, 
              values_from = pir,
              names_prefix = "pir_")

t1a_vi = data_zone_wk %>%
  select(zone, spp, vi) %>%
  pivot_wider(names_from = spp, 
              values_from = vi,
              names_prefix = "vi_",
              values_fill = 0) %>%
  mutate(all_vi = coalesce(vi_Pipiens, 0) + coalesce(vi_Tarsalis, 0))

t1a = t1a_abund %>%
  left_join(t1a_pir, by = "zone") %>%
  left_join(t1a_vi, by = "zone") 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>TABLE 2A
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#total indiv examined
#number pools examined
t2a_collected = data_zone_wk %>%
  select(zone, spp, mosq_L) %>% 
  pivot_wider(names_from = "spp", 
              values_from = "mosq_L",
              names_prefix = "collected_",
              values_fill = 0) %>%
  mutate(all_collected = coalesce(collected_Pipiens, 0) + coalesce(collected_Tarsalis, 0))


t2a_traps = data_zone_wk %>%
  distinct(zone, trap_L_func)

if(nrow(get_dupes(t2a_traps)) > 0) { #if there are duplicates it means the number of traps for pip and tar don't match
  print("the number of traps for pipiens and tarsalis don't match")
}
  


t2a_abund = t1a_abund %>%
  mutate(all_abund = coalesce(abund_Pipiens, 0) + coalesce(abund_Tarsalis, 0))

t2a = t2a_collected %>%
  left_join(t2a_traps, by = "zone") %>%
  left_join(t2a_abund, by = "zone") #%>%
 # mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>TABLE 3A
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

t3a_examined = data_zone_wk %>%
  select(zone, spp, mosq) %>% 
  pivot_wider(names_from = "spp", 
              values_from = "mosq",
              names_prefix = "examined_",
              values_fill = 0) %>%
  mutate(all_examined = coalesce(examined_Pipiens, 0) + coalesce(examined_Tarsalis, 0))

#number pools examined
t3a_pools = data_zone_wk %>%
  select(zone, spp, n_pools) %>%
  pivot_wider(names_from = "spp", 
              values_from = "n_pools",
              names_prefix = "pool_",
              values_fill = 0) %>%
  mutate(all_pool = coalesce(pool_Pipiens, 0) + coalesce(pool_Tarsalis, 0))

t3a_p_pools = data_zone_wk %>%
  select(zone, spp, n_pos_pools) %>%
  pivot_wider(names_from = "spp", 
              values_from = "n_pos_pools",
              names_prefix = "pos_pool_",
              values_fill = 0) %>%
  mutate(all_pos_pool = coalesce(pos_pool_Pipiens, 0) + coalesce(pos_pool_Tarsalis, 0))

t3a_pir = t1a_pir %>%
  mutate(all_pir = coalesce(pir_Pipiens, 0) + coalesce(pir_Tarsalis, 0)) %>%
  mutate(across(-zone, ~ . * 1000))

t3a = t3a_examined %>%
  left_join(t3a_pools, by = "zone") %>%
  left_join(t3a_p_pools, by = "zone") %>%
  left_join(t3a_pir, by = "zone") # %>%
 # mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

write.csv(t1a, "data_output/table1a.csv", row.names = F)
write.csv(t2a, "data_output/table2a.csv", row.names = F)
write.csv(t3a, 'data_output/table3a.csv', row.names = F)
