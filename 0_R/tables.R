

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#TABLE 1A
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

t1a = function(df, col, prefix) {
  df %>%
    select(zone, spp, {{col}}) %>%
    pivot_wider(names_from = spp, 
                values_from = {{col}},
                names_prefix = prefix)
}

t1a_abund = t1a(current_wk, abund, "abund_")
t1a_pir = t1a(current_wk, pir, "pir_")
t1a_vi = t1a(current_wk, vi, "vi_")
  
t1a = t1a_abund %>%
  left_join(t1a_pir, by = "zone") %>%
  left_join(t1a_vi, by = "zone") 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>TABLE 2A
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#total indiv examined
#number pools examined
t2a_collected = current_wk %>%
  select(zone, spp, mosq_L) %>% 
  pivot_wider(names_from = "spp", 
              values_from = "mosq_L",
              names_prefix = "collected_",
              values_fill = 0)


t2a_traps = current_wk %>%
  distinct(zone, trap_L)

if(nrow(get_dupes(t2a_traps)) > 0) { #if there are duplicates it means the number of traps for pip and tar don't match
  print("the number of traps for pipiens and tarsalis don't match")
}
  


t2a_abund = t1a_abund

t2a = t2a_collected %>%
  left_join(t2a_traps, by = "zone") %>%
  left_join(t2a_abund, by = "zone") #%>%
 # mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>TABLE 3A
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

t3a_examined = current_wk %>%
  select(zone, spp, mosq_L) %>% 
  pivot_wider(names_from = "spp", 
              values_from = "mosq_L",
              names_prefix = "examined_",
              values_fill = 0)

#number pools examined
t3a_pools = pools %>%
  select(zone, spp, n_pools) %>%
  pivot_wider(names_from = "spp", 
              values_from = "n_pools",
              names_prefix = "pool_",
              values_fill = 0)

t3a_p_pools = pools %>%
  select(zone, spp, n_pos_pools) %>%
  pivot_wider(names_from = "spp", 
              values_from = "n_pos_pools",
              names_prefix = "pos_pool_",
              values_fill = 0)

t3a_pir = t1a_pir %>%
  mutate(across(-zone, ~ round(. * 1000,2)))

t3a = t3a_examined %>%
  left_join(t3a_pools, by = "zone") %>%
  left_join(t3a_p_pools, by = "zone") %>%
  left_join(t3a_pir, by = "zone") # %>%
 # mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

rm(t3a_examined, t3a_pools, t3a_pir)

write.csv(t1a, file.path(dir_output, "table1a.csv"), row.names = F)
write.csv(t2a, file.path(dir_output, "table2a.csv"), row.names = F)
write.csv(t3a, file.path(dir_output, "table3a.csv"), row.names = F)
