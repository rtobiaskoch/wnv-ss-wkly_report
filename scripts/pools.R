source("scripts/config.R")

#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = check_read_fun(fn_database_update)


suppressMessages({
  
  #calculate number of pools  
  n_pools_zone = data_input %>% 
    group_by(across(all_of(c(grp_vars, "method")))) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pools_", values_fill = 0) %>%
    mutate(n_pools = pools_L + pools_G)
  
  
  fc_n_pools = data_input %>% 
    group_by(year, week, spp, method) %>%
    summarise(zone = "FC",
              n = n()) %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pools_", values_fill = 0) %>%
    mutate(n_pools = pools_L + pools_G)
  
  
  n_pools =  rbind(n_pools_zone, fc_n_pools)
  
  #calculate the number of positive pools
  pos_pools_zone =  data_input %>% 
    group_by(across(all_of(c(grp_vars, "method")))) %>%
    summarise(n = sum(test_code)) %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pos_pools_", values_fill = 0) %>%
    mutate(n_pos_pools = pos_pools_L + pos_pools_G)
  
  fc_pos_pools = data_input %>% 
    group_by(year, week, spp, method) %>%
    summarise(zone = "FC",
              n = sum(test_code)) %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pos_pools_", values_fill = 0) %>%
    mutate(n_pos_pools = pos_pools_L + pos_pools_G)
  
  pos_pools =  rbind(pos_pools_zone, fc_pos_pools)
  
  pools = left_join(n_pools, pos_pools, by = grp_vars)
  
}
  
 
  
)



write.csv(pools, fn_pools_mid, row.names = F)
