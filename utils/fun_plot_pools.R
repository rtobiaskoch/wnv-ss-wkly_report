plot_pools = function(df) {
 df %>%
  mutate(n_pools= n_pools - n_pos_pools) %>%
  pivot_longer(cols = c(n_pools, n_pos_pools)) %>%
  filter(zone != "FC") %>%
  mutate(name = factor(name, 
                       levels = c( "n_pos_pools","n_pools"),
                       labels = c("Positive Pools","Total Pools"))) %>%
  ggplot(aes(x = zone, y = value, fill = name, color = name)) +
  geom_col() +  # Add dodging for side-by-side bars
  theme_classic() +
  labs(fill = "Pool Status", color = "Pool Status")  # Better legend title
}


plot_p_pool_pt = function(df) {
  df = df %>%
    mutate(
      test_code = as.factor(test_code),
      zone = factor(zone, levels = zone_lvls)
    ) %>%
    group_by(year, week, zone) %>%
    arrange(year, week, zone, desc(test_code)) %>%
    mutate(n = row_number()) %>%
    ungroup() 
  
  p = df %>%
    ggplot(aes(x = n, y = zone, size = total, color = test_code, shape = spp)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("1" = "red", "0" = "blue")) +
    scale_y_discrete(limits = rev) +  # Reverse y-axis (BC at bottom)
    scale_shape_manual(values = c("Tarsalis" = 16, "Pipiens" = 1)) +  # 16=circle, 17=triangle
    theme_classic() +
    ggtitle("Mosquito Pool qPCR Test Results")
  return(p)
}