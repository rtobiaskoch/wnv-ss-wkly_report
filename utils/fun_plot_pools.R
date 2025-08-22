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


#claude
plot_p_pool_pt = function(df) {
  df = df %>%
    mutate(
      test_code = as.factor(test_code),
      zone = factor(zone, levels = zone_lvls)
    ) %>%
    group_by(year, week, zone, trap_id) %>%
    arrange(year, week, zone, desc(test_code)) %>%
    mutate(n = row_number()) %>%
    ungroup() 
  
  # Create a factor for trap_id within each zone to control y-axis
  df = df %>%
    group_by(zone) %>%
    mutate(
      trap_id_factor = factor(trap_id, levels = unique(trap_id))
    ) %>%
    ungroup()
  
  p = df %>%
    ggplot(aes(x = trap_id_factor, y = n, size = total, color = test_code, shape = spp)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c("1" = "red", "0" = "blue"), 
                       name = "Test Result",
                       labels = c("0" = "Negative", "1" = "Positive")) +
    scale_shape_manual(values = c("Tarsalis" = 16, "Pipiens" = 1), 
                       name = "Species") +
    facet_wrap(~ zone, scales = "free_x", ncol = 4) +  # Free y-axis scales
    theme_classic() +
    theme(
      strip.background = element_rect(fill = "lightblue", color = "black"),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 8),  # Smaller text for trap IDs
      axis.text.x = element_text(size = 8, angle = 90),  # Smaller text for trap IDs
      panel.grid.major.y = element_line(color = "gray90", linetype = "dashed")
    ) +
    labs(
      title = "Mosquito Pool qPCR Test Results by Zone",
      x = "Trap ID",
      y = "Pools",
      size = "Total Mosquitos"
    )
  
  return(p)
}
