source("scripts/config.R")


data_input = check_read_fun(data_output_fn) %>%
  mutate(year = as.factor(year),
         week = as.factor(week)) %>%
  left_join(year_cols_df, by = "year")

if(fc_zone_filter == "YES") {
  data_input = data_input %>%
    filter(zone %in% fc_zones) %>%
    mutate(zone = factor(zone, levels = c("NW", "NE", "SW", "SE")))
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> ABUNDANCE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(length(year_filter) > 1) {
  title = paste0("Average mosquitoes per trap per week ", year_fn)
  #boxplot mosquitoes per trap per week 2000-2019
  ggplot(data_input, aes(week, abund, fill = spp, color = spp)) +
    geom_boxplot(alpha = 0.5) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette) +
    ggtitle(title) +
    theme_classic() + 
    facet_wrap(~zone)
  
  data_input %>%
    group_by(year, week, zone, col) %>%
    summarise(abund = mean(abund)) %>%
    ungroup()%>%
    ggplot(aes(week, abund, color = col, group = year)) +
    geom_line() + 
    ggtitle(title) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette) +
    theme_classic() +
    facet_wrap(~zone)
} else{
  title = paste0("Mosquitoes per trap per week ", year_fn, "-", week_fn)
  ggplot(data_input, aes(week, abund, fill = spp, color = spp)) +
    geom_col(position = "dodge", alpha = 0.5) + 
    scale_color_manual(year_cols) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette) +
    ggtitle(title) +
    theme_classic() +
    facet_wrap(~zone)

}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> VI
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(length(year_filter) > 1) {
  title = paste0("Vector Index per trap per week ", year_fn)
  #boxplot mosquitoes per trap per week 2000-2019
  ggplot(data_input, aes(week, vi, fill = spp, color = spp)) +
    geom_boxplot(alpha = 0.5) +
    ggtitle(title) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette) +
    theme_classic() + 
    facet_wrap(~zone)
  
  data_input %>%
    group_by(year, week, zone, col) %>%
    summarise(vi = mean(vi)) %>%
    ungroup()%>%
    ggplot(aes(week, vi, color = col, group = year)) +
    geom_line(size = 0.8) + 
    ggtitle(title) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette) +
    theme_classic() +
    facet_wrap(~zone)
  
} else{
  title = paste0("Vector Index trap per week ", year_fn, "-", week_fn)
  ggplot(data_input, aes(week, vi, fill = spp, color = spp)) +
    geom_col(position = "dodge", alpha = 0.5) + 
    ggtitle(title) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette) +
    theme_classic() +
    facet_wrap(~zone)
  
}


