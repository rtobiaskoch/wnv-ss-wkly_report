source("0_R/config")

data_input0 = read.csv("data_output/hx_data.csv")

data_input = data_input0 %>%
  mutate(type = if_else(year ==year_filter, "current", "hx"))
  


p_df_all_fun2 = function(df, value, text) {
  
  ggplot(df, aes(x = week, y = {{value}}, group = interaction(type2, spp))) +
    # Separate geom_area for stacking
    geom_line(data = df %>% filter(type == "hx"), aes(fill = type2, color = type2), position = "stack", alpha = 0.5) +
    geom_line(data = df %>% filter(type == "current"), aes(fill = type2, color = type2), position = "stack", alpha = 0.5) +
    geom_hline(yintercept = 0) +
    # Separate geom_area for dodging
    #  geom_area(data = df %>% filter(type %in% c("hx", "current")), aes(fill = type2), position = "dodge", alpha = 0.5) +
    facet_grid(zone ~ .) +
    theme_classic() +
    ggtitle(text) +
    scale_color_manual(values = pal_mozzy) +
    scale_fill_manual(values = pal_mozzy)
