source("utils/fun_calc_abund.R")
source("utils/fun_calc_pir.R")
source("utils/fun_calc_vi.R")
source("utils/fun_clean_wnv_s.R")



BC_sim = function(df, culex, keep_zone = c("SE", "SW", "NE", "NW")) {
  
  #set.seed(5)
  
  trap = df %>%
    filter(zone %in% keep_zone) %>%
    distinct(trap_id, zone) %>%
    sample_n(5)
  
  print(trap)
  
  abund = culex %>%
    semi_join(trap, by = "trap_id")
  

  df_sample = df %>% 
    semi_join(trap, by = "trap_id") %>%  # keep only the traps from the list
    group_by(year, week, trap_id) %>%
    slice_max(order_by = total, n = 1, with_ties = FALSE) %>%  # keep max total
    ungroup() %>%
    mutate(zone2 = "FC")
    
  pir = calc_pir(df_sample, zone_complete = trap$zone, rm_zone = c("BC", "BE", "LV"), grp_var = c("year", "week", "zone2")) %>%
    wnv_s_clean(all_cols = names(.), silence = T) %>%
    arrange(year, week, zone, spp)
  
  abund = calc_abund(abund, rm_zone = c("BC", "BE", "LV"), grp_var = c("year", "week", "zone2")) %>%
    wnv_s_clean(all_cols = names(.), silence = T) %>%
    arrange(year, week, zone2, spp)
  #browser()
  
  vi = calc_vi(abund, pir, by = c("year", "week")) %>%
    mutate(type= "downsample")
  
  #browser()
  return(vi)
  
  #left_join(pir, abund, by = c("year", "week", "zone", "spp"))
    
}


database = read.csv("1_input/w32/wnv-s_database.csv")
culex_database = read.csv("1_input/w32/culex_sheet_database.csv")

ds_df = BC_sim(database, culex_database) %>%
  filter(!is.na(vi))

full_df = read.csv("3_output/zone_stats.csv") %>%
  mutate(type = "full") %>%
  filter(zone == "FC" & spp == "All") %>%
  select(-zone2)

bc_comp = rbind(ds_df, full_df) %>%
  filter(year > 2019)


# ggplot(bc_comp, aes(x = week, y = abund, fill = type, color = type)) +
#   geom_area(position = "dodge", alpha = 0.3) +
#   facet_wrap(~year, nrow = 2) +
#   theme_classic()
# 
# ggplot(bc_comp, aes(x = week, y = pir, fill = type, color = type)) +
#   geom_area(position = "dodge", alpha = 0.3) +
#   facet_wrap(~year, nrow = 2) +
#   theme_classic()

ggplot(bc_comp, aes(x = week, y = vi, fill = type, color = type)) +
  geom_area(position = "dodge", alpha = 0.3) +
  facet_wrap(~year, nrow = 2) +
  theme_classic()
  
