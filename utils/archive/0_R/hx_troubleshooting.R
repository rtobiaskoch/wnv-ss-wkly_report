t = culex_database %>% 
  filter(week == ) %>%
  filter(zone == "NW") %>% 
  filter(method == "L") %>%
  filter(year %in% 2021)


t_abund = calc_abund(t, grp_var = c("zone","week"))

mean(t_abund$abund)


t2 = database %>% 
  filter(week == 24) %>%
  filter(zone == "NW") %>% 
  filter(method == "L") %>%
  filter(year %in% 2021)


culex_db_dupes = get_dupes(culex_database, year, week, zone, trap_id, spp)

waldo::compare(culex_database, culex_update)
