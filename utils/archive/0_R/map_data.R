list2env(readRDS(config_params_file),           
         envir = .GlobalEnv)

gsheet_pull(key_trap_gsheet, "data", fn_trap)

trap_data = read.csv(fn_trap)

# Create a color palette based on the 'zone' column
pal <- colorFactor(palette = "viridis", domain = trap_data$zone)

# Create the Leaflet map
leaflet(trap_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    color = ~pal(zone),
    popup = ~paste0("Trap ID: ", trap_id, "<br>Zone: ", zone)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~zone, title = "Zone")

