library(leaflet)

# Water Right Info.
load("./output/dwast-wrinfo.RData")

station_locs <- read_csv("./common/station-locations.csv")

huc8_selected <- "North Fork American"
priority_selected <- 1954

# Filter POD points.
pod_points <- pods %>% 
    filter(huc8_name %in% huc8_selected) %>% 
    mutate(vsd_fill_color = if_else(priority == "Statement Demand",
                                    "Statement Demand",
                                    if_else(p_year >= priority_selected,
                                            "Junior Post-14", "Post-14")))

plot_poly <- huc8_layer %>% 
  filter(huc8_name %in% huc8_selected)

station_points <- 
  station_locs %>% filter(huc8_name %in% huc8_selected)

# Render.
leaflet() %>%
  
  # Add base map.
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addPolygons(data = plot_poly,
              weight = 3,
              col = "blue",
              fill = TRUE,
              fillOpacity = 0) %>% 
  
  addMarkers(data = station_points,
             lat = ~lat,
             lng = ~lng,
             label = ~htmlEscape(station_id))

