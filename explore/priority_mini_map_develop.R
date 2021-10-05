source("./explore/app_init.R")
source("./explore/test_variable_values.R")


## Add Library

if(!("package:htmltools" %in% search())) {
  suppressMessages(library(htmltools))
}

## Define color and shape aesthetics. ----

# Priority.
priority_order <- c(c(year(now()):1914), "Statement Demand", "Environmental Demand")
priority_pal <- c(viridis_pal()(length(c(year(now()):1914))), "gray", "black")
names(priority_pal) <- priority_order

map_priority_pal <- colorFactor(palette = priority_pal, 
                                levels = names(priority_pal))

# Filter POD points.
pod_points <- 
  pods %>% 
  filter(huc8_name %in% huc8_selected)

# Filter watershed polygon.
plot_poly <- 
  huc8_layer %>% filter(huc8_name %in% huc8_selected)

mini_map_labs <- lapply(seq(nrow(pod_points)), function(i) {
  paste0('Water Right ID: ', st_drop_geometry(pod_points[i, "wr_id"]), '<br>',
         'Owner: ', st_drop_geometry(pod_points[i, "owner"]), '<br>',
         'Water Right Type: ', st_drop_geometry(pod_points[i, "wr_type"]), '<br>',
         'Priority: ' , st_drop_geometry(pod_points[i, "priority"]), '<br>',
         'Status: ', st_drop_geometry(pod_points[i, "wr_status"])
         )
})
# 
# map2 = leaflet( cities ) %>%
#   addTiles() %>%
#   addCircles(lng = ~lng, lat = ~lat, fillColor = 'darkBlue', radius = 10000,
#              stroke = FALSE, fillOpacity = 0.8,
#              label = lapply(mini_map_labs, HTML))
# 
# #########



leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = plot_poly,
              weight = 3,
              col = "blue",
              fill = TRUE,
              fillOpacity = 0#,
              # label = plot_poly$huc8_name,
              # labelOptions = labelOptions(textsize = "12px",
              #                             sticky = TRUE)
  ) %>% 
  addCircleMarkers(data = pod_points,
                   radius = 4,
                   fillOpacity = 0.8,
                   stroke = FALSE,
                   #  weight = 2,
                   fillColor = ~map_priority_pal(priority), 
                   label =  lapply(mini_map_labs, HTML)
                   
  )
