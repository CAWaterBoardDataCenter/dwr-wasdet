source("./explore/app_init.R")
source("./explore/test_variable_values.R")


plot_poly <- filter(huc8_layer, huc8_name == huc8_selected)




pods <- pods %>% 
  mutate(mm_label_text = paste0("<p>Water Right ID: ", wr_id, "</br>",
                                "Owner: ", owner, "</br>",
                                "Water Right Type: ", wr_type, "</br>",
                                "Priority: " , priority, "</br>",
                                "Status: ", wr_status, "</p>"))

update_points <- filter(pods, huc8_name == huc8_selected)


library(leaflet)
library(htmltools)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = plot_poly,
              weight = 3,
              col = "blue",
              fill = TRUE,
              fillOpacity = 0
  ) %>% 
  
  addCircleMarkers(data = update_points,
                   radius = 4,
                   fillOpacity = 0.8,
                   stroke = FALSE,
                   weight = 2,
                   fillColor = "black", #~map_demand_pal(vsd_fill_color), 
                   label = ~htmlEscape(mm_label_text)
  )


# update_points <- pod_points()
# 
# mini_map_labs <- lapply(seq(nrow(update_points)), function(i) {
#   paste0('Water Right ID: ', st_drop_geometry(update_points[i, "wr_id"]), '<br>',
#          'Owner: ', st_drop_geometry(update_points[i, "owner"]), '<br>',
#          'Water Right Type: ', st_drop_geometry(update_points[i, "wr_type"]), '<br>',
#          'Priority: ' , st_drop_geometry(update_points[i, "priority"]), '<br>',
#          'Status: ', st_drop_geometry(update_points[i, "wr_status"])
#   )
# })
