

source("app_init.R")

source("./explore/test_variable_values.R")



## Color and shape aesthetics

# wr_type_list <- sort(unique(wr_info$wr_type))
# # Water right type.
# wrt_pal <- c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))[2:10]
# names(wrt_pal) <- sort(unique(wr_info$wr_type))

# Water right type.
plot_wrt_pal <- c(wes_palette("Darjeeling1"), 
                  wes_palette("Darjeeling2"))[2:10]
#names(plot_wrt_pal) <- sort(unique(wr_info$wr_type))
map_wrt_pal <- colorFactor(palette = plot_wrt_pal, 
                           domain = wr_info$wr_type)


##########


# Filter POD points.
pod_points <- #reactive({
  pods %>% 
    filter(wr_id %in% demand[[huc8_selected]]$wr_id)
#})

# Filter watershed polygon
plot_poly <- #reactive({
  huc8_layer %>% filter(huc8_name %in% huc8_selected)
#})


# Demand by water right type dataset.
wrt_plot_data <- #reactive({ 
  # req(input$huc8_selected, input$d_scene_selected)
  demand[[huc8_selected]] %>% 
    filter(d_scenario %in% d_scene_selected,
           wr_type %in% wrt_selected) %>% 
    group_by(d_scenario, 
             wr_type, 
             plot_month = month(plot_date, label = TRUE)) %>%
    summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
              af_daily = sum(af_daily, na.rm = TRUE),
              cfs = sum(cfs, na.rm = TRUE),
              .groups = "drop")
#})



leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = plot_poly,
              weight = 2,
              col = "blue",
              fill = TRUE,
              fillOpacity = 0,
              label = plot_poly$huc8_name,
              labelOptions = labelOptions(textsize = "12px",
                                          sticky = TRUE)) %>%
  addCircleMarkers(data = pod_points,
                   # lat = pod_points()$lat, 
                   # lng = pod_points()$lon,
                   radius = 4,
                   fillOpacity = 0.7,
                   stroke = FALSE,
                   # color = "black",
                   weight = 2,
                   fillColor = ~map_wrt_pal(wr_type),
                   label = pod_points$wr_id
  ) %>% 
  addLegend(position = "bottomleft",
            pal = map_wrt_pal,
            values = pod_points$wr_type,
            title = "Water Right Type",
            opacity = 1)






























