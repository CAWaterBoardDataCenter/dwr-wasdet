



huc8_selected <- "San Pablo Bay"
priority_selected <- 1950

demand_test %>% 
  filter(huc8_name %in% huc8_selected) %>% 
  mutate(fill_color = if_else(priority == "Statement Demand",
                              "Statement Demand",
                              if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(p_year >= priority_selected,
                                              "Junior Post-14", "Post-14"))),
         fill_color = ordered(fill_color, levels = demand_order)) %>% 
  group_by(rept_date, fill_color) %>% 
  summarize(demand = sum(demand, na.rm = TRUE),
            demand_daily = sum(demand_daily, na.rm = TRUE),
            .groups = "drop")

























# Define water right types included in dataset.
wr_type_list <- c("Appropriative",
                  "Federal Claims",
                  "Federal Stockponds",
                  "Registration Cannabis",
                  "Registration Domestic",
                  "Registration Irrigation",
                  "Registration Livestock",
                  "Statement of Div and Use",
                  "Stockpond")

load("./output/wr_info-2020-11-04 14:31:31.RData")

# Define fill color for plot points (currently wr_type)
pal1 <- c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))[1:9]
names(pal1) <- wr_type_list

salinas <- pods %>% filter(huc8_name == "Salinas")

wr_type_pal <- colorFactor(palette = pal1, domain = salinas$wr_type, reverse = F)

salinas %>% 
  leaflet() %>% 
  addCircleMarkers(
  radius = 4,
  stroke =TRUE,
  color = ~wr_type_pal(wr_type),
  weight = 2,
  fill = ~wr_type_pal(wr_type),
  fillOpacity = 0.2,
  popup = ~paste(wr_id, "<br>",
    owner, "<br>",
    wr_type))  %>% 
  addLegend(position = "bottomleft",
            pal = wr_type_pal,
            values = ~wr_type,
            title = "Water Right Type",
            opacity = 1)







# Reproject points to st_crs("+proj=longlat +datum=WGS84 +no_defs") (4326).
# This is projection leaflet likes.
pod_points <- st_transform(pod_points, 4326)

# leaflet plot.
pod_points %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 3,
                   weight = 1,
                   clusterOptions = markerClusterOptions())

# Determine POD counts for each water right id.
pod_points <- pod_points %>% 
  group_by(wr_id) %>% 
  mutate(n_hucs = n_distinct(huc8_name)) %>% 
  group_by(wr_id, huc8_name) %>% 
  mutate(n_pods = n()) %>% 
  select(wr_id, huc8_name, n_hucs, n_pods, SHAPE)


pod_points_dissolved <- pod_points %>% 
  group_by(wr_id, huc8_name) %>% 
  summarise_all(mean)
pod_points_dissolved
class(pod_points_dissolved)
str(pod_points_dissolved)
plot(st_geometry(pod_points_dissolved))

# leaflet plot.
# I'm told that leafly can handle multipoints in GeoJSON format.
pod_points_geo <- geojson_json(pod_points_dissolved)
pod_points_geo %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers()


# with clustering.
pod_points_dissolved %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers()


#######################################################

Water Right Info
================
  
  Inputs {.sidebar data-width=300}
-----------------------------------------------------------------------
  
  ```{r}
# Reporting Status Filter.
radioButtons(inputId = "filing_status", 
             label = "Select Reporting Status:",
             choices = c("Filed" = "filed",
                         "Did Not File" = "not_filed",
                         "Both" = "both"),
             selected = "both")

rep_status_filter <- reactive({
  
  if(input$filing_status == "both") {
    display_df <- reporting
  }
  if(input$filing_status == "filed") {
    display_df <- reporting %>% 
      filter(!is.na(report_filed_date))
  }
  if(input$filing_status == "not_filed") {
    display_df <- reporting %>% 
      filter(is.na(report_filed_date))
  }
  display_df
})
```
<br>
  
  ```{r}
# Select Watershed(s)
selectInput(inputId = "ws_selected",
            label = "Select Watershed(s):",
            choices = sort(unique(reporting$watershed)),
            selected = sample(unique(reporting$watershed), 1),
            multiple = TRUE)
```
If you are unsure of the watershed name of the area you are interested in, select the *Watershed Map* tab. Hover your mouse over the area of interest, then select the name in the box above.  
<br>
  
  ```{r}
# Select/deselect HUC 8 watersheds.
selectInput(inputId = "huc8_selected",
            label = "Select HUC8 Watershed(s):",
            choices = NULL,
            selected = NULL,
            multiple = TRUE)
```
<br>
  
  ```{r}
# Select Water Right types.
checkboxGroupInput(inputId = "wrt_selected",
                   label = "Select Water Right Type(s) to Include:",
                   choices = wr_type_list,
                   selected = wr_type_list[1:2])

selected_rights <- reactive({ input$wrt_selected })

ws_selected <- reactive({
  filter(reporting, watershed %in% input$ws_selected)
})

observeEvent(ws_selected(), {
  choices <- unique(ws_selected()$watershed_huc8)
  updateSelectInput(session, "huc8_selected", choices = choices,
                    selected = choices)
})

# Filter Water Rights table.
reporting_table <- reactive({
  filter(rep_status_filter(),
         watershed %in% input$ws_selected,
         watershed_huc8 %in% input$huc8_selected,
         wr_type %in% input$wrt_selected) %>% 
    select("wr_id", 
           "owner", 
           "wr_status", 
           "wr_type",
           "watershed",
           "watershed_huc8",
           "report_due_date", 
           "report_filed_date")
})

```

Column {data-width=150 .tabset}
-----------------------------------------------------------------------
  
  ### Water Right Locations
  
  ```{r}
pod_points <- reactive({
  pod_info %>% 
    filter(wr_id %in% reporting_table()$wr_id)
})

ws_poly <- reactive({
  cw_shape_data %>% filter(HUNAME %in% reporting_table()$watershed)
})

renderLeaflet({
  validate(
    need(nrow(pod_points()) > 0, 
         paste0("No Data Available.\n",
                "Please select other Watershed(s) or Water Right Type(s)."))
  )
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = ws_poly(),
                weight = 2,
                col = "blue",
                fill = TRUE,
                fillOpacity = 0,
                label = ws_poly()$HUNAME,
                labelOptions = labelOptions(textsize = "12px",
                                            sticky = TRUE)) %>% 
    addCircleMarkers(lat = pod_points()$lat, 
                     lng = pod_points()$lon,
                     radius = 6,
                     fillOpacity = 0.7,
                     stroke = TRUE,
                     color = "black",
                     weight = 2,
                     fillColor = pod_points()$color,
                     label = pod_points()$wr_id) %>% 
    addLegend(position = "bottomleft",
              color = c("green", "red"),
              labels = c("Filed", "Did Not File"),
              title = "POD Color Legend",
              opacity = 1)
})
```

> This map initializes by displaying Water Right PODs for a random watershed.

### Water Right Info

```{r}
DT::renderDataTable(reporting_table(), 
                    colnames = c("Water Right ID",
                                 "Primary Owner",
                                 "Water Right Status",
                                 "Water Right Type",
                                 "Watershed",
                                 "HUC-8 Watershed",
                                 "Date Report Due",
                                 "Date Report Filed"),
                    options = list(pageLength = 25))
```

> Data current through `r format(max(reporting$report_filed_date, na.rm = TRUE), "%B %d, %Y")`.

### Watershed Map

If you are unsure of the watershed name of the area you are interested in, locate the area in the map below to identify the watershed name to enter in the box to the left.  

```{r water-shed-map}

renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = cw_shape_data,
                weight = 2,
                col = "blue",
                fill = TRUE,
                fillOpacity = 0,
                label = cw_shape_data$HUNAME,
                labelOptions = labelOptions(textsize = "12px",
                                            sticky = TRUE))
})

```











