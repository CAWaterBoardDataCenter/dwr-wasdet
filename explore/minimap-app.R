# Initialization -------------------------------------------------------------

# Load libraries. ----
if (!("package:shiny" %in% search())) {
  suppressMessages(library(shiny))
}
if (!("package:shinythemes" %in% search())) {
  suppressMessages(library(shinythemes))
}
if (!("package:shinyjs" %in% search())) {
  suppressMessages(library(shinyjs))
}
if (!("package:shinipsum" %in% search())) {
  suppressMessages(library(shinipsum))
}
if (!("package:htmltools" %in% search())) {
  suppressMessages(library(htmltools))
}
if (!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if (!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}
if (!("package:sf" %in% search())) {
  suppressMessages(library(sf))
}
if (!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}
if (!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if (!("package:spdplyr" %in% search())) {
  suppressMessages(library(spdplyr))
}
if (!("package:readr" %in% search())) {
  suppressMessages(library(readr))
}
if (!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}
if (!("package:scales" %in% search())) {
  suppressMessages(library(scales))
}
if (!("package:DT" %in% search())) {
  suppressMessages(library(DT))
}

# Debug #####
if (Sys.info()["nodename"] == "Home-iMac.local") {
  source("./explore/test_variable_values.R")
  if (!("package:reactlog" %in% search())) {
    suppressMessages(library(reactlog))
  }
  reactlogReset()
  reactlog_enable()
}

# Initialize values. ----

# Data source. Currently using data included in repository. In future, will
# retrieve from AWS S3 bucket.

# Load data. ----

# Water Right Info.
load("./output/dwast-wrinfo.RData")

# Demand data. Load smaller test set if on my local machine.
ifelse(Sys.info()["nodename"] == "Home-iMac.local",
       load("./explore/dwast-demands-test-set.RData"),
       load("./output/dwast-demands.RData"))

# Supply data.
load("./output/dwast-supplies.RData")

# Load gage station location information.
station_locs <- read_csv("./common/station-locations.csv")

# Define color and shape aesthetics. ----

# Demand.
wa_demand_order <- ordered(c("Junior Post-14",
                             "Post-14",
                             "Statement Demand",
                             "Environmental Demand"))
wa_demand_pal <- c(wes_palettes$GrandBudapest1[c(2, 1)], "#BEBEBE", "#000000")
names(wa_demand_pal) <- wa_demand_order
map_demand_pal <- colorFactor(palette = wa_demand_pal,
                              levels = names(wa_demand_pal))

# Water right type.
plot_wrt_pal <- c(wes_palette("Darjeeling1"),
                  wes_palette("Darjeeling2"))[2:10]
names(plot_wrt_pal) <- sort(unique(wr_info$wr_type))
map_wrt_pal <- colorFactor(palette = plot_wrt_pal,
                           domain = wr_info$wr_type)

# Priority.
priority_order <- c(c(year(now()):1914),
                    "Statement Demand", "Environmental Demand")
priority_pal <- c(viridis_pal()(length(c(year(now()):1914))),
                  "#BEBEBE", "#000000")
names(priority_pal) <- priority_order
map_priority_pal <- colorFactor(palette = priority_pal,
                                levels = names(priority_pal))

# Supply.
wa_supply_pal <- colorRampPalette(wes_palette("Rushmore")[3:4])(3)
wa_supply_shapes <- c(15, 16, 17)

# Mini map icons.
station_icon <- icons(
  iconUrl = "./www/x-diamond-fill-red.svg"
)
#   iconWidth = 38, iconHeight = 95,
#   iconAnchorX = 22, iconAnchorY = 94,
#   shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
#   shadowWidth = 50, shadowHeight = 64,
#   shadowAnchorX = 4, shadowAnchorY = 62
# )


# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  
  # Title.
  title = div(img(src = "DWR-ENF-Logo.png",
                  style = "position: relative;
                  margin:-15px 0px;
                  display:right-align;"),
              paste("DWR-WaSDET: Division of Water Rights",
                    "Water Supply/Demand Exploration Tool")),
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:-6px !important; 
                            padding-bottom:0 !important;
                            height: 56px;
                            }
                           .navbar {min-height:56px !important;}'))
  ),
  
  # Set theme.
  theme = shinytheme("cerulean"),
  
  # Prototype Warning.
  h4(paste("Under Development. Do not rely on data used in this dashboard",
           "until it is officially released."), 
     style = "color:red"),
  
  fluidRow(
    sidebarLayout(
      
      ## Sidebar Panel. ----
      sidebarPanel(width = 2,
                   
                   ## Select units to display. ----
                   radioButtons(inputId = "units_selected",
                                label = "Select Units:",
                                choiceNames = list(
                                  "Cubic Feet per Second (cfs)",
                                  "Acre-Feet per Day (AF/d)"
                                ),
                                choiceValues = list(
                                  "cfs",
                                  "afd"
                                ),
                                selected = "cfs"
                   ),
                   
                   ### Select HUC-8 watershed. ----
                   selectInput(inputId = "huc8_selected",
                               label = "Select HUC-8 Watershed:",
                               choices = NULL,
                               selected = NULL,
                               multiple = FALSE
                   ),
                   
                   ### Filter for watersheds with supply information. ----
                   checkboxInput(inputId = "supply_filter",
                                 label = "Filter for watersheds with available supply information",
                                 value = FALSE
                   ),
                   
                   ### Select demand scenario(s). ----
                   selectizeInput(inputId = "d_scene_selected",
                                  label = "Select Up To Two Demand Scenarios:",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 2)
                   ),
                   
                   ### Conditional Inputs. ----
                   
                   wellPanel(
                     
                     ### #Select supply scenario(s) for vsd_plot. ----
                     selectizeInput(inputId = "s_scene_selected",
                                    label = "Select Up To Three Supply Scenarios:",
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = TRUE,
                                    options = list(maxItems = 3)
                     ),
                     
                     #### Select priority year to slice for vsd_plot. ----
                     selectInput(inputId = "priority_selected",
                                 label = "Select Demand Priority Year:",
                                 choices = NULL,
                                 selected = NULL,
                                 multiple = FALSE),
                     
                     #### Select water right types to include in dbwrt_plot. ----
                     checkboxGroupInput(inputId = "wrt_selected",
                                        label = "Select Water Right Type(s) to Display:",
                                        choices = NULL,
                                        selected = NULL)
                   ),
                   
                   # My logos !!
                   br(),br(),br(),
                   HTML('<center><p>Built with</p>
                      <p><img src="shiny.png", height = "50">
                      and <img src="RStudio.png", height = "50">
                      by <a href="https://img1.looper.com/img/gallery/keanu-reeves-head-turning-comment-on-the-script-for-matrix/intro-1569601235.jpg">
                      <img src="jgy_hex.png", height = "50"></p></center></a>')
      ),
      
      ## Main Panel. ----
      mainPanel(width = 10,
                
                # Plot/Data/Watershed map Tabs.
                tabsetPanel(type = "pills",
                            
                            ### Plot tabs. ----
                            tabPanel("Plots",
                                     fluidRow(
                                       
                                       # Plot column.
                                       column(width = 3,
                                              tabsetPanel(id = "plot_tabs",
                                                          type = "pills",
                                                          
                                                          ### Supply-Demand plot tab. ----
                                                          tabPanel(title = "Supply-Demand Scenarios",
                                                                   id = "vsd_tab",
                                                                   # fluidRow(
                                                                   #   plotOutput(outputId = "vsd_plot")
                                                                   # )
                                                          ),
                                                          
                                                          ### Demand by Water right type plot tab. ----
                                                          tabPanel(title = "Demand by Water Right Type",
                                                                   id = "dbwrt_tab",
                                                                   # fluidRow(
                                                                   #   br(),
                                                                   #   plotOutput(outputId = "dbwrt_plot")
                                                                   # )
                                                          ),
                                                          
                                                          ###Demand by priority plot tab. ----
                                                          tabPanel(title = "Demand by Priority",
                                                                   id = "dbp_tab",
                                                                   # fluidRow(
                                                                   #   br(),
                                                                   #   plotOutput(outputId = "dbp_plot")
                                                                   # )
                                                          )
                                              )      
                                       ),
                                       
                                       ## Mini map column. ----
                                       column(width = 9,
                                              fluidRow(
                                                h4("Watershed Location and PODs"),
                                                leafletOutput(outputId = "mini_map",
                                                              height = "500px",
                                                              width = "95%"),
                                                br(),br(),
                                                
                                                ### Debug notes. ----
                                                h3("Debug"),
                                                
                                                textOutput("debug_text")
                                              )
                                       )
                                     )
                            ),    
                            
                            # Data Tabs.
                            tabPanel("Data",
                                     br(),
                                     h3("Selected Demand Data"),
                                     DTOutput(outputId = "demand_data_table")
                            ),
                            
                            # California Watershed Map.
                            tabPanel("California Watershed Map",
                                     br(),
                                     h3("Coming soon...")
                            )
                )      
      )
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Debug. ----
  
  output$debug_text <- renderText({ paste0("DEBUG: input$supply_filter is: ", 
                                           input$supply_filter) })
  
  # Setup. ----
  
  # Disable units selector until implemented
  disable(id = "units_selected")
  #  disable(id = "supply_filter")
  
  # Helper Functions. ----
  
  # Define plot height function to keep facet panels roughly the same height
  # whether displaying one or two.
  plot_height <- reactive({
    ifelse(length(input$d_scene_selected) == 1, 480,
           ifelse(length(input$d_scene_selected) == 2, 835, "auto"))
  })
  
  # Observers. ----
  
  ## Control input selectors. ----
  observe({
    if (input$plot_tabs == "Supply-Demand Scenarios") {
      showElement(id = "s_scene_selected")
      showElement(id = "priority_selected")
      hideElement(id = "wrt_selected")
    } else
      if (input$plot_tabs == "Demand by Water Right Type") {
        hideElement(id = "s_scene_selected")
        hideElement(id = "priority_selected")
        showElement(id = "wrt_selected")
      } else
        if (input$plot_tabs == "Demand by Priority") {
          hideElement(id = "s_scene_selected")
          hideElement(id = "priority_selected")
          showElement(id = "wrt_selected")
        }
  })
  
  ## Filter for watersheds that have supply data. ----
  observeEvent(input$supply_filter, {
    if (input$supply_filter) { 
      choices <- sort(names(demand)[names(demand) %in% supply$huc8_name]) 
    } else { 
      choices <- sort(names(demand))
    }
    updateSelectInput(session,
                      inputId = "huc8_selected",
                      choices = choices,
                      selected = "North Fork American")
  })
  
  ## Update demand scenario choices. ----
  observeEvent(input$huc8_selected, {
    choices <- sort(unique(demand[[input$huc8_selected]]$d_scenario))
    updateSelectizeInput(session, 
                         inputId = "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2019")
  })
  
  ## Update supply scenario choices. ----
  observeEvent(input$huc8_selected, {
    choices <- sort(unique(filter(supply, 
                                  huc8_name %in% input$huc8_selected)$s_scenario))
    updateSelectizeInput(session, 
                         inputId = "s_scene_selected",
                         choices = choices,
                         selected = c("Historic: Estimated Mean Unimpaired Flow at AMA, Wet Year",
                                      "Historic: Estimated Mean Unimpaired Flow at AMA, Critical Year"))
  })
  
  ## Update priority year choices. ----
  py_choice_list <- reactive({
    sort(na.omit(unique(demand[[input$huc8_selected]]$p_year)), decreasing = TRUE)
  })
  observeEvent(input$d_scene_selected, {
    choices <- py_choice_list()[py_choice_list() > min(py_choice_list())]
    updateSelectInput(session, "priority_selected",
                      choices = choices,
                      # selected = max(choices),
                      selected = sample(choices, 1))
  })
  
  ## Update water right type choices. ----
  observeEvent(input$huc8_selected, {
    choices <- unique(demand[[input$huc8_selected]]$wr_type)
    updateCheckboxGroupInput(session = session, 
                             inputId = "wrt_selected",
                             choices = choices,
                             selected = choices)
  })
  
  
  # Outputs. ----
  
  ## Mini Map. ----
  
  # Filter watershed polygon.
  plot_poly <- reactive({
    huc8_layer %>% filter(huc8_name %in% input$huc8_selected)
  })
  
  # Filter POD points.
  pod_points <- reactive({
    pods %>% 
      filter(huc8_name %in% input$huc8_selected) %>% 
      mutate(vsd_fill_color = if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(p_year >= input$priority_selected,
                                              "Junior Post-14", "Post-14")))
  })
  
  # Filter gauge stations.
  station_points <- reactive({
    station_locs %>% filter(huc8_name %in% input$huc8_selected)
  })
  
  output$mini_map <- renderLeaflet({
    
    # Validate.
    validate(
      need(nrow(pod_points()) > 0, 
           paste0("No Data Available.\n",
                  "Please select other Watershed(s) or Water Right Type(s)."))
    )
    
    # Render.
    leaflet() %>%
      
      # Add base map.
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Add selected watershed polygon.
      addPolygons(data = plot_poly(),
                  weight = 3,
                  col = "blue",
                  fill = TRUE,
                  fillOpacity = 0) %>% 
      addMarkers(data = station_points(),
                 lat = ~lat,
                 lng = ~lng,
                 icon = station_icon,
                 label = ~htmlEscape(station_id))
  })
  
  ### Update POD points and legends. ----
  observe({

    update_points <- pod_points()

    mini_map_labs <- lapply(seq(nrow(update_points)), function(i) {
      paste0('Water Right ID: ', st_drop_geometry(update_points[i, "wr_id"]), '<br>',
             'Owner: ', st_drop_geometry(update_points[i, "owner"]), '<br>',
             'Water Right Type: ', st_drop_geometry(update_points[i, "wr_type"]), '<br>',
             'Priority: ' , st_drop_geometry(update_points[i, "priority"]), '<br>',
             'Status: ', st_drop_geometry(update_points[i, "wr_status"])
      )
    })

    #### vsd plot points. ----
    if( input$plot_tabs == "Supply-Demand Scenarios" ) {

      leafletProxy(mapId = "mini_map") %>%
        clearGroup(group = "content") %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(group = "content",
                         data = update_points,
                         radius = 4,
                         fillOpacity = 0.8,
                         stroke = FALSE,
                         weight = 2,
                         fillColor = ~map_demand_pal(vsd_fill_color),
                         label = lapply(mini_map_labs, HTML)
                         ) %>%

        addLegend(position = "topright",
                  colors = wa_demand_pal[1:3],
                  labels = c(paste(input$priority_selected, "& Junior Post-14 Demand"),
                             paste(as.numeric(input$priority_selected) -1, "& Senior Post-14 Demand"),
                             "Statement Demand"),
                  title = "Demand Type",
                  opacity = 1)
    } else

      ### dbwrt plot points. ----
    if( input$plot_tabs == "Demand by Water Right Type" ) {

      leafletProxy(mapId = "mini_map",
                   data = update_points) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(radius = 4,
                         fillOpacity = 0.8,
                         stroke = FALSE,
                         weight = 2,
                         fillColor = ~map_wrt_pal(wr_type),
                         label = lapply(mini_map_labs, HTML)
        ) %>%
        addLegend(position = "topright",
                  pal = map_wrt_pal,
                  values = update_points$wr_type,
                  title = "Water Right Type",
                  opacity = 1)
    } else

      ### vbp plot points. ----
    if( input$plot_tabs == "Demand by Priority" ) {

      leafletProxy(mapId = "mini_map",
                   data = update_points) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(radius = 4,
                         fillOpacity = 0.9,
                         stroke = FALSE,
                         #  weight = 2,
                         fillColor = ~map_priority_pal(priority),
                         label = lapply(mini_map_labs, HTML)
        ) %>%
        addControl(html = "Legend not provided. Too many categories.",
                   position = "topright")
    }

  })

  ### Update gauge station markers. ----
  # observe({
  #
  #   update_stations <- station_points()
  #
  #   if( input$plot_tabs == "Supply-Demand Scenarios" ) {
  #
  #     leafletProxy(mapId = "mini_map") %>%
  #       addMarkers(data = station_points(),
  #                  lat = ~lat,
  #                  lng = ~lng,
  #                  label = ~htmlEscape(station_id))
  #   }
  #
  #
  # })
  
  ## Tables. ----
  
  ## Demand data table.
  output$demand_data_table <- renderDataTable({
    demand[[input$huc8_selected]] %>% 
      filter(d_scenario %in% input$d_scene_selected,
             wr_type %in% input$wrt_selected) %>% 
      mutate(plot_date = as.integer(month(plot_date))) %>% 
      rename(month = plot_date) %>% 
      select(-p_year)
  },
  filter = "top",
  rownames = FALSE)
  
} # End Server

# APP --------------------------------------------------------------------------

shinyApp(ui = ui,
         server = server)













































