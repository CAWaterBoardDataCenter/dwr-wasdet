### INITIALIZATION -------------------------------------------------------------

## Load libraries. ----
if(!("package:shiny" %in% search())) {
  suppressMessages(library(shiny))
}
if(!("package:shinythemes" %in% search())) {
  suppressMessages(library(shinythemes))
}
if(!("package:shinyjs" %in% search())) {
  suppressMessages(library(shinyjs))
}
if(!("package:shinipsum" %in% search())) {
  suppressMessages(library(shinipsum))
}
if(!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if(!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}
if(!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}
if(!("package:janitor" %in% search())) {
  suppressMessages(library(janitor))
}
if(!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if(!("package:sf" %in% search())) {
  suppressMessages(library(sf))
}
if(!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}
if(!("package:scales" %in% search())) {
  suppressMessages(library(scales))
}
if(!("package:aws.s3" %in% search())) {
  suppressMessages(library(aws.s3))
}
if(!("package:DT" %in% search())) {
  suppressMessages(library(DT))
}

#### DEBUG ----
if(!("package:reactlog" %in% search())) {
  suppressMessages(library(reactlog))
}
reactlog_enable()

## Initialize values. ---

# Data source.
load_from_s3 <- ifelse(Sys.info()["nodename"] == "Home-iMac.local", FALSE, TRUE)

## Load data files. ----

# Demand data.
load("./output/dwast-demands.RData")

## Define color and shape aesthetics. ----

# Water right type.
wrt_pal <- c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))[2:10]
names(wrt_pal) <- sort(unique(wr_info$wr_type))


### UI -------------------------------------------------------------------------

ui <- fluidPage(                                                   # b fluidPage
  useShinyjs(),
  
  sidebarLayout(                                               # b sidebarLayout
    
    sidebarPanel(width = 3,                                     # b sidebarPanel
                 fluidRow(                                          # b fluidRow
                   
                   # Select HUC-8 watershed.
                   selectInput(inputId = "huc8_selected",
                               label = "Select HUC-8 Watershed:",
                               choices = names(demand),
                               selected = "North Fork American",
                               multiple = FALSE
                   ),
                   
                   # Select demand scenario(s).
                   selectizeInput(inputId = "d_scene_selected",
                                  label = "Select Up To Two Demand Scenarios:",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 2)
                   ),
                   
                   ## Select water right types to include in vbwrt plot.
                   checkboxGroupInput(inputId = "wrt_selected",
                                      label = "Select Water Right Type(s) to Include:",
                                      choices = NULL,
                                      selected = NULL)
                   
                 )                                                  # e fluidRow
    ),                                                          # e sidebarPanel
    
    mainPanel(width = 9,                                           # b mainPanel
              
              # h4("Demand in Selected Watershed"),
              plotOutput(outputId = "main_plot")
              
              
    )                                                              # e mainPanel
    
  )                                                            # e sidebarLayout
  
)                                                                  # e fluidPage


### SERVER ---------------------------------------------------------------------

server <- function(input, output, session) {                          # b server
  
  ## SETUP. ----
  
  # Convert input values to reactive variables.
  demand_selected <- reactive({ demand[[input$huc8_selected]] })
  huc8_selected <- reactive({ input$huc8_selected })
  d_scene_selected <- reactive ({ input$d_scene_selected })
  wrt_selected <- reactive({ input$wrt_selected })
  
  scenario_demand <- reactive({ 
    req(input$huc8_selected)
    filter(demand[[input$huc8_selected]],
           d_scenario %in% d_scene_selected()) })
  
  # Define plot height function to keep facet panels roughly the same height
  # whether displaying one or two.
  plot_height <- reactive({
    ifelse(length(d_scene_selected()) == 1, 480,
           ifelse(length(d_scene_selected()) == 2, 835, "auto"))
    
  })
  
  ## TESTING ELEMENTS. ----
  
  # observeEvent(input$huc8_selected, {
  #   # print(paste0("class: ", class(input$priority_selected)))
  #   print(head(demand_selected()))
  # })
  
  ## Update elements. ----
  
  ## Update selections. ----
  
  # Update demand scenario choices.
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- sort(unique(demand_selected()$d_scenario))
    updateSelectizeInput(session, 
                         inputId = "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2019")
  })
  

  # Update water right type choices.
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected, input$d_scene_selected)
    choices <- unique(demand_selected()$wr_type)
    updateCheckboxGroupInput(session = session, 
                             inputId = "wrt_selected",
                             choices = choices,
                             selected = choices)
  })
  
  ## Build datasets. ----
  
  # Demand by water right type dataset.
  wrt_plot_data <- reactive({ 
  # req(input$huc8_selected, input$d_scene_selected)
    demand_selected() %>% 
      filter(d_scenario %in% d_scene_selected(),
             wr_type %in% wrt_selected()) %>% 
      group_by(d_scenario, 
               wr_type, 
               plot_month = month(plot_date, label = TRUE)) %>%
      summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
                af_daily = sum(af_daily, na.rm = TRUE),
                cfs = sum(cfs, na.rm = TRUE),
                .groups = "drop")
  })
  
  ## Render outputs. ----
  
  # Main plot.
  output$main_plot <- renderPlot({
    
    # Validate.
    validate(
      need(input$d_scene_selected, 
           "No data to plot.\nPlease slelect at least one Demand Scenario."),
      need(input$huc8_selected, 
           "No data to plot.\nPlease slelect a Watershed.")
    )
    
    # Demand by water right type. ----
    ggplot(data = wrt_plot_data(),
           aes(x = plot_month,
               y = cfs,
               fill = wr_type)) +
      
      # Demand bars.
      geom_col() + 
      
      # Y axis format.
      scale_y_continuous(labels = comma) +
      
      # Legend.
      scale_fill_manual(name = "Water Right Type:",
                        values = wrt_pal) +
      
      # labels
      labs(title = "Monthly Demand by Water Right Type",
           y = "Cubic Feet per Second (cfs)") +
      
      # # Facet.
      # facet_wrap(~ d_scenario,
      #            ncol = 1) +
      
      # Theme.
      theme_minimal() +
      theme(
        plot.title = element_text(size = rel(2.0)),
        strip.text.x = element_text(size = rel(2.0)),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "vertical",
        panel.spacing = unit(2, "lines"),
        axis.title.x = element_blank())
    
  }, height = function() plot_height()
  )
  
}                                                                     # e server


### APP ------------------------------------------------------------------------

shinyApp(ui, server)








