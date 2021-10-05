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

##### DEBUG #####
if(!("package:reactlog" %in% search())) {
  suppressMessages(library(reactlog))
}
reactlog_enable()


## Load data files. ----

# Water Right Info.
load("./output/dwast-wrinfo.RData")

# Demand data.
load("./output/dwast-demands.RData")

# Supply data.
load("./output/dwast-supplies.RData")

## Define color and shape aesthetics. ----

# Water right type.
wrt_pal <- c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))[2:10]
names(wrt_pal) <- sort(unique(wr_info$wr_type))


### UI -------------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 fluidRow(
                   
                   # Select Visualization to show.
                   radioButtons(inputId = "plot_type_selected",
                                label = "Select Visualization:",
                                choices = c(
                                  "View Supply-Demand Scenarios" = "vsd",
                                  "View Demand By Priority (Placeholder)" = "dbp",
                                  "View Demand By Water Right Type (Placeholder)" = "dbwrt"),
                                selected = "dbwrt"
                   ),
                   
                   # Select HUC-8 watershed.
                   selectInput(inputId = "huc8_selected",
                               label = "Select HUC-8 Watershed:",
                               choices = NULL,
                               selected = NULL,
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
                   
                   # Select water right types to include in dbwrt plot.
                   checkboxGroupInput(inputId = "wrt_selected",
                                      label = "Select Water Right Type(s) to Include:",
                                      choices = NULL,
                                      selected = NULL)
                 )
    ),
    
    mainPanel(width = 9,
              tableOutput(outputId = "main_table"))
  )
)


### SERVER ---------------------------------------------------------------------

server <- function(input, output, session) {
  
#  huc8_selected <- reactive({ input$huc8_selected })
  d_scene_selected <- reactive ({ input$d_scene_selected })
  scenario_demand <- reactive({ filter(demand[[input$huc8_selected]], 
                                       d_scenario %in% input$d_scene_selected) })
  wrt_selected <- reactive({ input$wrt_selected })
  
  ## Update selections. ----
  
  # if plot_type is vsd, filter available HUC8s to those with corresponding
  # supply data.  sort(names(demand))
  observeEvent(input$plot_type_selected, {
   # req(plot_type_selected)
    if (input$plot_type_selected != "vsd") {
      choices <- sort(names(demand))
    } else {
      choices <- sort(subset(names(demand), names(demand) %in% supply$huc8_name))
    }
    updateSelectInput(session, 
                      inputId = "huc8_selected",
                      choices = choices,
                      selected = "North Fork American")
  })
  
  # Update demand scenario choices.
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- sort(unique(demand[[input$huc8_selected]]$d_scenario))
    updateSelectizeInput(session, 
                         inputId = "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2019")
  })
  
  # Demand by water right type dataset.
  wrt_plot_data <- reactive({ 
    scenario_demand() %>% 
      filter(wr_type %in% wrt_selected()) %>% 
      group_by(d_scenario, 
               wr_type, 
               plot_month = month(plot_date, label = TRUE)) %>%
      summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
                af_daily = sum(af_daily, na.rm = TRUE),
                cfs = sum(cfs, na.rm = TRUE),
                .groups = "drop")
  })
  
  # Test render - table
  output$main_table <- renderTable({
    wrt_plot_data()
  })
  
}


### APP ------------------------------------------------------------------------

shinyApp(ui, server)


















