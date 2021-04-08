### INITIALIZATION -------------------------------------------------------------

# Load libraries.
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinipsum)
library(sf)
library(leaflet)
library(dplyr)
library(wesanderson)
library(ggplot2)
library(aws.s3)
library(DT)

## Initialize values. ---

# Data source.
load_from_s3 <- FALSE

# Water availability demand type order.
wa_demand_order <- ordered(c("Junior Post-14",
                             "Post-14",
                             "Statement Demand",
                             "Environmental Demand"))

## Load data files. ----

if(load_from_s3) {
  # Water Right Info.
  s3load(object = "dwast-wrinfo.RData",
         bucket = "dwr-enf-shiny")
  
  # Demand Data.
  s3load(object = "dwast-demands.RData",
         bucket = "dwr-enf-shiny")
  
  # Supply data.
  s3load(object = "dwast-supplies.RData",
         bucket = "dwr-enf-shiny")
} else {
  # Water Right Info.
  load("./output/dwast-wrinfo.RData")
  
  # Demand data.
  load("./output/dwast-demands.RData")
  
  # Supply data.
  load("./output/dwast-supplies.RData")
}

## Define color and shape aesthetics. ----

# Water right type.
wr_type_pal <-colorFactor(c(wes_palette("Darjeeling1"), 
                            wes_palette("Darjeeling2"))[1:9],
                          pods$wr_type)

# Demand.
wa_demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(wa_demand_pal) <- wa_demand_order

# Supply.
wa_supply_pal <- colorRampPalette(wes_palette("Zissou1")[1:2])(3)
wa_supply_shapes <- c(15, 16, 17)




### UI -------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # Set theme.
  theme = shinytheme("cerulean"),
  
  # Dashboard Title
  titlePanel(title = div(img(src = "DWR-ENF-Logo-48px.png"), 
                         "Water Supply/Demand Exploration Tool")),
  
             sidebarLayout(
               
               # Sidebar Panel.
               sidebarPanel(width = 3,
                            fluidRow(
                              
                              ## Select HUC-8 watershed.
                              selectInput(inputId = "huc8_selected",
                                          label = "Select HUC-8 Watershed:",
                                          choices = sort(names(demand)),
                                          selected = sample(unique(names(demand)), 1),
                                          multiple = FALSE),
                              
                              # Select Demand Scenario(s).
                              selectizeInput(inputId = "d_scene_selected",
                                             label = "Select Up To Two Demand Scenarios:",
                                             choices = NULL,
                                             selected = NULL,
                                             multiple = TRUE,
                                             options = list(maxItems = 2)
                              )
                            ) # End fluidRow
               ),
               
               # Main Panel.
               mainPanel(width = 9,
                         tabsetPanel(
                           
                           tabPanel(title = "Main",
                                    column(width = 7,
                                           h4("Demand in Selected Watershed"),
                                           plotOutput(outputId = "plot")
                                    ),
                                    column(width = 5,
                                           fluidRow(
                                             h4("Watershed Location Map"),
                                             imageOutput(outputId = "image",
                                                           height = "500px"),
                                             h4("Random Text"),
                                             tableOutput(outputId = "text")
                                           )
                                    )
                           ),
                           
                           tabPanel(title = "Water Right Info",
                                    fluidRow(
                                      h4("Water Rights in Selected Watershed"),
                                #      downloadButton(outputId = "dl_wr_info"),
                                      headerPanel(""),
                                      DTOutput(outputId = "data_table")
                                    )
                           ),
                           
                           tabPanel(title = "Demand Data",
                                    h4("Demands in Selected Scenario(s)"),
                                    DTOutput(outputId = "data_table")
                           )
                         )
               )
             )
   
  )

### SERVER ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  ws_demand_selected <- reactive({ demand[[huc8_selected]] })
  
  # Watch plot type radio buttons.
  observe({
    if(input$plot_type != "was") {
      hideElement(id = "supply_selected")
      hideElement(id = "priority_selected")
    } else {
      showElement(id = "supply_selected")
      showElement(id = "priority_selected")
    }
  })
  
  # Update demand scenario choices.
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- sort(unique(demand[[huc8_selected]]$d_scenario))
    updateSelectizeInput(session, "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2019")
  })
  

  
  
  output$data_table <- DT::renderDT({
    random_DT(10, 5)
  })
  output$image <- renderImage({
    random_image()
  }, deleteFile = FALSE)
  output$plot <- renderPlot({
    random_ggplot()
  })
  output$print <- renderPrint({
    random_print("model")
  })
  
  output$scenario_table <- renderTable({
    plot_height()
  })
  
  output$text <- renderText({
    random_text(nwords = 50)
  })
}

### APP ------------------------------------------------------------------------
shinyApp(ui, server)















