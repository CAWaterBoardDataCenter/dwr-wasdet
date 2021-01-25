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
  load("./output/dwast-demands-test-set.RData")
  
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

ui <- fluidPage(                                                   # b fluidPage
  useShinyjs(),
  
  # Set theme.
  theme = shinytheme("cerulean"),
  
  # Dashboard Title
  titlePanel(title = div(img(src = "DWR-ENF-Logo-48px.png"), 
                         "Water Supply/Demand Exploration Tool")),
  
  sidebarLayout(                                               # b sidebarLayout
    
    sidebarPanel(width = 3,                                     # b sidebarPanel
                 fluidRow(                                          # b fluidRow
                   
                   # Quick description of 3 plot.
                   random_text(nwords = 25),
                   br(), br(),
                   
                   # Select Visualization to show.
                   radioButtons(inputId = "plot_type_selected",
                                label = "Select Visualization:",
                                choices = c(
                                  "View Supply-Demand Scenarios" = "vsd",
                                  "Demand By Priority (Placeholder)" = "vbp",
                                  "View Demand By Water Right Type (Placeholder)" = "vbwrt"),
                                selected = "vsd"
                   ),
                   
                   # Select HUC-8 watershed.
                   selectInput(inputId = "huc8_selected",
                               label = "Select HUC-8 Watershed:",
                               choices = sort(names(demand)),
                               # selected = sample(unique(names(demand)), 1),
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
                   
                   ## Select supply scenario(s).
                   selectizeInput(inputId = "s_scene_selected",
                                  label = "Select Up To Three Supply Scenarios:",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 3)
                   ),
                   
                   ## Select priority year to slice.
                   selectInput(inputId = "priority_selected",
                               label = "Select Demand Priority Year to Slice:",
                               choices = NULL,
                               selected = NULL,
                               multiple = FALSE)
                   
                 )                                                  # e fluidRow
    ),                                                          # e sidebarPanel
    
    mainPanel(width = 9,                                           # b mainPanel
              
              h4("Demand in Selected Watershed"),
              plotOutput(outputId = "main_plot"),
              
              
    )                                                              # e mainPanel
    
  )                                                            # e sidebarLayout
  
)                                                                  # e fluidPage


### SERVER ---------------------------------------------------------------------

server <- function(input, output, session) {                          # b server
  
  demand_selected <- reactive({ demand[[input$huc8_selected]] })
  
  huc8_selected <- reactive({ input$huc8_selected })
  
  d_scene_selected <- reactive ({ input$d_scene_selected })
  
  s_scene_selected <- reactive ({ input$s_scene_selected })
  
  priority_selected <- reactive({ input$priority_selected })
  
  plot_type_selected <- reactive({ input$plot_type_selected })
  
  plot_height <- reactive({
    ifelse(is.null(d_scene_selected()),
           "auto",
           350 * length(d_scene_selected()))
  })
  
  ## TESTING ELEMENTS. ----
  
  observeEvent(input$d_scene_selected, {
    print(paste0("You have chosen: ", input$d_scene_selected))
  })
  
  ## Update elements. ----
  
  # Watch plot type radio buttons.
  observe({
    if(input$plot_type_selected != "vsd") {
      hideElement(id = "s_scene_selected")
      hideElement(id = "priority_selected")
    } else {
      showElement(id = "s_scene_selected")
      showElement(id = "priority_selected")
    }
  })
  
  ## Update selections. ----
  
  # Update demand scenario choices.
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- sort(unique(demand_selected()$d_scenario))
    updateSelectizeInput(session, "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2019")
  })
  
  # Update supply scenario choices.
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- sort(unique(filter(supply, 
                                  huc8_name %in% huc8_selected())$s_scenario))
    updateSelectizeInput(session, "s_scene_selected",
                         choices = choices,
                         selected = NULL)
  })
  
  # Update priority year choices.
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- sort(na.omit(unique(demand_selected()$p_year)),
                    decreasing = TRUE)
    updateSelectInput(session, "priority_selected",
                      choices = choices,
                      selected = max(choices))
  })
  
  ## Build datasets. ----
  
  # Supply exploration dataset. This is where the magic happens.
  vsd_plot_data <- reactive({
    bind_rows(filter(demand_selected(), 
                     d_scenario %in% d_scene_selected()) %>% 
                mutate(fill_color = if_else(priority == "Statement Demand",
                                            "Statement Demand",
                                            if_else(priority == "Statement Demand",
                                                    "Statement Demand",
                                                    if_else(p_year >= priority_selected(),
                                                            "Junior Post-14", "Post-14"))),
                       fill_color = ordered(fill_color, levels = wa_demand_order)) %>% 
                group_by(d_scenario, plot_date, fill_color) %>% 
                summarise(af_day = sum(af_day, na.rm = TRUE),
                          cfs = sum(cfs, na.rm = TRUE),
                          .groups = "drop") %>% 
                mutate(plot_group = "demand",
                       s_scenario = NA) %>% 
                select(d_scenario, s_scenario, plot_date,
                       fill_color, af_day, cfs, plot_group), 
              filter(supply, huc8_name %in% huc8_selected(),
                     s_scenario %in% s_scene_selected()) %>% 
                mutate(fill_color = NA,
                       plot_group = "supply") %>% 
                full_join(., 
                          as_tibble(d_scene_selected()), 
                          by = character()) %>% 
                select(d_scenario = value, 
                       s_scenario, 
                       plot_date, 
                       fill_color, 
                       af_day, 
                       cfs, 
                       plot_group))
  })
  
  ## Render outputs. ----
  
  # Main plot.
  output$main_plot <- renderPlot({
    
    # Validate.
    validate(
      need(input$d_scene_selected, 
           "No data to plot.\nPlease slelect at least one Demand Scenario.")
    )
    
    # View Supply and Demand (vsd).
    if (input$plot_type_selected == "vsd") {
      ggplot(data = vsd_plot_data(),
             aes(x = plot_date,
                 y = cfs)) +
        
        # Demand.
        geom_area(data = subset(vsd_plot_data(), plot_group == "demand"),
                  position = "stack",
                  aes(fill = fill_color)) +
        
        # Supply.
        geom_point(data = subset(vsd_plot_data(), plot_group == "supply"),
                   aes(color = s_scenario,
                       shape = s_scenario),
                   size = 4) +
        geom_line(data = subset(vsd_plot_data(), plot_group == "supply"),
                  aes(color = s_scenario)) +
        
        # X axis.
        scale_x_date(date_labels = "%m/%d/%y",
                     date_minor_breaks = "1 month") +
        
        # Demand legend.
        scale_fill_manual(name = "Demand Type:",
                          values = wa_demand_pal,
                          labels = c(paste(input$priority_selected, 
                                           "& Junior Post-14 Demand"),
                                     "Senior Post-14 Demand",
                                     "Statement Demand")) +
        
        # Supply legend.
        scale_shape_manual(name = "Supply Scenario:",
                           values = wa_supply_shapes) +
        scale_color_manual(name = "Supply Scenario:",
                           values = wa_supply_pal) +
        
        # Facet on demand scenario.
        facet_wrap(~ d_scenario, 
                   ncol = 1) +
        
        # Labels.
        labs(y = "Cubic Feet per Second (cfs)") +
        
        # Theme.
        theme_bw() +
        theme(# legend.position = "bottom",
          strip.text.x = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.2)),
          legend.text = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.2)),
          axis.title.x = element_blank())
      
    } else if (input$plot_type_selected == "vbwrt") {
      
      # Demand by water right type.
      random_ggplot()
    } else if (input$plot_type_selected == "vbp") {
      
      # Demand by priority.
      random_ggplot()
    }
    
  }, height = function() plot_height()
  )
  
  
  output$random_plot <- renderPlot({
    random_ggplot()
  })
  
  
}                                                                     # e server


### APP ------------------------------------------------------------------------

shinyApp(ui, server)








