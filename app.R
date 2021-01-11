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

load_from_s3 <- FALSE

## Load data files. ----

if(load_from_s3) {
  # Water Right Info.
  s3load(object = "dwast-wrinfo.RData",
         bucket = "dwr-enf-shiny")
  
  # Demand Data.
  s3load(object = "dwast-demands.RData",
         bucket = "dwr-enf-shiny")
} else {
  # Water Right Info.
  load("./output/dwast-wrinfo-2021-01-05.RData")
  
  # Demand Data.
  load("./output/dwast-demands-2021-01-05.RData")
}

## Define color palettes and plot order. ----

# Water right type.
wr_type_pal <-colorFactor(c(wes_palette("Darjeeling1"), 
                            wes_palette("Darjeeling2"))[1:9],
                          pods$wr_type)

# Water availability demand.
wa_demand_order <- ordered(
  c("Junior Post-14",
    "Post-14",
    "Statement Demand",
    "Environmental Demand")
)
wa_demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(wa_demand_pal) <- wa_demand_order

## UI --------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # Set theme.
  theme = shinytheme("cerulean"),
  
  # Dashboard Title
  titlePanel(title = div(img(src = "DWR-ENF-Logo-48px.png"), 
                         "Water Availability Screening Tool")),
  
  tabsetPanel(
    tabPanel(title = "Statewide By HUC-8 Watershed", #Tab Panel 1
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
                 
                 ## Select Demand Scenario(s).
                 selectizeInput(inputId = "scenario_selected",
                                label = "Select Up To Two Demand Scenarios:",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE,
                                options = list(maxItems = 2)
                                ),
                 
                 ## Select Visualization.
                 radioButtons(inputId = "plot_type",
                              label = "Select Visualization:",
                              choices = c(
                                "Demand By Water Right Type" = "db_wrt",
                                "Demand By Priority" = "db_pri",
                                "Water Availability Screening" = "was"),
                              selected = "was"
                              ),
                 
                 random_text(nwords = 25),
                 br(), br(),
                 
                 # Select supply forecasts (limit 3?) # DRAFT ###
                 selectizeInput(inputId = "supply_selected",
                                label = "Select Up To Three Supply Scenarios:",
                                choices = c(
                                  "Forecast: 99% Probability of Exceedance",
                                  "Forecast: 90% Probability of Exceedance",
                                  "Forecast: 75% Probability of Exceedance",
                                  "Forecast: 50% Probability of Exceedance",
                                  "Forecast: 25% Probability of Exceedance",
                                  "Forecast: 10% Probability of Exceedance",
                                  "Historical: Mean",
                                  "Historical: Median",
                                  "Historical: p10",
                                  "Historical: p90",
                                  "Historical: Individual Years?"
                                ),
                                selected = c(
                                  "Forecast: 90% Probability of Exceedance",
                                  "Forecast: 75% Probability of Exceedance",
                                  "Forecast: 50% Probability of Exceedance"
                                ),
                                multiple = TRUE,
                                options = list(maxItems = 3)
                 ),
                 
                 ## Select priority year to slice.
                 selectInput(inputId = "priority_selected",
                             label = "Select Priority Year:",
                             choices = NULL,
                             selected = NULL,
                             multiple = FALSE)
                 ) # End fluidRow
                 ),
               
               # Main Panel.
               mainPanel(width = 9,
                 tabsetPanel(
                 
                   tabPanel(title = "Main",
                          column(width = 7,
                                 h4("Demand in Selected Watershed"),
                                 plotOutput(outputId = "demand_plot")
                                 ),
                          column(width = 5,
                                 fluidRow(
                                 h4("Watershed Location"),
                                 leafletOutput(outputId = "mini_map",
                                               height = "500px"),
                                 h4("Random Text"),
                                 tableOutput(outputId = "text")
                                 )
                                 )
                          ),
                 
                 tabPanel(title = "Water Right Info",
                          fluidRow(
                          h4("Water Rights in Selected Watershed"),
                          downloadButton(outputId = "dl_wr_info"),
                          headerPanel(""),
                          DTOutput(outputId = "wr_info_table")
                          )
                          ),
                 
                 tabPanel(title = "Demand Data",
                          h4("Demands in Selected Scenario(s)"),
                          DTOutput(outputId = "demand_table")
                          )
                 )
               )
             )
    ), #Close Tab Panel 1                     
    tabPanel(title = "Sac-SJ-Delta Watershed" #Tab Panel 2
             
             ), #Close Tab Panel 2
    tabPanel(title = "About This Dashboard" #Tab Panel 3
             
             ) #Close Tab Panel 2
  )
)

## SERVER ----------------------------------------------------------------------
server <- function(input, output, session) {

  # Watch plot type radio buttons.
  observe({
    if(input$plot_type != "was") {
      hide(id = "supply_selected")
      hide(id = "priority_selected")
    } else {
      show(id = "supply_selected")
      show(id = "priority_selected")
    }
  })
  
  # Demand scenario filter handler.
  scenario_selected <- reactive({ input$scenario_selected })
  
  # Grab demand and wr_info for selected HUC-8.
  ws_demand <- reactive({
    req(input$huc8_selected)
    demand[[input$huc8_selected]]
  })
  ws_wr_info <- reactive({
    req(input$huc8_selected)
    filter(wr_info, huc8_name %in% input$huc8_selected)
  })
  
  # Update scenario choices.
  observeEvent(input$huc8_selected, {
    choices <- sort(na.omit(unique(ws_demand()$scenario)))
    updateSelectizeInput(session, "scenario_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2019")
  })
  
  # Grab demand and wr_info for selected demand scenario(s).
  scenario_demand <- reactive({
    req(input$scenario_selected)
    filter(ws_demand(), scenario %in% input$scenario_selected)
  })
  
   scenario_wr_info <- reactive({
    req(input$scenario_selected)
    filter(ws_wr_info(), wr_id %in% scenario_demand()$wr_id)
  })
  
  # Select priority year to slice.
  observeEvent(input$scenario_selected, {
    req(input$huc8_selected)
    choices <- sort(na.omit(unique(scenario_demand()$p_year)), 
                    decreasing = TRUE)
    updateSelectInput(session, "priority_selected",
                      choices = choices,
                      selected = max(scenario_demand()$p_year, na.rm = TRUE))
  })
  
  ## Summarize demand data for plot. ----
  
  plot_demand <- reactive({
    scenario_demand() %>% 
      #    req(input$priority_selected)
      #   filter(huc8_name %in% input$huc8_selected) %>% 
      mutate(fill_color = if_else(priority == "Statement Demand",
                                  "Statement Demand",
                                  if_else(priority == "Statement Demand",
                                          "Statement Demand",
                                          if_else(p_year >= input$priority_selected,
                                                  "Junior Post-14", "Post-14"))),
             fill_color = ordered(fill_color, levels = wa_demand_order)) %>% 
      group_by(scenario, rept_date, fill_color) %>% 
      summarise(demand_daily_af = sum(demand_daily_af, na.rm = TRUE),
                demand_daily_cfs = sum(demand_daily_cfs, na.rm = TRUE),
                .groups = "drop")
  })
  
  plot_height <- reactive({
    500 * length(scenario_selected())
  })
  
  ## Render the plot. ----
  output$demand_plot <- renderPlot({
  
      ggplot(
      data = plot_demand(),
      aes(x = rept_date,
          y = demand_daily_af,
          group = fill_color,
          fill = fill_color)) +
      geom_area(position = "stack") +
      scale_x_date(date_labels = "%m/%d/%Y",
                   date_minor_breaks = "1 month") +
      scale_fill_manual(name = "Demand type:",
                        values = wa_demand_pal,
                        labels = c(paste(input$priority_selected, 
                                         "& Junior Post-14 Demand"),
                                   "Senior Post-14 Demand",
                                   "Statement Demand")) +
      labs(y = "Acre-Feet/Day") +
      theme_bw() +
      theme(
        legend.position = "bottom",
        strip.text.x = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        axis.title.x = element_blank()) +
      facet_wrap(facets = vars(scenario),
                 nrow = length(unique(plot_demand()$scenario))
      )
    
  }, height = function() plot_height())
  
  
  ## Render selected watershed map. ----
  
  pod_points <- reactive({
    pods %>%
      filter(huc8_name %in% input$huc8_selected)
  })
  
  ws_poly <- reactive({
    huc8_layer %>% filter(huc8_name %in% input$huc8_selected)
  })
  
  output$mini_map <- renderLeaflet({
    validate(
      need(nrow(ws_poly()) > 0, 
           paste0("No Data Available.\n",
                  "Please select another watershed."))
    )
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = ws_poly(),
                  weight = 2,
                  col = "blue",
                  fill = TRUE,
                  fillOpacity = 0,
                  label = ws_poly()$huc8_name,
                  labelOptions = labelOptions(textsize = "12px",
                                              sticky = TRUE)) %>%
      addCircleMarkers(data = pod_points(),
                       radius = 3,
                       fillOpacity = 0.7,
                       stroke = TRUE,
                       color = ~wr_type_pal(wr_type),
                       weight = 1,
                       fillColor = ~wr_type_pal(wr_type),
                       label = pod_points()$wr_id) %>%
      addLegend(position = "topright",
                pal = wr_type_pal,
                values = pod_points()$wr_type,
                title = "Water Right Type",
                opacity = 1)
  })
  
  ## Water Rights Info Table. ----
  
  output$wr_info_table <- renderDT({
    datatable(data = ws_wr_info(),
              options = list(pageLength = 20, 
                             lengthMenu = c(10, 20, 100)),
              filter = "top",
              rownames = FALSE)
  })
  
  output$dl_wr_info <- downloadHandler(
    filename = paste0(input$huc8_selected, "_", "wr_info_", Sys.Date(), ".csv"),
    content = function(file) {
      data <- ws_wr_info()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  ## Demand Table. ----
  
  output$demand_table <- renderDT({
    scenario_demand() %>% 
      select(-p_year)
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



## APP -------------------------------------------------------------------------
shinyApp(ui, server)
