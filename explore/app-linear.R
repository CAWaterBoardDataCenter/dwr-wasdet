# Initialization -------------------------------------------------------------

# Load libraries. ----
if (!("package:aws.s3" %in% search())) {
  suppressMessages(library(aws.s3))
}
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

# Debug. #####
if (Sys.info()["nodename"] == "Home-iMac.local") {
  if (!("package:reactlog" %in% search())) {
    suppressMessages(library(reactlog))
  }
  reactlogReset()
  reactlog_enable()
}

# Initialization. ----

# Application title.
app_title <- paste("DWR-WaS/DET: Division of Water Rights",
                   "Water Supply/Demand Exploration Tool")

# Load data from AWS S3 bucket. ----

## Load S3 keys. ----
source("load-s3-keys.R")

## Load Water Right Info. ----
s3load(object = "wasdet-wrinfo-develop.RData",
       bucket = "dwr-shiny-apps")

## Load Demand Data. ----
## Load smaller test set if on local machine for testing.
if (Sys.info()["nodename"] == "Home-iMac.local") {
  s3load(object = "wasdet-demands-test-set-develop.RData", bucket = "dwr-shiny-apps")
  # s3load(object = "wasdet-demands-develop.RData", bucket = "dwr-shiny-apps")
} else {
  s3load(object = "wasdet-demands-develop.RData", bucket = "dwr-shiny-apps")
}

## Load Supply data. ----
s3load(object = "wasdet-supplies-develop.RData",
       bucket = "dwr-shiny-apps")

# Load local data. ----

## Gage station location information. ----
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

# Historical and Forecast supply.
wa_supply_pal <- colorRampPalette(wes_palette("Rushmore")[3:4])(3)
wa_supply_shapes <- c(15, 16, 17)

# Current-year supply.
cy_supply_pal <- "blue"

# VSD Plot theme.
vsd_plot_theme <-  theme(
  legend.box = "vertical",
  legend.direction = "horizontal",
  # strip.text.x = element_text(size = rel(1.5)),
  # axis.title = element_text(size = rel(1.2)),
  # axis.text = element_text(size = rel(1.2)),
  # legend.text = element_text(size = rel(1.2)),
  # legend.title = element_text(size = rel(1.2)),
  axis.title.x = element_blank()
)

# Mini map icons.
station_icon <- icons(iconUrl = "./www/x-diamond-fill.svg")

# Icon legend for vsd plot.
html_legend <-
  "<img src='circle-fill.svg' style='width:8px;height:8px;'> Point of Diversion<br/>
<img src='x-diamond-fill.svg'> Gage Station"

# UI ---------------------------------------------------------------------------
# 
# ui <- fluidPage( # Start fluidpage_1
#   useShinyjs(),
#   
#   # Set theme.
#   theme = shinytheme("cerulean"),
#   
#   ## Title bar. ----
#   titlePanel(windowTitle = app_title,
#              title =
#                div(
#                  img(
#                    src = "DWR-ENF-Logo-2048.png",
#                    height = 50,
#                  ),
#                  app_title
#                )
#   ),
#   
#   # # Prototype Warning.
#   # p(paste("Under Development. Do not rely on data used in this dashboard",
#   #          "until it is officially released."), 
#   #    style = "color:red"),
#   
#   ## Main Tabs. ----
#   navbarPage(title = NULL,
#              ### Explore. ----
#              tabPanel("Explore",
#                       
#                       fluidRow(
#                         sidebarLayout(
#                           
#                           #### Sidebar Panel. ----
#                           sidebarPanel(width = 2,
#                                        
#                                        ##### Mandatory inputs. ----
#                                        
#                                        ###### Select units to display. ----
#                                        radioButtons(inputId = "units_selected",
#                                                     label = "Select Units:",
#                                                     choiceNames = list(
#                                                       "Cubic Feet per Second (cfs)",
#                                                       "Acre-Feet per Day (AF/d)"
#                                                     ),
#                                                     choiceValues = list(
#                                                       "cfs",
#                                                       "afd"
#                                                     ),
#                                                     selected = "cfs"
#                                        ),
#                                        
#                                        ###### Select HUC-8 watershed. ----
#                                        selectInput(inputId = "huc8_selected",
#                                                    label = "Select HUC-8 Watershed:",
#                                                    choices = NULL,
#                                                    selected = NULL,
#                                                    multiple = FALSE
#                                        ),
#                                        
#                                        ###### Filter for watersheds with supply information. ----
#                                        checkboxInput(inputId = "supply_filter",
#                                                      label = "Filter for watersheds with available supply information",
#                                                      value = FALSE
#                                        ),
#                                        
#                                        ###### Select demand scenario(s). ----
#                                        selectizeInput(inputId = "d_scene_selected",
#                                                       label = "Select Up To Two Demand Scenarios:",
#                                                       choices = NULL,
#                                                       selected = NULL,
#                                                       multiple = TRUE,
#                                                       options = list(maxItems = 2)
#                                        ),
#                                        
#                                        ##### Conditional Inputs. ----
#                                        
#                                        wellPanel(
#                                          
#                                          ###### Select supply scenario(s) for vsd_plot. ----
#                                          selectizeInput(inputId = "s_scene_selected",
#                                                         label = "Select Up To Three Supply Scenarios:",
#                                                         choices = NULL,
#                                                         selected = NULL,
#                                                         multiple = TRUE,
#                                                         options = list(maxItems = 3)
#                                          ),
#                                          
#                                          ###### Select priority year to slice for vsd_plot. ----
#                                          selectInput(inputId = "priority_selected",
#                                                      label = "Select Demand Priority Year:",
#                                                      choices = NULL,
#                                                      selected = NULL,
#                                                      multiple = FALSE),
#                                          
#                                          ###### Select water right types to include in dbwrt_plot. ----
#                                          checkboxGroupInput(inputId = "wrt_selected",
#                                                             label = "Select Water Right Type(s) to Display:",
#                                                             choices = NULL,
#                                                             selected = NULL)
#                                        ),
#                                        
#                                        ##### Copyright. ----
#                                        HTML('<center><img src="waterboards_logo_high_res.jpg", width = "75%"></center>'),
#                                        HTML(paste("<center>©", year(now()), 
#                                                   "State Water Resources Control Board</center>"))
#                           ),
#                           
#                           #### Main Panel. ----
#                           mainPanel(width = 10,
#                                     
#                                     # Plot/Data/Watershed map Tabs.
#                                     tabsetPanel(type = "pills",
#                                                 
#                                                 ##### Plot tabs. ----
#                                                 tabPanel("Plots",
#                                                          fluidRow(
#                                                            
#                                                            # Plot column.
#                                                            column(width = 7,
#                                                                   tabsetPanel(id = "plot_tabs",
#                                                                               type = "pills",
#                                                                               
#                                                                               ###### Supply-Demand plot tab. ----
#                                                                               tabPanel(title = "Supply-Demand Scenarios",
#                                                                                        id = "vsd_tab",
#                                                                                        fluidRow(
#                                                                                          plotOutput(outputId = "vsd_plot")
#                                                                                        )
#                                                                               ),
#                                                                               
#                                                                               ###### Demand by Water right type plot tab. ----
#                                                                               tabPanel(title = "Demand by Water Right Type",
#                                                                                        id = "dbwrt_tab",
#                                                                                        fluidRow(
#                                                                                          br(),
#                                                                                          plotOutput(outputId = "dbwrt_plot")
#                                                                                        )
#                                                                               ),
#                                                                               
#                                                                               ###### Demand by priority plot tab. ----
#                                                                               tabPanel(title = "Demand by Priority",
#                                                                                        id = "dbp_tab",
#                                                                                        fluidRow(
#                                                                                          br(),
#                                                                                          plotOutput(outputId = "dbp_plot")
#                                                                                        )
#                                                                               )
#                                                                   )      
#                                                            ),
#                                                            
#                                                            ##### Mini map column. ----
#                                                            
#                                                            ###### Mini map. ----
#                                                            column(width = 5,
#                                                                   fluidRow(
#                                                                     h4("Watershed Location and PODs"),
#                                                                     leafletOutput(outputId = "mini_map",
#                                                                                   height = "500px",
#                                                                                   width = "95%"),
#                                                                     br(),
#                                                                     
#                                                                     ###### Debug notes. ----
#                                                                     h3("Debug"),
#                                                                     textOutput("debug_text")
#                                                                   )
#                                                            )
#                                                          )
#                                                 ),    
#                                                 
#                                                 ##### Data tab. ----
#                                                 tabPanel("Data",
#                                                          br(),
#                                                          h3("Selected Demand Data"),
#                                                          DTOutput(outputId = "demand_data_table")
#                                                 ),
#                                                 
#                                                 ##### California Watershed Map tab. ----
#                                                 tabPanel("California Watershed Map",
#                                                          br(),
#                                                          h3("Coming soon...")
#                                                 )
#                                     )      
#                           )
#                         )
#                       )
#              ), 
#              
#              ### Dataset Information. ----
#              navbarMenu("Dataset Information",
#                         icon = icon("table"),
#                         tabPanel("Demand Scenarios",
#                                  icon = icon("faucet"),
#                                  "Demand Scenarios", br(),
#                                  "Content Goes Here",br(),br(),
#                                  includeMarkdown("docs/temp_data_descrip.md")),
#                         
#                         tabPanel("Supply Scenarios",
#                                  icon = icon("water"),
#                                  "Supply Scenarios", br(),
#                                  "Content Goes Here"),
#                         tabPanel("Other Data",
#                                  icon = icon("table"),
#                                  "Other Data", br(),
#                                  "Content Goes Here")
#              ),
#              
#              ### About/Help. ----
#              navbarMenu("About/Help",
#                         icon = icon("info-circle"),
#                         tabPanel("About",
#                                  icon = icon("info-circle"),
#                                  includeMarkdown("docs/ABOUT.md")),
#                         tabPanel("How To Use The Filters",
#                                  icon = icon("life-ring"),
#                                  "How To Use The Filters", br(),
#                                  "Content Goes Here"),
#                         tabPanel("FAQ",
#                                  icon = icon("question"),
#                                  "Frequently Asked Question", br(),
#                                  "Content Goes Here"),
#                         
#                         
#                         tabPanel("Report Bugs/Data Issues",
#                                  icon = icon("bug"),
#                                  "link to GitHub repository,",br(),
#                                  "data maintainer contacts?")
#              )
#   ),
#   selected = "Explore"
# ) # end fluidPage_1
# 
# # SERVER -----------------------------------------------------------------------
# 
# server <- function(input, output, session) {
#   
#   # Debug. ----
#   
#   output$debug_text <- renderText({ paste0("DEBUG:",
#                                            "\ninput$huc8_selected is: ", 
#                                            input$huc8_selected,
#                                            "\ninput$s_scenario_selected is: ", 
#                                            input$s_scenario_selected[1])
#   })
#   
#   # Setup. ----
#   
#   # Disable units selector until implemented
#   disable(id = "units_selected")
#   #  disable(id = "supply_filter")
  
  # Helper Functions. ----
  
  # Define plot height function to keep facet panels roughly the same height
  # whether displaying one or two.
  plot_height <- 
    ifelse(length(input_d_scene_selected) == 1, 480,
           ifelse(length(input_d_scene_selected) == 2, 835, "auto"))
  
  
  # # OBSERVERS. ----
  # 
  # ## Control input selectors. ----
  # observe({
  #   if (input$plot_tabs == "Supply-Demand Scenarios") {
  #     showElement(id = "s_scene_selected")
  #     showElement(id = "priority_selected")
  #     hideElement(id = "wrt_selected")
  #   } else
  #     if (input$plot_tabs == "Demand by Water Right Type") {
  #       hideElement(id = "s_scene_selected")
  #       hideElement(id = "priority_selected")
  #       showElement(id = "wrt_selected")
  #     } else
  #       if (input$plot_tabs == "Demand by Priority") {
  #         hideElement(id = "s_scene_selected")
  #         hideElement(id = "priority_selected")
  #         showElement(id = "wrt_selected")
  #       }
  # })
  
  ## Filter for watersheds that have supply data. ----
  # observeEvent(input$supply_filter, {
    if (input_supply_filter) { 
      # choices <- sort(names(demand))
      choices <- sort(names(demand)[names(demand) %in% names(supply)])
    } else { 
      choices <- sort(names(demand))
    }
  #   updateSelectInput(session,
  #                     inputId = "huc8_selected",
  #                     choices = choices,
  #                     selected = "Upper San Joaquin")
  # })
  
  ## Update demand scenario choices. ----
  # observeEvent(input$huc8_selected, {
    choices <- sort(unique(demand[[input_huc8_selected]]$d_scenario))
  #   updateSelectizeInput(session, 
  #                        inputId = "d_scene_selected",
  #                        choices = choices,
  #                        selected = "Reported Diversions - 2019")
  # })
  ## XXXXXXXXXXXXX Finished to here





  ## Update supply scenario choices. ----
  # observeEvent(input$huc8_selected, {
    choices <- sort(unique(supply[[input_huc8_selected]]$s_scenario))
  #   updateSelectizeInput(session, 
  #                        inputId = "s_scene_selected",
  #                        choices = choices,
  #                        selected = NULL)
  # })
  
  ## Update priority year choices. ----
  py_choice_list <- # reactive({
    sort(na.omit(unique(demand[[input_huc8_selected]]$p_year)), decreasing = TRUE)
  # })
  # observeEvent(input$d_scene_selected, {
    choices <- py_choice_list[py_choice_list > min(py_choice_list)]
  #   updateSelectInput(session, "priority_selected",
  #                     choices = choices,
  #                     # selected = max(choices),
  #                     selected = sample(choices, 1))
  # })
  
  ## Update water right type choices. ----
  # observeEvent(input$huc8_selected, {
    choices <- unique(demand[[input_huc8_selected]]$wr_type)
  #   updateCheckboxGroupInput(session = session, 
  #                            inputId = "wrt_selected",
  #                            choices = choices,
  #                            selected = choices)
  # })
  
  # OUTPUTS. ----
  
  ## Supply-Demand Scenario plot (vsd). ----
  
  ### Build dataset. ----
  ## This is where the magic happens.
  vsd_plot_data <- # reactive({
    bind_rows(
      {
        # Demand.
        filter(demand[[input_huc8_selected]], 
               d_scenario %in% input_d_scene_selected) %>%
          mutate(fill_color = if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(priority == "Statement Demand",
                                              "Statement Demand",
                                              if_else(p_year >= input_priority_selected,
                                                      "Junior Post-14", "Post-14"))),
                 fill_color = ordered(fill_color, levels = wa_demand_order)) %>%
          group_by(d_scenario, plot_date, fill_color, plot_category) %>%
          summarise(af_monthly = sum(af_monthly, na.rm = TRUE),
                    af_daily = sum(af_daily, na.rm = TRUE),
                    cfs = sum(cfs, na.rm = TRUE),
                    .groups = "drop") %>% 
          mutate(s_scenario = NA) %>%
          select(d_scenario, 
                 s_scenario, 
                 plot_date,
                 fill_color, 
                 af_monthly,
                 af_daily, 
                 cfs, 
                 plot_category) %>% 
          # Add boundary points to facilitate barplot vis and correct stacking.
          bind_rows(old = .,
                    new = mutate(., 
                                 plot_date = ceiling_date(x = plot_date,
                                                          unit = "month") - 1),
                    .id = "source") %>% 
          arrange(plot_date, source)
      }, 
      {
        # Supply.
        filter(supply[[input_huc8_selected]],
               s_scenario %in% input_s_scene_selected) %>%
          mutate(source = "old",
                 fill_color = NA,
                 plot_category = "supply") %>%
          full_join(.,
                    as_tibble(input_d_scene_selected),
                    by = character()) %>%
          select(source,
                 d_scenario = value,
                 s_scenario,
                 plot_date,
                 fill_color,
                 af_monthly,
                 af_daily,
                 cfs,
                 plot_category)
      }
    )
  # })
  
  ### Render plot. ----
  output$vsd_plot <- renderPlot({
    
    # Validate.
    validate(
      need(input$d_scene_selected, 
           "No data to plot.\nPlease slelect at least one Demand Scenario.")
    )
    
    # Render.
    ggplot(data = vsd_plot_data(),
           aes(x = plot_date,
               y = cfs)) +
      
      # Demand.
      geom_area(data = subset(vsd_plot_data(), plot_category == "demand"),
                position = "stack",
                aes(fill = fill_color)) +
      
      # Supply.
      geom_point(data = subset(vsd_plot_data(), plot_category == "supply"),
                 aes(color = s_scenario,
                     shape = s_scenario),
                 size = 7) +
      geom_line(data = subset(vsd_plot_data(), plot_category == "supply"),
                aes(color = s_scenario),
                linetype = "dashed") +
      
      # X axis format.
      scale_x_date(date_labels = "%b %d",
                   date_minor_breaks = "1 month") +
      
      # Y axis format.
      scale_y_continuous(labels = comma) +
      
      # Demand legend.
      scale_fill_manual(name = "Demand Priority:",
                        values = wa_demand_pal,
                        labels = c(paste(input$priority_selected, 
                                         "& Junior Post-14 Demand"),
                                   paste(as.numeric(input$priority_selected) -1,
                                         "& Senior Post-14 Demand"),
                                   "Statement Demand")) +
      
      # Supply legend.
      scale_shape_manual(name = "Supply Scenario:",
                         values = wa_supply_shapes) +
      scale_color_manual(name = "Supply Scenario:",
                         values = wa_supply_pal) +
      
      # Facet on demand scenario.
      facet_wrap(~ d_scenario, 
                 ncol = 1,
                 scales = "free_x") +
      
      # Labels.
      labs(y = "Cubic Feet per Second (cfs)") +
      
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
        axis.title.x = element_blank()
      )
    
  }, height = function() plot_height())
  
  ## Demand By Water Right Type Plot (dbwrt). ----
  
  ### Build dataset. ----
  # Demand by water right type dataset.
  wrt_plot_data <- reactive({ 
    demand[[input$huc8_selected]] %>% 
      filter(d_scenario %in% input$d_scene_selected,
             wr_type %in% input$wrt_selected) %>% 
      group_by(d_scenario, 
               wr_type, 
               plot_month = month(plot_date, label = TRUE)) %>%
      summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
                af_daily = sum(af_daily, na.rm = TRUE),
                cfs = sum(cfs, na.rm = TRUE),
                .groups = "drop")
  })
  
  ### Render plot. ----
  output$dbwrt_plot <- renderPlot({
    
    # Validate.
    validate(
      need(input$d_scene_selected, 
           "No data to plot.\nPlease slelect at least one Demand Scenario."),
      need(input$huc8_selected, 
           "No data to plot.\nPlease slelect a Watershed."),
      need(nrow(wrt_plot_data()) > 0,
           "No data for selected water right types.")
    )
    
    # Render.
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
                        values = plot_wrt_pal) +
      
      # labels
      labs(title = "Monthly Demand by Water Right Type",
           y = "Cubic Feet per Second (cfs)") +
      
      # Facet on demand scenario.
      facet_wrap(~ d_scenario, 
                 ncol = 1,
                 scales = "free_x") +
      
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
        axis.title.x = element_blank())
    
  }, height = function() plot_height()
  )
  
  ## Demand by Priority plot (dbp). ----
  
  ### Build dataset. ----
  
  # Demand by priority dataset.
  dbp_plot_data <- reactive({
    demand[[input$huc8_selected]] %>% 
      filter(d_scenario %in% input$d_scene_selected,
             wr_type %in% input$wrt_selected) %>% 
      group_by(d_scenario, 
               priority, 
               plot_month = month(plot_date, label = TRUE)) %>%
      summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
                af_daily = sum(af_daily, na.rm = TRUE),
                cfs = sum(cfs, na.rm = TRUE),
                .groups = "drop") %>% 
      mutate(priority = ordered(priority, levels = names(priority_pal)))
  })
  
  ### Render plot. ----
  output$dbp_plot <- renderPlot({
    
    # Render.
    ggplot(data = dbp_plot_data(),
           aes(x = plot_month,
               y = cfs,
               fill = priority)) +
      
      # Demand bars.
      geom_col() + 
      
      # Y axis format.
      scale_y_continuous(labels = comma) +
      
      # Legend.
      scale_fill_manual(name = "Priority:",
                        values = priority_pal) +
      guides(fill = guide_legend(ncol = 3)) +
      
      # labels
      labs(title = "Monthly Demand by Priority",
           y = "Cubic Feet per Second (cfs)") +
      
      # Facet on demand scenario.
      facet_wrap(~ d_scenario, 
                 ncol = 1,
                 scales = "free_x") +
      
      # Theme.
      theme_minimal() +
      theme(
        plot.title = element_text(size = rel(2.0)),
        strip.text.x = element_text(size = rel(2.0)),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        axis.title.x = element_blank())
    
  }, height = function() plot_height()
  )
  
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
  
  # Filter gage stations.
  station_points <- reactive({
    station_locs %>% 
      filter(huc8_name %in% input$huc8_selected)
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
      addPolygons(group = "base",
                  data = plot_poly(),
                  weight = 3,
                  col = "blue",
                  fill = TRUE,
                  fillOpacity = 0)
  })
  
  ### Update POD points and legends. ----
  observe({
    
    pod_points <- pod_points()
    station_points <- station_points()
    
    # Create POD label look-up.
    pod_labs <- lapply(seq(nrow(pod_points)), function(i) {
      paste0('Water Right ID: ', st_drop_geometry(pod_points[i, "wr_id"]), '<br>',
             'Owner: ', st_drop_geometry(pod_points[i, "owner"]), '<br>',
             'Water Right Type: ', st_drop_geometry(pod_points[i, "wr_type"]), '<br>',
             'Priority: ' , st_drop_geometry(pod_points[i, "priority"]), '<br>',
             'Status: ', st_drop_geometry(pod_points[i, "wr_status"])
      )
    })
    
    station_labs <- lapply(seq(nrow(station_points)), function(i) {
      paste0('<b>Station: </b>', station_points[i, "station_id"], '<br>',
             '<b>Name: </b>', station_points[i, "station_name"], '<br>',
             '<b>Data Provider: </b>', station_points[i, "data_provider"], '<br>',
             '<b>', station_points[i, "link"], '</b>')
    })
    
    #### vsd plot points. ----
    if( input$plot_tabs == "Supply-Demand Scenarios" ) {
      
      leafletProxy(mapId = "mini_map") %>%
        clearGroup(group = "content") %>% 
        clearControls() %>%
        
        # POD points.
        addCircleMarkers(group = "content",
                         data = pod_points,
                         radius = 4,
                         fillOpacity = 0.8,
                         stroke = FALSE,
                         weight = 2,
                         fillColor = ~map_demand_pal(vsd_fill_color),
                         label = lapply(pod_labs, HTML)) %>%
        
        # Gage station markers.
        addMarkers(group = "content",
                   data = station_points(),
                   lat = ~lat,
                   lng = ~lng,
                   icon = station_icon,
                   popup = lapply(station_labs, HTML)) %>% 
        
        # Color legend.
        addLegend(position = "topright",
                  colors = wa_demand_pal[1:3],
                  labels = c(paste(input$priority_selected, "& Junior Post-14 Demand"),
                             paste(as.numeric(input$priority_selected) -1, "& Senior Post-14 Demand"),
                             "Statement Demand"),
                  title = "Demand Priority",
                  opacity = 1) %>% 
        
        # Shape legend.
        addControl(html = html_legend,
                   position = "bottomleft")
      
    } else
      
      ### dbwrt plot points. ----
    if( input$plot_tabs == "Demand by Water Right Type" ) {
      
      leafletProxy(mapId = "mini_map") %>%
        clearGroup(group = "content") %>% 
        clearControls() %>%
        addCircleMarkers(group = "content",
                         data = pod_points,
                         radius = 4,
                         fillOpacity = 0.8,
                         stroke = FALSE,
                         weight = 2,
                         fillColor = ~map_wrt_pal(wr_type), 
                         label = lapply(pod_labs, HTML)
        ) %>% 
        addLegend(position = "topright",
                  pal = map_wrt_pal,
                  values = pod_points$wr_type,
                  title = "Water Right Type",
                  opacity = 1)
    } else
      
      ### vbp plot points. ----
    if( input$plot_tabs == "Demand by Priority" ) {
      
      leafletProxy(mapId = "mini_map") %>%
        clearGroup(group = "content") %>% 
        clearControls() %>%
        addCircleMarkers(group = "content",
                         data = pod_points,
                         radius = 4,
                         fillOpacity = 0.9,
                         stroke = FALSE,
                         #  weight = 2,
                         fillColor = ~map_priority_pal(priority), 
                         label = lapply(pod_labs, HTML)
        ) %>% 
        addControl(html = "See plot legend for color categories.",
                   position = "topright")
    }
    
  })
  
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













































