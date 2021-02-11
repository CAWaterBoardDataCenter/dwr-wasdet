# Initialization -------------------------------------------------------------

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
if(!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if(!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}
if(!("package:sf" %in% search())) {
  suppressMessages(library(sf))
}
if(!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}
if(!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if(!("package:spdplyr" %in% search())) {
  suppressMessages(library(spdplyr))
}
if(!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}
if(!("package:scales" %in% search())) {
  suppressMessages(library(scales))
}
if(!("package:DT" %in% search())) {
  suppressMessages(library(DT))
}


## Debug #####
if(!("package:reactlog" %in% search())) {
  suppressMessages(library(reactlog))
}
reactlog_enable()

## Initialize values. ----

# Data source.
# load_from_s3 <- ifelse(Sys.info()["nodename"] == "Home-iMac.local", FALSE, TRUE)

## Load data files. ----

# Water Right Info.
load("./output/dwast-wrinfo.RData")

# Demand data.
ifelse(Sys.info()["nodename"] == "Home-iMac.local", 
       load("./explore/dwast-demands-test-set.RData"), 
       load("./output/dwast-demands.RData"))


# Supply data.
load("./output/dwast-supplies.RData")


## Define color and shape aesthetics. ----

# Demand.
wa_demand_order <- ordered(c("Junior Post-14",
                             "Post-14",
                             "Statement Demand",
                             "Environmental Demand"))
wa_demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(wa_demand_pal) <- wa_demand_order

# Water right type.
plot_wrt_pal <- c(wes_palette("Darjeeling1"), 
                  wes_palette("Darjeeling2"))[2:10]
names(plot_wrt_pal) <- sort(unique(wr_info$wr_type))
map_wrt_pal <- colorFactor(palette = plot_wrt_pal, 
                           domain = wr_info$wr_type)

# Priority.
priority_pal <- c(viridis_pal()(length(c(year(now()):1914))), "gray", "black")
names(priority_pal) <- c(c(year(now()):1914), "Statement Demand", "Environmental Demand")

# Supply.
wa_supply_pal <- colorRampPalette(wes_palette("Rushmore")[3:4])(3)
wa_supply_shapes <- c(15, 16, 17)


# UI ------------------------------------------------------------------------

ui <- navbarPage(
  useShinyjs(),
  
  # Title.
  title = div(img(src = "DWR-ENF-Logo.png",
                  style = "position: relative; margin:-15px 0px; display:right-align;"),
              "Water Supply/Demand Exploration Tool"),
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
  h2("Under Development. Do not rely on data used in this dashboard until it is officially released.", 
     style = "color:red"),
  
  ## Main Tabs.
  ## Explore. ----
  tabPanel("Explore",
           
           fluidRow(
             sidebarLayout(
               
               ### Sidebar Panel. ----
               sidebarPanel(width = 2,
                            
                            # Select units to display.
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
                            
                            # Select HUC-8 watershed.
                            selectInput(inputId = "huc8_selected",
                                        label = "Select HUC-8 Watershed:",
                                        choices = sort(names(demand)),
                                        selected = "North Fork American",
                                        multiple = FALSE
                            ),
                            
                            # Filter for watersheds with supply information.
                            checkboxInput(inputId = "supply_filter",
                                          label = "Filter for watersheds with available supply information",
                                          value = FALSE
                            ),
                            
                            # Select demand scenario(s).
                            selectizeInput(inputId = "d_scene_selected",
                                           label = "Select Up To Two Demand Scenarios:",
                                           choices = NULL,
                                           selected = NULL,
                                           multiple = TRUE,
                                           options = list(maxItems = 2)
                            ),
                            wellPanel(
                              # Select supply scenario(s) for vsd_plot.
                              selectizeInput(inputId = "s_scene_selected",
                                             label = "Select Up To Three Supply Scenarios:",
                                             choices = NULL,
                                             selected = NULL,
                                             multiple = TRUE,
                                             options = list(maxItems = 3)
                              ),
                              
                              # Select priority year to slice for vsd_plot.
                              selectInput(inputId = "priority_selected",
                                          label = "Select Demand Priority Year to Slice:",
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = FALSE),
                              
                              # Select water right types to include in dbwrt_plot.
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
                      by <img src="jgy_hex.png", height = "50"></p></center>')
               ),
               
               ### Main Panel. ----
               mainPanel(width = 10,
                         
                         # Plot/Data/Watershed map Tabs.
                         tabsetPanel(type = "pills",
                                     
                                     #### Plot tabs. ----
                                     tabPanel("Plots",
                                              fluidRow(
                                                
                                                # Plot column.
                                                column(width = 7,
                                                       tabsetPanel(id = "plot_tabs",
                                                                   type = "pills",
                                                                   
                                                                   # Supply-Demand Plot.
                                                                   tabPanel(title = "Supply-Demand Scenarios",
                                                                            id = "vsd_tab",
                                                                            fluidRow(
                                                                              plotOutput(outputId = "vsd_plot")
                                                                            )
                                                                   ),
                                                                   
                                                                   # Demand by Water right type plot.
                                                                   tabPanel(title = "Demand by Water Right Type",
                                                                            id = "dbwrt_tab",
                                                                            fluidRow(
                                                                              br(),
                                                                              plotOutput(outputId = "dbwrt_plot")
                                                                            )
                                                                   ),
                                                                   
                                                                   # Demand by priority plot.
                                                                   tabPanel(title = "Demand by Priority",
                                                                            id = "dbp_tab",
                                                                            fluidRow(
                                                                              br(),
                                                                              plotOutput(outputId = "dbp_plot")
                                                                            )
                                                                   )
                                                       )      
                                                ),
                                                
                                                # Mini-map column
                                                column(width = 5,
                                                       fluidRow(
                                                         h4("Watershed Location and PODs"),
                                                         leafletOutput(outputId = "mini_map",
                                                                       height = "500px"),
                                                         br(),br(),
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
  ), # end fluid row
  
  ## About. ----
  tabPanel("About",
           tabPanel("About",
                    h2("What"),
                    h2("Why"),
                    h2("How"),
                    br(),br(),
                    h4("Data source information"),
                    h4("Dashboard Curator Information")
           )
  ),
  selected = "Explore"
)




# SERVER --------------------------------------------------------------------

server <- function(input, output, session) {
  
  ### Debug. ----
  
  output$debug_text <- renderText({paste0("You are viewing tab \"", input$plot_tabs, "\"")})
  
  ## Setup. ----
  
  # Disable units selector until implemented
  disable(id = "units_selected")
  disable(id = "supply_filter")
  
  ## Helper Functions. ----
  
  # Define plot height function to keep facet panels roughly the same height
  # whether displaying one or two.
  plot_height <- reactive({
    ifelse(length(input$d_scene_selected) == 1, 480,
           ifelse(length(input$d_scene_selected) == 2, 835, "auto"))
    
  })
  
  ## Observers. ----
  
  # Update demand scenario choices.
  observeEvent(input$huc8_selected, {
    choices <- sort(unique(demand[[input$huc8_selected]]$d_scenario))
    updateSelectizeInput(session, 
                         inputId = "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2019")
  })
  
  # Update supply scenario choices.
  observeEvent(input$huc8_selected, {
    choices <- sort(unique(filter(supply, 
                                  huc8_name %in% input$huc8_selected)$s_scenario))
    updateSelectizeInput(session, 
                         inputId = "s_scene_selected",
                         choices = choices,
                         selected = c("Historic: Estimated Mean Unimpaired Flow at AMA, Wet Year",
                                      "Historic: Estimated Mean Unimpaired Flow at AMA, Critical Year"))
  })
  
  # Update priority year choices.
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
  
  # Update water right type choices.
  observeEvent(input$huc8_selected, {
    choices <- unique(demand[[input$huc8_selected]]$wr_type)
    updateCheckboxGroupInput(session = session, 
                             inputId = "wrt_selected",
                             choices = choices,
                             selected = choices)
  })
  
 
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
  
  ### Outputs. ----
  
  #### Supply-Demand Scenario plot (vsd). ----
  
  ###### Build dataset. ----
  ## This is where the magic happens.
  vsd_plot_data <- reactive({
    bind_rows(
      {
        
        # Demand.
        filter(demand[[input$huc8_selected]], 
               d_scenario %in% input$d_scene_selected) %>%
          mutate(fill_color = if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(priority == "Statement Demand",
                                              "Statement Demand",
                                              if_else(p_year >= input$priority_selected,
                                                      "Junior Post-14", "Post-14"))),
                 fill_color = ordered(fill_color, levels = wa_demand_order)) %>%
          group_by(d_scenario, plot_date, fill_color) %>%
          summarise(af_monthly = sum(af_monthly, na.rm = TRUE),
                    af_daily = sum(af_daily, na.rm = TRUE),
                    cfs = sum(cfs, na.rm = TRUE),
                    .groups = "drop") %>% 
          mutate(plot_group = "demand",
                 s_scenario = NA) %>%
          select(d_scenario, 
                 s_scenario, 
                 plot_date,
                 fill_color, 
                 af_monthly,
                 af_daily, 
                 cfs, 
                 plot_group) %>% 
          
          # Add boundary points to facilitate barplot visualization and correct stacking.
          bind_rows(old = .,
                    new = mutate(., 
                                 plot_date = ceiling_date(x = plot_date,
                                                          unit = "month") - 1),
                    .id = "source") %>% 
          arrange(plot_date, source)
      }, 
      {
        
        # Supply.
        filter(supply, huc8_name %in% input$huc8_selected,
               s_scenario %in% input$s_scene_selected) %>%
          mutate(source = "old",
                 fill_color = NA,
                 plot_group = "supply") %>%
          full_join(.,
                    as_tibble(input$d_scene_selected),
                    by = character()) %>%
          select(source,
                 d_scenario = value,
                 s_scenario,
                 plot_date,
                 fill_color,
                 af_monthly,
                 af_daily,
                 cfs,
                 plot_group)
      }
    )
  })
  
  ##### Render plot. ----
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
      geom_area(data = subset(vsd_plot_data(), plot_group == "demand"),
                position = "stack",
                aes(fill = fill_color)) +
      
      # Supply.
      geom_point(data = subset(vsd_plot_data(), plot_group == "supply"),
                 aes(color = s_scenario,
                     shape = s_scenario),
                 size = 7) +
      geom_line(data = subset(vsd_plot_data(), plot_group == "supply"),
                aes(color = s_scenario),
                linetype = "dashed") +
      
      # X axis format.
      scale_x_date(date_labels = "%m/%d/%y",
                   date_minor_breaks = "1 month") +
      
      # Y axis format.
      scale_y_continuous(labels = comma) +
      
      # Demand legend.
      scale_fill_manual(name = "Demand Type:",
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
  
  #### Demand By Water Right Type Plot (dbwrt). ----
  
  ##### Build dataset. ----
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
  
  ##### Render plot. ----
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
        #  panel.spacing = unit(2, "lines"),
        axis.title.x = element_blank())
    
  }, height = function() plot_height()
  )
  
  #### Demand by Priority plot (dbp). ----
  
  ##### Build dataset. ----
  
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
  
  ##### Render plot. ----
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
      guides(fill = guide_legend(ncol = 2)) +
      
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
        #  legend.position = "bottom",
        # legend.box = "horizontal",
        #  legend.direction = "horizontal",
        #  panel.spacing = unit(2, "lines"),
        axis.title.x = element_blank())
    
  }, height = function() plot_height()
  )
  
  #### Mini Map. ----
  
  # Filter POD points.
  pod_points <- reactive({
    pods %>% 
      filter(huc8_name %in% input$huc8_selected)
  })
  
  # Filter watershed polygon.
  plot_poly <- reactive({
    huc8_layer %>% filter(huc8_name %in% input$huc8_selected)
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
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = plot_poly(),
                  weight = 3,
                  col = "blue",
                  fill = TRUE,
                  fillOpacity = 0,
                  label = plot_poly()$huc8_name,
                  labelOptions = labelOptions(textsize = "12px",
                                              sticky = TRUE))
  })
  
  # Color POD points to match plot legend categories they fall under.
  
  observe({
    fill_color <- ifelse(input$plot_tabs == "Supply-Demand Scenarios", "red",
                         ifelse(input$plot_tabs == "Demand by Water Right Type", "orange", "black"))
    
    leafletProxy(mapId = "mini_map", 
                 data = pod_points()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = 3,
                 fillOpacity = 0.8,
                 stroke = FALSE,
                 weight = 2,
                 fillColor = fill_color, 
                 label = pod_points()$wr_id
      )
  })
 
  #### Tables. ----
  
  ## Demand data table.
  output$demand_data_table <- renderDataTable({
    demand[[input$huc8_selected]] %>% 
      filter(d_scenario %in% input$d_scene_selected,
             wr_type %in% input$wrt_selected) %>% 
      mutate(plot_date = month(plot_date)) %>% 
      rename(month = plot_date) %>% 
      select(-p_year)
  },
  filter = "top",
  rownames = FALSE)
  
} # End Server


# APP -----------------------------------------------------------------------

shinyApp(ui = ui,
         server = server)













































