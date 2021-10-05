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

## Initialize values. ---

# Data source.
load_from_s3 <- ifelse(Sys.info()["nodename"] == "Home-iMac.local", FALSE, TRUE)

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
wa_supply_pal <- colorRampPalette(wes_palette("Rushmore")[3:4])(3)
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
                   
                   # Select Visualization to show.
                   radioButtons(inputId = "plot_type_selected",
                                label = "Select Visualization:",
                                choices = c(
                                  "View Supply-Demand Scenarios" = "vsd",
                                  "View Demand By Priority (Placeholder)" = "vbp",
                                  "View Demand By Water Right Type (Placeholder)" = "vbwrt"),
                                selected = "vsd"
                   ),
                   
                   # Select HUC-8 watershed.
                   selectInput(inputId = "huc8_selected",
                               label = "Select HUC-8 Watershed:",
                               choices = NULL,
                               selected = NULL,
                               multiple = FALSE
                   ),
                   p("When 'View Supply-Demand Scenarios' is selected, only those watersheds with available Supply Scenarios are selectable."),
                   br(),
                   
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
                               multiple = FALSE),
                   
                   ## My logos !!
                   br(),br(),br(),
                   HTML('<center><p>Built with</p>
                      <p><img src="shiny.png", height = "50">
                      and <img src="RStudio.png", height = "50">
                      by <img src="jgy_hex.png", height = "50"></p></center>')
                   
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
  s_scene_selected <- reactive ({ input$s_scene_selected })
  priority_selected <- reactive({ input$priority_selected })
  plot_type_selected <- reactive({ input$plot_type_selected })
  
  # Define plot height function to keep facet panels roghly the same height
  # wheter displaying one or two.
  plot_height <- reactive({
    ifelse(length(d_scene_selected()) == 1, 480,
           ifelse(length(d_scene_selected()) == 2, 835, "auto"))
           
  })
  
  ## TESTING ELEMENTS. ----
  
  #  observeEvent(input$s_scene_selected, {
  #   # print(paste0("class: ", class(input$priority_selected)))
  #    print(paste0("value: ", input$s_scene_selected))
  # })
  
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
  
  # if plot_type is vsd, filter available HUC8s to those with corresponding
  # supply data.  sort(names(demand))
  observeEvent(input$plot_type_selected, {
    req(plot_type_selected)
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
    choices <- sort(unique(demand_selected()$d_scenario))
    updateSelectizeInput(session, 
                         inputId = "d_scene_selected",
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
                         selected = c("Historic: Estimated Mean Unimpaired Flow at AMA, Wet Year",
                                      "Historic: Estimated Mean Unimpaired Flow at AMA, Critical Year"))
  })
  
  # Update priority year choices.
  py_choice_list <- reactive({
    sort(na.omit(unique(demand_selected()$p_year)), decreasing = TRUE)
  })
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- py_choice_list()[py_choice_list() > min(py_choice_list())]
    updateSelectInput(session, "priority_selected",
                      choices = choices,
                     # selected = max(choices),
                      selected = 1958)
  })
  
  ## Build datasets. ----
  
  # Supply exploration dataset. This is where the magic happens.
  vsd_plot_data <- reactive({
    bind_rows(
      {
        # Demand.
        # Demand.
        filter(demand_selected(), 
               d_scenario %in% d_scene_selected()) %>%
          mutate(fill_color = if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(priority == "Statement Demand",
                                              "Statement Demand",
                                              if_else(p_year >= priority_selected(),
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
        filter(supply, huc8_name %in% huc8_selected(),
               s_scenario %in% s_scene_selected()) %>%
          mutate(source = "old",
                 fill_color = NA,
                 plot_group = "supply") %>%
          full_join(.,
                    as_tibble(d_scene_selected()),
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
                          labels = c(paste(priority_selected(), 
                                           "& Junior Post-14 Demand"),
                                     paste(as.numeric(priority_selected()) -1,
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
        labs(title = "Plot Title Placeholder",
             y = "Cubic Feet per Second (cfs)") +
        
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








