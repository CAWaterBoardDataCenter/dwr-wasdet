library(shiny)
library(shinyjs)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# Constants.

plot_area_enabled <- FALSE
wy_type_enabled <- FALSE


y_max <- 32000

# Load data file.

load("./data/wrdds_2016.RData")
load("./data/approp_plot_df.RData")

all_areas <- c("Sacramento", "San Joaquin")
priority_years <- sort(unique(approp_plot_df$variable), decreasing = TRUE)

# Just consider 1914 and junior appropriative rights for now.
priority_years <- priority_years[priority_years >= 1914]

# Consider comparing demands at different water year types?
water_year_types <- c(paste0("Previous Year (", year(now()) -1, ")"), 
                      "Wet", "Normal", "Dry")

# Define color palette.
plot_variables <- c("Appropriative", "Pre-14", "Riparian",
                    "16_05_fnf.50", "16_05_fnf.90", "16_05_fnf.99", "daily_fnf")
my_colors <- c("#D55E00", "#E69f00", "#F0E442", "green", "orange", "red", "blue")
names(my_colors) <- levels(plot_variables)

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  h1("Water Availability Screening Tool"),
  h2("Proof of Concept Prototype"),
  sidebarLayout(
    
    # Inputs.
    sidebarPanel(
      h4("Plot Parameters"),
      
      # Watershed area.
      selectInput(inputId = "watershed",
                  label = "Select Watershed",
                  choices = all_areas,
                  selected = "San Joaquin"),
      
      # Priority line.
      sliderInput(inputId = "priority_line",
                  label = "Select Post-14 Priority Year Line to Display",
                  min = 1914,
                  max = max(priority_years),
                  value = 1927,
                  sep = ""),
      
      # Water Year Type.
      selectInput(inputId = "wy_type",
                  label = "Select Water Year Type",
                  choices = water_year_types,
                  selected = "Normal"),
      
      # Toggle Supply Forecast.
      checkboxInput(inputId = "show_fnf_forecast",
                    label = "Show Supply Forecast",
                    value = FALSE),
      
      # Toggle Actual Supply.
      checkboxInput(inputId = "show_fnf_actual",
                    label = "Show Actual Supply",
                    value = FALSE),
      
      # Built with Shiny by RStudio
      br(),
      br(),
      h5(
        "Built with",
        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "60px"),
        "and",
        img(src = "RStudio.png", height = "60px"),
        "by",
        img(src = "jgy_hex.png", height = "60px"),
        "."
      )
    ),
    
    # Main panel:
    mainPanel(
      
      # Plot
      plotOutput(outputId = "main_plot",
                 height = "600px"),
      
      # Description
      h5(textOutput("description"))
      
    )
  )
)   

# Define server logic
server <- function(input, output) {
  
  # Disable Watershed and Water Year type inputs (not developed).
  #  disable("wy_type")
  disable("watershed")
  
  # Reactive appropriative priority line.
  app_line <- reactive(geom_line(data = subset(approp_plot_df, variable == input$priority_line),
                                 size = 1.1, linetype = 2))
  
  # FNF Forecast points.
  forecast_points <- reactive({
    if(input$show_fnf_forecast) {
      geom_point(data = subset(plot_df,
                               variable %in% c("16_05_fnf.50",
                                               "16_05_fnf.90",
                                               "16_05_fnf.99") &
                                 area == input$watershed),
                 aes_string(color = "variable"), size = 4)
    } else {
      geom_blank()
    }
  })
  
  # FNF Forecast lines.
  forecast_lines <- reactive({
    if(input$show_fnf_forecast) {
      geom_line(data = subset(plot_df,
                              variable %in% c("16_05_fnf.50",
                                              "16_05_fnf.90",
                                              "16_05_fnf.99") &
                                area == input$watershed),
                aes_string(color = "variable"),
                linetype = 2,
                alpha = 0.3)
    } else {
      geom_blank()
    }
  })
  
  # FNF Forecast legend.
  forecast_legend <- reactive({
    if(input$show_fnf_forecast) {
      scale_color_manual(name="Forecasted Supplies",
                         breaks = c("16_05_fnf.50",
                                    "16_05_fnf.90",
                                    "16_05_fnf.99",
                                    "daily_fnf"),
                         labels=c("Adjusted 50% Exceedance Monthly FNF Forecast",
                                  "Adjusted 90% Exceedance Monthly FNF Forecast",
                                  "Adjusted 99% Exceedance Monthly FNF Forecast",
                                  "Actual Daily FNF"),
                         values = my_colors[4:7])
    } else {
      geom_blank()
    }
  })
  
  # FNF Forecast guides.
  forecast_guides <- reactive({
    if(input$show_fnf_forecast) {
      guides(color = guide_legend(order = 1, override.aes =
                                    list(shape=c(16, 16, 16),
                                         linetype=c(0, 0, 0))),
             shape = guide_legend(order = 2))
    } else {
      geom_blank()
    }
  })
  
  # Actual daily FNF line.
  actual_daily_line <- reactive({
    if(input$show_fnf_actual) {
      geom_line(data = subset(plot_df,
                              area == input$watershed &
                                variable == "daily_fnf"),
                aes(color = variable))
    } else {
      geom_blank()
    }
  })
  
  # Actual daily FNF legend
  actual_daily_legend <- reactive({
    if(input$show_fnf_actual) {
      scale_color_manual(name="Actual Supplies",
                         breaks = "daily_fnf",
                         labels = "Actual Supply",
                         values = my_colors[7])
    } else {
      geom_blank()
    }
  })
  
  # Actual Daily guides.
  actual_daily_guides <- reactive({
    if(input$show_fnf_actual) {
      guides(color = guide_legend(order = 1, override.aes =
                                    list(shape = 16,
                                         linetype = 0)),
             shape = guide_legend(order = 2))
    } else {
      geom_blank()
    }
  })
  
  # Create plot.
  output$main_plot <- renderPlot({
    ggplot(data = plot_df, aes_string(x = "date", y = "value")) +
      
      # Define axis formatting:
      scale_y_continuous(limits = c(0, y_max),
                         labels = comma) +
      scale_x_date(date_breaks = "1 month", date_labels = "%m/%d/%y", minor_breaks = NULL) +
      
      # Set plot area.
      coord_cartesian(xlim = c(as.Date("2016-03-01"), as.Date("2016-09-30"))) +
      
      # Demand bar data.
      geom_area(data = subset(plot_df, variable %in% c("Riparian", "Pre-14", "Appropriative") & area == input$watershed),
                position = "stack",
                aes_string(fill = "variable")) +
      
      forecast_points() + 
      
      forecast_lines() +
      
      actual_daily_line() +
      
      # Demand Legend.
      scale_fill_manual(name = "Demand", 
                        breaks = c("Appropriative", "Pre-14", "Riparian"),
                        labels = c("Post-1914 Demand", "Pre-1914 Demand", "Riparian Demand"),
                        values = my_colors) +
      
      forecast_legend() +
      
      forecast_guides() +
      
  #    actual_daily_legend() +
      
   #   actual_daily_guides() +
      
      # Customize plot look.
      theme(axis.title = element_text(size = rel(1.2)),
            axis.text = element_text(size = rel(1.2)),
            legend.justification = c(1, 1),
            legend.position = c(1, 1),
            legend.box.just = "right",
            legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.2))) + 
      
      # Axis Labels:
      labs(x = "", y = "Time-Averaged Flow in Cubic Feet per Second (cfs)") +
      
      # Appropriative priority line.
      app_line()
  })
  
  # Create description of plot
  output$description <- renderText({
    paste("The area above the black line represents demand for the",
          input$priority_line,
          "and junior years.")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



