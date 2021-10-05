## Load Libraries. ----

if (!("package:shiny" %in% search())) {
  suppressMessages(library(shiny))
}
if (!("package:shinythemes" %in% search())) {
  suppressMessages(library(shinythemes))
}
if (!("package:DT" %in% search())) {
  suppressMessages(library(DT))
}
if (!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if (!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if (!("package:plotly" %in% search())) {
  suppressMessages(library(plotly))
}
if (!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}

## Load data file. ----
load("./data/div_screening_data.RData")

## Define variables/constants. ----

# List of water right types.
all_types <- sort(unique(divs_by_huc$wr_type))

# Plot color palette.
my_palette <- c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))
names(my_palette) <- all_types



## UI --------------------------------------------------------------------------
ui <- fluidPage(
  # Set theme.
  theme = shinytheme("cerulean"),

  # App Title
  h1("Reported Diversions Exploration and Visualization Tool"),
  h3("Under Development", style = "color:red"),

  tabsetPanel(
    tabPanel(title = "Plot",
             sidebarLayout(
               sidebarPanel(
                 h4("Input Selections"),
                 wellPanel(
                   # Select Report Year.
                   sliderInput(inputId = "rep_year",
                               label = "Select Reporting Year",
                               min = 2009,
                               max = max(divs_by_huc$rep_year),
                               step = 1,
                               value = sample(c(2009:max(divs_by_huc$rep_year) -1), 1),
                               sep = "",
                               ticks = TRUE,
                               animate = animationOptions(interval = 3000,
                                                          loop = TRUE
                                                          ))
                 ),
                 wellPanel(

                   # Select Region.
                   selectInput(inputId = "region_selected",
                               label = "Select Region:",
                               choices = "-- Under Construction --",
                               selected = "-- Under Construction --",
                               multiple = TRUE),

                   # Select Watershed(s)
                   selectInput(inputId = "ws_selected",
                               label = "Select Watershed(s):",
                               choices = unique(divs_by_huc$watershed),
                               selected = sample(unique(divs_by_huc$watershed), 1),
                               multiple = TRUE),

                   # Select/deselect HUC 8 watersheds.
                   selectInput(inputId = "huc8_selected",
                               label = "Select HUC8 Watershed(s):",
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE)
                 ),
                 wellPanel(
                   # Select Water Right types.
                   checkboxGroupInput(inputId = "wrt_selected",
                                      label = "Select Water Right Type(s) to Include:",
                                      choices = all_types,
                                      selected = all_types)

                 ),

                 # Logos
                 br(),

                 # DWR-ENF Logo
                 # HTML('<center><img src="DWR-ENF-Logo.png" width="150"></center>'),

                 # Built with Shiny by RStudio
                 br(),
                 HTML('<center><p>Built with</p>
                      <p><img src="shiny.png", height = "50">
                      and <img src="RStudio.png", height = "50">
                      by <img src="jgy_hex.png", height = "50"></p></center>'),

                 # set sidebarpanel relative width.
                 width = 3
               ),
               mainPanel(
                 br(),
                 br(),
                 # Plot Output.
                 plotlyOutput("bar_plot", height = "600px")
               )
             )
    ),

    # Water Rights Tab.
    tabPanel(title = "Water Rights",
             fluidRow(
               column(10,
                      h2("Water Rights Included In Plot")
               )
             ),
             fluidRow(
               column(1,
                      downloadButton("download_rights", "Download CSV")
               )
             ),
             fluidRow(
               column(1,
                      br()
               )
             ),
             fluidRow(
               column(12,
                      dataTableOutput(outputId = "wr_table")
               )
             )
    ),

    # Diversions Tab.
    tabPanel(title = "Diversions",
             fluidRow(
               column(10,
                      h2("Reported Diversions Included In Plot")
               )
             ),
             fluidRow(
               column(1,
                      downloadButton("download_diversions", "Download CSV")
               )
             ),
             fluidRow(
               column(1,
                      br()
               )
             ),
             fluidRow(
               column(12,
                      dataTableOutput(outputId = "divs_table")
               )
             )
    ),

    # Codebook and Disclaimers Tab.
    tabPanel(title = "Codebook & Disclaimers",
             h2("Under Construction"))
  )
)

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {

  plot_year <- reactive({ input$rep_year })

  selected_rights <- reactive({ input$wrt_selected })

  ws_selected <- reactive({
    filter(divs_by_huc, watershed %in% input$ws_selected)
  })

  observeEvent(ws_selected(), {
    choices <- unique(ws_selected()$watershed_huc8)
    updateSelectInput(session, "huc8_selected", choices = choices,
                      selected = choices)
  })

  # Filter Water Rights table.
  wr_table_data <- reactive({
    filter(wr_info,
           watershed %in% input$ws_selected,
           watershed_huc8 %in% input$huc8_selected,
           wr_type %in% input$wrt_selected)
  })

  # Render Water Rights table.
  output$wr_table <- renderDataTable({
    req(input$wrt_selected)
    datatable(data = wr_table_data(),
              options = list(pageLength = 20),
              rownames = FALSE)
  })

  # Filter Diversions table.
  div_table_data <- reactive({
    filter(divs_by_wr,
           rep_year == input$rep_year,
           watershed %in% input$ws_selected,
           watershed_huc8 %in% input$huc8_selected,
           wr_type %in% input$wrt_selected) %>%
      mutate(rep_month = as.numeric(rep_month))
  })

  # Render Diversions table.
  output$divs_table <- renderDataTable({
    req(input$huc8_selected, input$wrt_selected)
    datatable(data = div_table_data(),
              options = list(pageLength = 20),
              rownames = FALSE)
  })

  # Filter plot data table.
  plot_data <- reactive({
    filter(divs_by_wr,
           rep_year == input$rep_year,
           watershed %in% input$ws_selected,
           watershed_huc8 %in% input$huc8_selected,
           wr_type %in% input$wrt_selected)
  })

  # Download water rights table.
  output$download_rights <- downloadHandler(
    filename = function() {
      paste0("water_right_list_", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv(wr_table_data(), file, row.names = FALSE)
    }
  )

  # Download diversions table.
  output$download_diversions <- downloadHandler(
    filename = function() {
      paste0("diversions_list_", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv(div_table_data(), file, row.names = FALSE)
    }
  )

  # Render bar plot.
  output$bar_plot <- renderPlotly({
    req(input$huc8_selected, input$wrt_selected)

    ggplotly({

      plot_data <- plot_data()

      plot_data <- plot_data %>%
        group_by(wr_type, rep_month) %>%
        summarize(diverted = sum(diverted, na.rm = TRUE),
                  .groups = "drop")

      ggplot(plot_data, aes(x = rep_month, y = diverted, fill = wr_type)) +
        geom_col() +

        scale_fill_manual(values = my_palette) +

        # Customize plot look.
        # theme(axis.title = element_text(size = rel(1.2)),
        #       axis.text = element_text(size = rel(1.2)),
        #       legend.text = element_text(size = rel(1.2)),
        #       legend.title = element_text(size = rel(1.2))) +

        # Plot Labels:
        labs(title = paste0("Reported Diversions in Acre-Feet for the ",
                            plot_year(),
                            " Reporting Year"),
             x = "Month",
             y = "Reported Amount Diverted (Acre Feet)",
             fill = "Water Right Type")
    })
  })

}

# Run the application.
shinyApp(ui = ui, server = server)
