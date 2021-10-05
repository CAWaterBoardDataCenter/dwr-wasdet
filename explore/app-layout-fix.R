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
if(!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if(!("package:DT" %in% search())) {
  suppressMessages(library(DT))
}
if(!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}


#### UI ------------------------------------------------------------------------

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
  
  ## Main Tabs. ----
  
  # --> Explore tab (plots & tables).
  tabPanel("Explore",
           fluidRow(
             sidebarLayout(
               
               # SIDEBAR PANEL.
               sidebarPanel(width = 3,
                            
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
               
               # MAIN PANEL.##############################
               mainPanel(width = 9,
                         
                         # Plots.
                         
                         
                         # Plot/Data Tabs.
                         tabsetPanel(type = "pills",
                                     
                                     # Plot tabs.
                                     tabPanel("Plots",
                                              fluidRow(
                                                
                                                # Plot column.
                                                column(width = 7,
                                                       tabsetPanel(type = "pills",
                                                                   
                                                                   # Supply-Demand scenario Plot.
                                                                   tabPanel("Supply-Demand Scenarios",
                                                                            fluidRow(
                                                                              br(),
                                                                              plotOutput(outputId = "vsd_plot")
                                                                            )
                                                                   ),
                                                                   
                                                                   # Demand by Water right type plot.
                                                                   tabPanel("Demand by Water Right Type",
                                                                            fluidRow(
                                                                              br(),
                                                                              plotOutput(outputId = "dbwrt_plot")
                                                                            )
                                                                   ),
                                                                   
                                                                   # Demand by priority plot.
                                                                   tabPanel("Demand by Priority",
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
                                                         br(),
                                                         h4("Watershed Location and PODs"),
                                                         leafletOutput(outputId = "mini_map",
                                                                       height = "500px")
                                                       )
                                                )
                                                
                                                
                                              )
                                     ),    
                                     
                                     # Data Tabs.
                                     tabPanel("Data",
                                              DTOutput(outputId = "pod_points_result")
                                     ),
                                     
                                     # California Watershed Map.
                                     tabPanel("California Watershed Map",
                                     )
                                     
                         )      
                         
               )
             ) ##############################################################
           )
  ), # end fluid row
  
  # --> About tab (how to use, data sources, etc.)
  tabPanel("About",
           h2("What"),
           h2("Why"),
           h2("How"),
           br(),br(),
           h4("Data source information"),
           h4("Dashboard Curator Information")
  ),
  selected = "Explore"
  
)




#### SERVER --------------------------------------------------------------------

server <- function(input, output, session) {
  
  ### Control Input Selections.
  
  # Disable units selector until implemented
  disable(id = "units_selected")
  
  ### Update elements. ----
  
  
  
  ### OUTPUT. ----
  
  ## Supply-Demand Scenario plot.
  output$vsd_plot <- renderPlot({
    random_ggplot(type = "point")
  })
  
  ## Demand By Water Right Type Plot.
  output$dbwrt_plot <- renderPlot({
    random_ggplot(type = "point")
  })
  
  # Random plot placeholder.
  output$dbp_plot <- renderPlot({
    random_ggplot(type = "point")
  })
  
  ## Mini Map.
  
  output$mini_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  output$pod_points_result <- renderDT({
    random_DT(nrow = 10, 
              ncol = 4)
  })
  
}


#### APP -----------------------------------------------------------------------

shinyApp(ui, server)














































