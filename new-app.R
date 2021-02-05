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


##### DEBUG #####
if(!("package:reactlog" %in% search())) {
  suppressMessages(library(reactlog))
}
reactlog_enable()


### UI -------------------------------------------------------------------------

ui <- navbarPage(
  title = div(img(src = "DWR-ENF-Logo.png",
                  style = "position: relative; margin:-15px 0px; display:right-align;"),
              "Water Supply/Demand Exploration Tool"),
  useShinyjs(),
  
  # Set theme.
  theme = shinytheme("cerulean"),
  
  ## Main Tabs. ----
  
  # --> Explore tab (plots & tables).
  tabPanel("Explore",
           fluidRow(
             sidebarLayout(
               
               # Sidebar Panel.
               sidebarPanel(width = 3,
                            
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
                      
               ),
               
               # Main panel.
               mainPanel(width = 9,
                         #fluidRow(
                         column(width = 7,
                                
                                # Plot/Data Tabs.
                                tabsetPanel(
                                  
                                  # Plot tabs.
                                  tabPanel("Plots",
                                           
                                           tabsetPanel(type = "pills",
                                                       
                                                       # Supply-Demand scenario Ppot.
                                                       tabPanel("Supply-Demand Scenarios",
                                                                fluidRow(
                                                                  plotOutput(outputId = "random_plot")
                                                                )
                                                       ),
                                                       
                                                       # Demand by Water right type plot.
                                                       tabPanel("Demand by Water Right Type",
                                                                fluidRow(
                                                                  plotOutput(outputId = "random_plot1")
                                                                )
                                                       ),
                                                       
                                                       # Demand by priority plot.
                                                       tabPanel("Demand by Priority",
                                                                fluidRow(
                                                                  plotOutput(outputId = "random_plot2")
                                                                )
                                                       )
                                           )
                                  ),
                                  
                                  # Data Tabs.
                                  tabPanel("Data"),
                                  
                                  # California Watershed Map.
                                  tabPanel("California Watershed Map")
                                  
                                )      
                         ),
                         
                         column(width = 4,
                                h4("Column 2"),
                                imageOutput(outputId = "random_image")
                                
                         )
               )
             )
           )
  ), # end fluid row
  
  # --> About tab (how to use, data sources, etc.)
  tabPanel("About",
           h4("Main Tab 1"),
           #   plotOutput(outputId = "random_plot")
  ),
  selected = "Explore"
  
)




### SERVER ---------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Random plot placeholder.
  output$random_plot <- renderPlot({
    random_ggplot()
  })
  
  # Random plot placeholder.
  output$random_plot1 <- renderPlot({
    random_ggplot()
  })
  
  # Random plot placeholder.
  output$random_plot2 <- renderPlot({
    random_ggplot()
  })
  
  # Random image placeholder.
  output$random_image <- renderImage({
    random_image()
  }, deleteFile = FALSE)
  
}


### APP ------------------------------------------------------------------------

shinyApp(ui, server)













































