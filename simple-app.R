library(shiny)
library(shinyjs)
library(shinythemes)
library(htmltools)

app_title <- paste("DWR-WaS/DET: Division of Water Rights",
                   "Water Supply/Demand Exploration Tool")

# Main Tabs. ----
## Explore. ----

ui <- fluidPage(
  useShinyjs(),
  # Set theme.
  theme = shinytheme("cerulean"),
  
 
  
  # Title bar.
  titlePanel(windowTitle = app_title,
             title =
               div(
                 img(
                   src = "waterboards_logo_high_res.jpg",
                   height = 50,
                   
               
                 ),
                 app_title
               )
  ),
  
  navbarPage(title = NULL,
   
    # 
    # 
    # 
    # # Title.
    # title = div(img(src = "DWR-ENF-Logo-2048.png",
    #                 height = 45,
    #                 width = 45,
    #                 style = "position: relative;
    #               margin:-12px 0px;
    #               display:right-align;"),
    #             paste("DWR-WaSDET: Division of Water Rights",
    #                   "Water Supply/Demand Exploration Tool")),
    # # tags$head(
    # #   tags$style(HTML('.navbar {
    # #                      height: 55px !important;}'))
    # # ),
    
    
    
    tabPanel("Explore",
             icon = icon("wpexplorer"),
             "Explore Content Goes Here..."),
    
    navbarMenu("Dataset Information",
               icon = icon("table"),
               tabPanel("Demand Scenarios",
                        icon = icon("faucet"),
                        "Demand Scenarios", br(),
                        "Content Goes Here",br(),br(),
                        includeMarkdown("explore/temp_data_descrip.md")),
               
               tabPanel("Supply Scenarios",
                        icon = icon("water"),
                        "Supply Scenarios", br(),
                        "Content Goes Here"),
               tabPanel("Other Data",
                        icon = icon("table"),
                        "Other Data", br(),
                        "Content Goes Here")
    ),
    
    
    navbarMenu("About/Help",
               icon = icon("info-circle"),
               tabPanel("About",
                        icon = icon("info-circle"),
                        includeMarkdown("docs/ABOUT.md")),
               tabPanel("How To Use The Filters",
                        icon = icon("life-ring"),
                        "How To Use The Filters", br(),
                        "Content Goes Here"),
               tabPanel("FAQ",
                        icon = icon("question"),
                        "Frequently Asked Question", br(),
                        "Content Goes Here"),
               
               
               tabPanel("Report Bugs/Data Issues",
                        icon = icon("bug"),
                        "link to GitHub repository,",br(),
                        "data maintainer contacts?")
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
