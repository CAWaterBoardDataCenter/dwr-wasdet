library("shiny")

ui <- basicPage(
  textOutput("text"),
  tabsetPanel(id = "tabs",
              tabPanel(
                "Signal", id = "signal", plotOutput("signalplot", height=1000)
              ),
              tabPanel("low", id = "low", plotOutput("looseplot", height=1000)),
              tabPanel("medium", id = "medium", plotOutput("mediumplot", height=1000)),
              tabPanel("tight", id = "tight", plotOutput("tightplot", height=1000))
  )
)

server <- function(input, output, session) {
  
  output$signalplot<-renderPlot({
    plot(c(6,7,8),c(5,7,5))
  })
  output$looseplot<-renderPlot({
    plot(c(7,7,8),c(5,0,5))
  })
  
  output$text <- renderText({paste0("You are viewing tab \"", input$tabs, "\"")})
}

shinyApp(ui, server)
