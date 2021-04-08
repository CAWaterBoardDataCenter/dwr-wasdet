library(shiny)
require(forecast)

name = c(2,4,3,8,6,12,4,16)
name <- ts(name, frequency=12, start=c(2007,1))

ui <- fluidPage(
  numericInput(inputId = "n2", "Size of training set", value = 12),
  radioButtons(inputId = "n3", "Forecast type",  c("Holt Winter's" = "HW", "ARIMA" = "arima")),
  plotOutput(outputId = "graph"), 
  plotOutput(outputId = "graph2")
)

server <- function(input, output) { 
  
  output$graph <- renderPlot({
    plot(name, type = "l")
  })
  
  output$graph2 <- renderPlot({
    if (input$n3 == "HW") { 
      fit <- ets (name, model = "ZAZ", damped=TRUE)
      plot(forecast(fit,level=c(80,80,99), h = input$n2), main = "Forecast, HW")
    }
    
    if (input$n3 == "arima") { 
      fit <- auto.arima (name)
      plot(forecast(fit, 12), main = "Forecast, ARIMA")
    }
  }) 
}

shinyApp(ui = ui, server = server)
