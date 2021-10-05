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


##### DEBUG #####
if(!("package:reactlog" %in% search())) {
  suppressMessages(library(reactlog))
}
reactlog_enable()


### UI -------------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  
)


### SERVER ---------------------------------------------------------------------

server <- function(input, output, session) {
  
}


### APP ------------------------------------------------------------------------

shinyApp(ui, server)











































