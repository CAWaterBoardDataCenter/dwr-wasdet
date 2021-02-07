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

## Load data files. ----

# wr_info data.
load("./output/dwast-wrinfo.RData")

# Demand data.
load("./output/dwast-demands.RData")

## Define color and shape aesthetics. ----





































