library(shiny)
library(shinythemes)
library(shinyjs)
library(shinipsum)
library(sf)
library(leaflet)
library(dplyr)
library(wesanderson)
library(ggplot2)
library(aws.s3)
library(DT)

## Initialize values. ---

load_from_s3 <- FALSE

## Load data files. ----

if(load_from_s3) {
  # Water Right Info.
  s3load(object = "dwast-wrinfo.RData",
         bucket = "dwr-enf-shiny")
  
  # Demand Data.
  s3load(object = "dwast-demands.RData",
         bucket = "dwr-enf-shiny")
} else {
  # Water Right Info.
  load("./output/dwast-wrinfo-2020-12-18.RData")
  
  # Demand Data.
  load("./output/dwast-demands-2020-12-15.RData")
}

## Define color palettes and plot order. ----

# Water right type.
wr_type_pal <-colorFactor(c(wes_palette("Darjeeling1"), 
                            wes_palette("Darjeeling2"))[1:9],
                          pods$wr_type)

# Water availability demand.
wa_demand_order <- ordered(
  c("Junior Post-14",
    "Post-14",
    "Statement Demand",
    "Environmental Demand")
)
wa_demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(wa_demand_pal) <- wa_demand_order

## watershed

huc8_selected <- "Trinity"

ws_demand <- demand[[huc8_selected]]
ws_wr_info <- filter(wr_info, huc8_name %in% huc8_selected)

## scenario

scenario_selected <- "Reported Diversions - 2019"

scenario_demand <- filter(ws_demand, scenario %in% scenario_selected)















