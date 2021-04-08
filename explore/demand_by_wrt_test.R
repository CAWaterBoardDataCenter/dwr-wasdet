library(shiny)
library(shinythemes)
library(shinyjs)
library(shinipsum)
library(sf)
library(leaflet)
library(dplyr)
library(broom)
library(wesanderson)
library(ggplot2)
library(ggforce)
library(aws.s3)
library(DT)

load_from_s3 <- FALSE

## Selections. ----

# HUC-8 watershed.
huc8_selected <- "North Fork American"

# Demand scenario.
#d_scene_selected <- "Reported Diversions - 2019"
d_scene_selected <- c("Reported Diversions - 2019", "Reported Diversions - 2011")

# Supply scenario.
#s_scene_selected <- "Historic: Estimated Mean Unimpaired Flow at AMA, Below Normal Year"
s_scene_selected <- c("Historic: Estimated Mean Unimpaired Flow at AMA, Below Normal Year",
                      "Historic: Estimated Mean Unimpaired Flow at AMA, Dry Year")

# Priority cutoff.
priority_selected <- 1950

## Load data files. ----

if(load_from_s3) {
  # Water Right Info.
  s3load(object = "dwast-wrinfo.RData",
         bucket = "dwr-enf-shiny")
  
  # Demand Data.
  s3load(object = "dwast-demands.RData",
         bucket = "dwr-enf-shiny")
  
  # Supply data.
  s3load(object = "dwast-supplies.RData",
         bucket = "dwr-enf-shiny")
} else {
  # Water Right Info.
  load("./output/dwast-wrinfo.RData")
  
  # Demand data.
  load("./output/dwast-demands-test-set.RData")
  
  # Supply data.
  load("./output/dwast-supplies.RData")
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

# Supply.
wa_supply_pal <- colorRampPalette(wes_palette("Zissou1")[1:2])(3)
wa_supply_shapes <- c(15, 16, 17)

## watershed

ws_demand <- demand[[huc8_selected]]

ws_wr_info <- filter(wr_info, huc8_name %in% huc8_selected)

## scenario

scenario_demand <- filter(ws_demand, d_scenario %in% d_scene_selected)


## filter

vbwrt_plot_data <- demand[[huc8_selected]] %>%
  #       group_by(wr_type, rep_month) %>%
  #       summarize(diverted = sum(diverted, na.rm = TRUE),
  #                 .groups = "drop")


ggplot(scenario_demand, aes(x = plot_date,
                            y = cfs)) +
  #       geom_col() +
  #       
  #       scale_fill_manual(values = my_palette)


# # Filter plot data table.
# vbwrt_plot_data <- #reactive({
#   filter(divs_by_wr,
#          rep_year == input$rep_year,
#          watershed %in% input$ws_selected,
#          watershed_huc8 %in% input$huc8_selected,
#          wr_type %in% input$wrt_selected)
# #})



# # Render bar plot.
# output$bar_plot <- renderPlotly({
#   req(input$huc8_selected, input$wrt_selected)
#   
#   ggplotly({
#     
#     plot_data <- plot_data()
#     
#     plot_data <- plot_data %>%
#       group_by(wr_type, rep_month) %>%
#       summarize(diverted = sum(diverted, na.rm = TRUE),
#                 .groups = "drop")
#     
#     ggplot(plot_data, aes(x = rep_month, y = diverted, fill = wr_type)) +
#       geom_col() +
#       
#       scale_fill_manual(values = my_palette) +
#       
#       # Customize plot look.
#       # theme(axis.title = element_text(size = rel(1.2)),
#       #       axis.text = element_text(size = rel(1.2)),
#       #       legend.text = element_text(size = rel(1.2)),
#       #       legend.title = element_text(size = rel(1.2))) +
#       
#       # Plot Labels:
#       labs(title = paste0("Reported Diversions in Acre-Feet for the ",
#                           plot_year(),
#                           " Reporting Year"),
#            x = "Month",
#            y = "Reported Amount Diverted (Acre Feet)",
#            fill = "Water Right Type")
#   })
# })
#


