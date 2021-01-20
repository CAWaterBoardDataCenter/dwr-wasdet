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


load_from_s3 <- TRUE

priority_selected <- 1950
huc8_selected <- "Trinity"
# scenario_selected <- "Reported Diversions - 2019"
scenario_selected <- c("Reported Diversions - 2019", "Reported Diversions - 2011")

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
  load("./output/dwast-demands.RData")
  
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

## watershed

ws_demand <- demand[[huc8_selected]]
ws_wr_info <- filter(wr_info, huc8_name %in% huc8_selected)

## scenario

scenario_demand <- filter(ws_demand, scenario %in% scenario_selected)


## Munge scenario demand for plot.

plot_demand <- scenario_demand %>% 
    mutate(fill_color = if_else(priority == "Statement Demand",
                                "Statement Demand",
                                if_else(priority == "Statement Demand",
                                        "Statement Demand",
                                        if_else(p_year >= priority_selected,
                                                "Junior Post-14", "Post-14"))),
           fill_color = ordered(fill_color, levels = wa_demand_order)) %>% 
    group_by(scenario, rept_date, fill_color) %>% 
    summarise(demand_daily_af = sum(demand_daily_af, na.rm = TRUE),
              demand_cfs = sum(demand_cfs, na.rm = TRUE),
              .groups = "drop")



## Render plot. ----

g <- ggplot(data = plot_demand,
            aes(x = rept_date,
                y = demand_daily_af,
                group = fill_color,
                fill = fill_color)) 

g <- g + geom_area(position = "stack")

g <- g + scale_fill_manual(name = "Demand type:",
                           values = wa_demand_pal,
                           labels = c(paste(priority_selected, 
                                            "& Junior Post-14 Demand"),
                                      "Senior Post-14 Demand",
                                      "Statement Demand"))

g <- g + labs(y = "Acre-Feet/Day")

g <- g + theme(
    legend.position = "bottom",
    strip.text.x = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2)),
    axis.title.x = element_blank())

g <- g + facet_wrap(facets = vars(plot_demand$scenario),
             nrow = length(unique(plot_demand$scenario)))










