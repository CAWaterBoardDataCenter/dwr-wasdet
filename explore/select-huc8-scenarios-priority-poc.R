
# Load libraries.
library(dplyr)
library(wesanderson)
library(ggplot2)

huc8_selected <- "San Pablo Bay"

load("./output/dwast-wrinfo-2020-12-15.RData")
load("./output/dwast-demands-2020-12-15.RData")

# Create color palette for demand bins.
demand_order <- ordered(c("Junior Post-14",
                          "Post-14",
                          "Statement Demand",
                          "Environmental Demand"))
demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(demand_pal) <- demand_order

# Grab demand and wr_info for selected HUC-8.
ws_demand <- demand[[huc8_selected]]

ws_wr_info <- filter(wr_info, huc8_name %in% huc8_selected)


