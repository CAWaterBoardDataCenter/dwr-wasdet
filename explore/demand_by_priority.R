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
if(!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}
if(!("package:sf" %in% search())) {
  suppressMessages(library(sf))
}
if(!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}
if(!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if(!("package:spdplyr" %in% search())) {
  suppressMessages(library(spdplyr))
}
if(!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}
if(!("package:scales" %in% search())) {
  suppressMessages(library(scales))
}
if(!("package:DT" %in% search())) {
  suppressMessages(library(DT))
}


## Initialize values. ---

# Data source.
load_from_s3 <- ifelse(Sys.info()["nodename"] == "Home-iMac.local", FALSE, TRUE)

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

# Load HUC-8 watershed polygons.
huc8_polys <- read_sf(dsn = "./common/CA_HUC-8_Watersheds/",
                      layer = "CA_HUC-8_Watersheds")
huc8_polys <- st_transform(huc8_polys,
                           crs = 4326)

## Define color and shape aesthetics. ----

# Water right type.
plot_wrt_pal <- c(wes_palette("Darjeeling1"), 
                  wes_palette("Darjeeling2"))[2:10]
names(plot_wrt_pal) <- sort(unique(wr_info$wr_type))
map_wrt_pal <- colorFactor(palette = plot_wrt_pal, 
                           domain = wr_info$wr_type)

# Demand.
wa_demand_order <- ordered(c("Junior Post-14",
                             "Post-14",
                             "Statement Demand",
                             "Environmental Demand"))
wa_demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(wa_demand_pal) <- wa_demand_order

# Supply.
wa_supply_pal <- colorRampPalette(wes_palette("Rushmore")[3:4])(3)
wa_supply_shapes <- c(15, 16, 17)

# Priority.
priority_order <- c("Environmental Demand", 
                           "Statement Demand", 
                           as.character(c(1914:year(now()))))

p_order <- ordered(demand[[huc8_selected]]$priority, levels = priority_order)

# Demand by water right type dataset.
pri_plot_data <- demand[[huc8_selected]] %>% 
    filter(d_scenario %in% scene_selected) %>% 
  mutate(priority = ordered(priority, levels = priority_order)) %>% 
    group_by(priority, 
             plot_month = month(plot_date, label = TRUE)) %>%
    summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
              af_daily = sum(af_daily, na.rm = TRUE),
              cfs = sum(cfs, na.rm = TRUE),
              .groups = "drop")
