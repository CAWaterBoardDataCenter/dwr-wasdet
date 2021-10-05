## Load library packages.

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
  load("./output/dwast-demands-test_v2.RData")
  
  # Supply data.
  load("./output/dwast-supplies_v2.RData")
}

## Define color palettes and plot order. ----




#   >>>>>
## Water right type.
wrt_pal <- c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))[2:10]
names(wrt_pal) <- sort(unique(wr_info$wr_type))
#   >>>>
## watershed

# Get filtered water right info.
ws_wr_info <- filter(wr_info, huc8_name %in% huc8_selected)

## scenario

# scenario_demand <- filter(demand[[huc8_selected]], 
#                           d_scenario %in% d_scene_selected)


scenario_demand <- filter(demand[[huc8_selected]],
                          d_scenario %in% d_scene_selected)

wrt_plot_data <- scenario_demand %>% 
#  mutate(plot_month = lubridate::month(plot_date, label = TRUE))

  group_by(d_scenario, 
           wr_type, 
           plot_month = month(plot_date, label = TRUE)) %>%
  summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
            af_daily = sum(af_daily, na.rm = TRUE),
            cfs = sum(cfs, na.rm = TRUE),
            .groups = "drop")
  

## wrt plot. ----

g <- ggplot(data = wrt_plot_data,
            aes(x = plot_month,
                y = cfs,
                fill = wr_type)) +
  
  # Demand bars.
  geom_col() + 
  
 


scale_fill_manual(name = "Water Right Type:",
                           values = wrt_pal) +

  labs(y = "cfs") +
  
  # Facet.
  facet_wrap(~ d_scenario,
             ncol = 1) +
  




 theme(
  legend.position = "bottom",
  legend.box = "vertical",
  # strip.text.x = element_text(size = rel(1.5)),
  # axis.title = element_text(size = rel(1.2)),
  # axis.text = element_text(size = rel(1.2)),
  # legend.text = element_text(size = rel(1.2)),
  # legend.title = element_text(size = rel(1.2)),
  axis.title.x = element_blank())

g



















































