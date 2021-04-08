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

source("./explore/test_variable_values.R")

## Load data files. ----

# Water Right Info.
load("./output/dwast-wrinfo.RData")

# Demand data.
load("./explore/dwast-demands-test-set.RData")
#load("./output/dwast-demands.RData")

# Supply data.
load("./output/dwast-supplies.RData")

## Define color and shape aesthetics. ----

# Priority Palette.
#priority_pal <- c("black", "gray", viridis_pal()(length(c(1914:year(now())))))
priority_pal <- c(viridis_pal()(length(c(year(now()):1914))), "gray", "black")

names(priority_pal) <- c(c(year(now()):1914), "Statement Demand", "Environmental Demand")
# wrt_pal <- c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))[2:10]
# names(wrt_pal) <- sort(unique(wr_info$wr_type))


  
  # Demand by water right type dataset.
dbp_plot_data <- 
  demand[[huc8_selected]] %>% 
    filter(d_scenario %in% d_scene_selected) %>% 
    group_by(d_scenario, 
             priority, 
             plot_month = month(plot_date, label = TRUE)) %>%
    summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
              af_daily = sum(af_daily, na.rm = TRUE),
              cfs = sum(cfs, na.rm = TRUE),
              .groups = "drop") %>% 
  mutate(priority = ordered(priority, levels = names(priority_pal)))

 

# Render.
ggplot(data = dbp_plot_data,
       aes(x = plot_month,
           y = cfs,
           fill = priority)) +
  
  # Demand bars.
  geom_col() + 
  
  # Y axis format.
  scale_y_continuous(labels = comma) +
  
  # Legend.
  scale_fill_manual(name = "Priority:",
                    values = priority_pal) +
  
  # labels
  labs(title = "Monthly Demand by Priority",
       y = "Cubic Feet per Second (cfs)") +
  
  # Facet on demand scenario.
  facet_wrap(~ d_scenario, 
             ncol = 1,
             scales = "free_x") +
  
  # Theme.
  theme_minimal() +
  theme(
    plot.title = element_text(size = rel(2.0)),
    strip.text.x = element_text(size = rel(2.0)),
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2)),
  #  legend.position = "bottom",
   # legend.box = "horizontal",
  #  legend.direction = "horizontal",
    #  panel.spacing = unit(2, "lines"),
    axis.title.x = element_blank())







