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




# Get filtered water right info.
ws_wr_info <- filter(wr_info, huc8_name %in% huc8_selected)

# # Build filtered plot demand tibble.
# plot_demand <- filter(demand[[huc8_selected]], 
#                       d_scenario %in% d_scene_selected) %>% 
#   mutate(fill_color = if_else(priority == "Statement Demand",
#                               "Statement Demand",
#                               if_else(priority == "Statement Demand",
#                                       "Statement Demand",
#                                       if_else(p_year >= priority_selected,
#                                               "Junior Post-14", "Post-14"))),
#          fill_color = ordered(fill_color, levels = wa_demand_order)) %>% 
#   group_by(d_scenario, plot_date, fill_color) %>% 
#   summarise(af_day = sum(af_day, na.rm = TRUE),
#             cfs = sum(cfs, na.rm = TRUE),
#             .groups = "drop") %>% 
#   mutate(plot_group = "demand",
#          s_scenario = NA) %>% 
#   select(d_scenario, s_scenario, plot_date,
#          fill_color, af_day, cfs, plot_group)



# # Build filtered plot supply tibble.
# plot_supply <- filter(supply, huc8_name %in% huc8_selected,
#                       s_scenario %in% s_scene_selected) %>% 
#   mutate(fill_color = NA,
#          plot_group = "supply") %>% 
#   full_join(., 
#             as_tibble(d_scene_selected), 
#             by = character()) %>% 
#   select(d_scenario = value, 
#          s_scenario, 
#          plot_date, 
#          fill_color, 
#          af_day, 
#          cfs, 
#          plot_group)


plot_data <- bind_rows(filter(demand[[huc8_selected]], 
                              d_scenario %in% d_scene_selected) %>% 
                         mutate(fill_color = if_else(priority == "Statement Demand",
                                                     "Statement Demand",
                                                     if_else(priority == "Statement Demand",
                                                             "Statement Demand",
                                                             if_else(p_year >= priority_selected,
                                                                     "Junior Post-14", "Post-14"))),
                                fill_color = ordered(fill_color, levels = wa_demand_order)) %>% 
                         group_by(d_scenario, plot_date, fill_color) %>% 
                         summarise(af_day = sum(af_day, na.rm = TRUE),
                                   cfs = sum(cfs, na.rm = TRUE),
                                   .groups = "drop") %>% 
                         mutate(plot_group = "demand",
                                s_scenario = NA) %>% 
                         select(d_scenario, s_scenario, plot_date,
                                fill_color, af_day, cfs, plot_group), 
                       filter(supply, huc8_name %in% huc8_selected,
                              s_scenario %in% s_scene_selected) %>% 
                         mutate(fill_color = NA,
                                plot_group = "supply") %>% 
                         full_join(., 
                                   as_tibble(d_scene_selected), 
                                   by = character()) %>% 
                         select(d_scenario = value, 
                                s_scenario, 
                                plot_date, 
                                fill_color, 
                                af_day, 
                                cfs, 
                                plot_group))





#plot_data <- bind_rows(plot_demand, plot_supply)



## Render plot. ----

g <- ggplot(data = plot_data,
            aes(x = plot_date,
                y = af_day)) 

# Demand
g <- g + geom_area(data = subset(plot_data, plot_group == "demand"),
                   position = "stack",
                   aes(fill = fill_color))

# Supply
g <- g + geom_point(data = subset(plot_data, plot_group == "supply"),
                    aes(color = s_scenario,
                        shape = s_scenario),
                    size = 4)


g <- g + geom_line(data = subset(plot_data, plot_group == "supply"),
                   aes(color = s_scenario))


g <- g + scale_fill_manual(name = "Demand Type:",
                           values = wa_demand_pal,
                           labels = c(paste(priority_selected, 
                                            "& Junior Post-14 Demand"),
                                      "Senior Post-14 Demand",
                                      "Statement Demand"))

g <- g + scale_shape_manual(name = element_blank(),
                            values = wa_supply_shapes)

g <- g + scale_color_manual(name =element_blank(),
                            values = wa_supply_pal)

g <- g + facet_wrap(~ d_scenario, 
                   ncol = 1)

g <- g + labs(y = "Acre-Feet/Day")

g <- g + theme(
    legend.position = "bottom",
    legend.box = "vertical",
    # strip.text.x = element_text(size = rel(1.5)),
    # axis.title = element_text(size = rel(1.2)),
    # axis.text = element_text(size = rel(1.2)),
    # legend.text = element_text(size = rel(1.2)),
    # legend.title = element_text(size = rel(1.2)),
    axis.title.x = element_blank())

g








