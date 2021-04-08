# Load library packages. ----

if (!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if (!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}
if (!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}
if (!("package:janitor" %in% search())) {
  suppressMessages(library(janitor))
}
if (!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if (!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}
if (!("package:scales" %in% search())) {
  suppressMessages(library(scales))
}
if (!("grid:gtable" %in% search())) {
  suppressMessages(library(grid))
}
if (!("package:gtable" %in% search())) {
  suppressMessages(library(gtable))
}
if (!("package:gridExtra" %in% search())) {
  suppressMessages(library(gridExtra))
}

load_from_s3 <- FALSE
show_rt_supply <- TRUE

## Selections. ----

# HUC-8 watershed.
huc8_selected <- "Upper San Joaquin"

# Demand scenario.
#d_scene_selected <- "Reported Diversions - 2019"
d_scene_selected <- c("Reported Diversions - 2019", "Reported Diversions - 2011")

# Supply scenario.
#s_scene_selected <- "Historic: Mean Unimpaired Flow at AMA, Below Normal Year"
s_scene_selected <- c("Historic: P90 Unimpaired Flow at SJF, Wet Year",
                      "Forecast: Unimpaired Flow at MIL, 50% Probability Of Exceedance (2/1/21)")

# Priority cutoff.
priority_selected <- 1950

## Load data files. ----

# Water Right Info.
load("./output/wasdet-wrinfo.RData")

# Demand data.
load("./output/wasdet-demands.RData")

# Supply data.
load("./output/wasdet-supplies.RData")

# Define color and shape aesthetics. ----

# Demand.
wa_demand_order <- ordered(c("Junior Post-14",
                             "Post-14",
                             "Statement Demand",
                             "Environmental Demand"))
wa_demand_pal <- c(wes_palettes$GrandBudapest1[c(2, 1)], "#BEBEBE", "#000000")
names(wa_demand_pal) <- wa_demand_order
map_demand_pal <- leaflet::colorFactor(palette = wa_demand_pal,
                                       levels = names(wa_demand_pal))

# Water right type.
plot_wrt_pal <- c(wesanderson::wes_palette("Darjeeling1"),
                  wesanderson::wes_palette("Darjeeling2"))[2:10]
names(plot_wrt_pal) <- sort(unique(wr_info$wr_type))
map_wrt_pal <- leaflet::colorFactor(palette = plot_wrt_pal,
                                    domain = wr_info$wr_type)

# Priority.
priority_order <- c(c(lubridate::year(lubridate::now()):1914),
                    "Statement Demand", "Environmental Demand")
priority_pal <- c(scales::viridis_pal()(length(c(lubridate::year(lubridate::now()):1914))),
                  "#BEBEBE", "#000000")
names(priority_pal) <- priority_order
map_priority_pal <- leaflet::colorFactor(palette = priority_pal,
                                         levels = names(priority_pal))

# Historical and forecast supply.
wa_supply_pal <- colorRampPalette(wesanderson::wes_palette("Rushmore")[3:4])(3)
wa_supply_shapes <- c(15, 16, 17)

# Current-year supply.
cy_supply_pal <- "blue"

# Plot theme.
sd_plot_theme <-  theme(
  legend.box = "vertical",
  legend.direction = "horizontal",
  # strip.text.x = element_text(size = rel(1.5)),
  # axis.title = element_text(size = rel(1.2)),
  # axis.text = element_text(size = rel(1.2)),
  # legend.text = element_text(size = rel(1.2)),
  # legend.title = element_text(size = rel(1.2)),
  axis.title.x = element_blank()
)

# Build dataset for plot based on selections. ----

## Build demand dataset. ----

plot_demand <- filter(demand[[huc8_selected]], 
                      d_scenario %in% d_scene_selected) %>%
  mutate(fill_color = if_else(priority == "Statement Demand",
                              "Statement Demand",
                              if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(p_year >= priority_selected,
                                              "Junior Post-14", "Post-14"))),
         fill_color = ordered(fill_color, levels = wa_demand_order)) %>%
  group_by(d_scenario, plot_date, fill_color, plot_category) %>%
  summarise(af_monthly = sum(af_monthly, na.rm = TRUE),
            af_daily = sum(af_daily, na.rm = TRUE),
            cfs = sum(cfs, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(s_scenario = NA) %>%
  select(d_scenario, 
         s_scenario, 
         plot_date,
         fill_color, 
         af_monthly,
         af_daily, 
         cfs, 
         plot_category) %>% 
  # Add boundary points to facilitate barplot vis and correct stacking.
  bind_rows(old = .,
            new = mutate(., 
                         plot_date = ceiling_date(x = plot_date,
                                                  unit = "month") - 1),
            .id = "source") %>% 
  arrange(plot_date, source)

# # test plot.
# ggplot(data = plot_demand,
#        aes(x = plot_date,
#            y = cfs)) +
#   geom_area(position = "stack",
#             aes(fill = fill_color)) +
#   facet_wrap(~ d_scenario,
#              ncol = 1)

## Build supply dataset. ----

plot_supply <- filter(supply[[huc8_selected]],
                      s_scenario %in% s_scene_selected | plot_category == "Current") %>%
  mutate(source = "old",
         fill_color = NA,
         plot_group = "supply") %>%
  full_join(.,
            as_tibble(d_scene_selected),
            by = character()) %>%
  select(source,
         d_scenario = value,
         s_scenario,
         plot_date,
         fill_color,
         af_monthly,
         af_daily,
         cfs,
         plot_category)

## Bind datasets. ----
vsd_plot_data <- bind_rows(plot_demand, plot_supply)

### Reactive: Filter out Current year supply name. ----
names(cy_supply_pal) <- filter(vsd_plot_data, 
                               grepl("^Current", plot_category)) %>% 
  select(s_scenario) %>% 
  distinct() %>% 
  unlist(.)


### load("Test Bed Data.Rdata")

# Generate plot. ----

## Create dummy plots to extract individual legends from each. ----

# Demand.
p1 <- ggplot(data = vsd_plot_data,
             aes(x = plot_date,
                 y = cfs)) +
  geom_area(data = subset(vsd_plot_data, plot_category == "demand"),
            position = "stack",
            aes(fill = fill_color)) +
  scale_fill_manual(name = "Demand Type:",
                    values = wa_demand_pal,
                    labels = c(paste(priority_selected, 
                                     "& Junior Post-14 Demand"),
                               c(paste(priority_selected - 1, 
                                       "& Senior Post-14 Demand"),
                                 "Statement Demand"))) +
  sd_plot_theme
leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")

# Historic & forecast.
p2 <- ggplot(data = vsd_plot_data,
             aes(x = plot_date,
                 y = cfs)) +
  geom_point(data = subset(vsd_plot_data, plot_category %in% c("Historic", "Forecast")),
             aes(color = s_scenario,
                 shape = s_scenario),
             size = 4) +
  geom_line(data = subset(vsd_plot_data, plot_category %in% c("Historic", "Forecast")),
            aes(color = s_scenario)) +
  scale_shape_manual(name = "Supply Type:",
                     values = wa_supply_shapes) +
  scale_color_manual(name = "Supply Type:",
                     values = wa_supply_pal) +
  sd_plot_theme
leg2 <- gtable_filter(ggplot_gtable(ggplot_build(p2)), "guide-box")

# Current-year supply.
if (show_rt_supply) {
  p3 <- ggplot(data = vsd_plot_data,
               aes(x = plot_date,
                   y = cfs)) +
    geom_line(data = subset(vsd_plot_data, 
                            plot_category %in% c("Current")),
              aes(color = s_scenario)) +
    scale_color_manual(name = "Current-year Supply:",
                       values = cy_supply_pal) +
    sd_plot_theme
  leg3 <- gtable_filter(ggplot_gtable(ggplot_build(p3)), "guide-box")
}

## Build plot. ---- 

g <- ggplot(data = vsd_plot_data,
            aes(x = plot_date,
                y = cfs)) +
  
  # Demand
  geom_area(data = subset(vsd_plot_data, plot_category == "demand"),
            position = "stack",
            aes(fill = fill_color)) +
  scale_fill_manual(name = "Demand Type:",
                    values = wa_demand_pal,
                    labels = c(paste(priority_selected, 
                                     "& Junior Post-14 Demand"),
                               c(paste(priority_selected - 1, 
                                       "& Senior Post-14 Demand"),
                                 "Statement Demand"))) +
  
  # Supply
  geom_point(data = subset(vsd_plot_data, plot_category %in% c("Historic", "Forecast")),
             aes(color = s_scenario,
                 shape = s_scenario),
             size = 4) +
  
  
  geom_line(data = subset(vsd_plot_data, plot_category %in% c("Historic", "Forecast")),
            aes(color = s_scenario)) + 
  scale_shape_manual(name = "Supply Type:",
                     values = wa_supply_shapes) +
  scale_color_manual(name = "Supply Type:",
                     values = wa_supply_pal) + {
                       
                       if (show_rt_supply) 
                         geom_line(data = subset(vsd_plot_data,
                                                 plot_category %in% c("Current")),
                                   color = cy_supply_pal)
                       else
                         geom_blank() } +
  # Facet wrap.
  facet_wrap(~ d_scenario,
             ncol = 1) +
  
  # Theme.
  theme(
    legend.position = "none",
    # legend.direction = "horizontal",
    # strip.text.x = element_text(size = rel(1.5)),
    # axis.title = element_text(size = rel(1.2)),
    # axis.text = element_text(size = rel(1.2)),
    # legend.text = element_text(size = rel(1.2)),
    # legend.title = element_text(size = rel(1.2)),
    axis.title.x = element_blank()
  )
  
plot <- arrangeGrob(leg1, leg2, leg3, 
                    ncol = 1, 
                    heights = rep(1,3))  
plot <- arrangeGrob(g, plot, 
                    ncol = 1,
                    heights = c(5,1))
grid.newpage()
grid.draw(plot)
  #   
  #   
  #   scale_fill_manual(name = "Demand Type:",
  #                     values = wa_demand_pal,
  #                     labels = c(paste(priority_selected, 
  #                                      "& Junior Post-14 Demand"),
  #                                "Senior Post-14 Demand",
  #                                "Statement Demand")) +
  #   
#   scale_shape_manual(name = "Supply Type:",
#                      values = wa_supply_shapes) +
#   
#   scale_color_manual(name = "Supply Type:",
#                      values = wa_supply_pal) +
#   
#   
#   
#   labs(y = "cfs") +
#   
#   theme(
#     legend.position = "bottom",
#     legend.box = "vertical",
#     # strip.text.x = element_text(size = rel(1.5)),
#     # axis.title = element_text(size = rel(1.2)),
#     # axis.text = element_text(size = rel(1.2)),
#     # legend.text = element_text(size = rel(1.2)),
#     # legend.title = element_text(size = rel(1.2)),
#     axis.title.x = element_blank())








# vsd_plot_data <- bind_rows(
#   
#   # Demand.
#   { filter(demand[[huc8_selected]], 
#            d_scenario %in% d_scene_selected) %>%
#       mutate(fill_color = if_else(priority == "Statement Demand",
#                                   "Statement Demand",
#                                   if_else(priority == "Statement Demand",
#                                           "Statement Demand",
#                                           if_else(p_year >= priority_selected,
#                                                   "Junior Post-14", "Post-14"))),
#              fill_color = ordered(fill_color, levels = wa_demand_order)) %>%
#       group_by(d_scenario, plot_date, fill_color) %>%
#       summarise(af_monthly = sum(af_monthly, na.rm = TRUE),
#                 af_daily = sum(af_daily, na.rm = TRUE),
#                 cfs = sum(cfs, na.rm = TRUE),
#                 .groups = "drop") %>% 
#       mutate(plot_group = "demand",
#              s_scenario = NA) %>%
#       select(d_scenario, 
#              s_scenario, 
#              plot_date,
#              fill_color, 
#              af_monthly,
#              af_daily, 
#              cfs, 
#              plot_group) %>% 
#       # Add boundary points to facilitate barplot vis and correct stacking.
#       bind_rows(old = .,
#                 new = mutate(., 
#                              plot_date = ceiling_date(x = plot_date,
#                                                       unit = "month") - 1),
#                 .id = "source") %>% 
#       arrange(plot_date, source)
#   },
#   
#   # Supply.
#   { filter(supply, huc8_name %in% huc8_selected,
#            s_scenario %in% s_scene_selected) %>%
#       mutate(source = "old",
#              fill_color = NA,
#              plot_group = "supply") %>%
#       full_join(.,
#                 as_tibble(d_scene_selected),
#                 by = character()) %>%
#       select(source,
#              d_scenario = value,
#              s_scenario,
#              plot_date,
#              fill_color,
#              af_monthly,
#              af_daily,
#              cfs,
#              plot_group)
#   }
# )











































