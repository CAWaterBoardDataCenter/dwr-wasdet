# Load library packages. ----

if (!("package:tidyr" %in% search())) {
  suppressMessages(library(tidyr))
}
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

show_rt_supply <- TRUE

test_scenario <- 1

## Selections. ----

## Plot_supply generation test scenarios. ----

# 1. DF exists in supply for huc8_selected, s_scene_selected not null or na.
if( test_scenario == 1 ) {
  huc8_selected <- "Upper San Joaquin"
  s_scene_selected <- "Forecast: Unimpaired Flow at MIL, 10% Probability Of Exceedance (2/1/21)"
} else
  
  # 2. DF exists in supply for huc8_selected, s_scene_selected is NULL
  if( test_scenario == 2 ) {
    huc8_selected <- "Upper San Joaquin"
    s_scene_selected <- NULL
  } else 
    
    # 3. DF does not exist  in supply for huc8_selected.
    if( test_scenario == 3 ) {
      huc8_selected <- "Upper Pit"
      s_scene_selected <- NULL
    }


# # HUC-8 watershed.
# huc8_selected <- "Upper San Joaquin" # Has Supply and Demand
# huc8_selected <- "Upper Pit" # No Supply

# Demand scenario.
#d_scene_selected <- "Reported Diversions - 2019"
d_scene_selected <- c("Reported Diversions - 2019", "Reported Diversions - 2011")

# Supply scenario.
# s_scene_selected <- "Historic: Mean Unimpaired Flow at AMA, Below Normal Year"
# s_scene_selected <- c("Historic: P90 Unimpaired Flow at SJF, Wet Year",
#                       "Forecast: Unimpaired Flow at MIL, 50% Probability Of Exceedance (2/1/21)")
# s_scene_selected <- NA # For Upper Pit scenario

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

#-------------------------------------------------------------------------------


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
  mutate(plot_group = "demand",
         s_scenario = NA) %>%
  select(d_scenario, 
         s_scenario, 
         plot_date,
         fill_color, 
         af_monthly,
         af_daily, 
         cfs, 
         plot_group,
         plot_category) %>% 
  # Add boundary points to facilitate barplot vis and correct stacking.
  bind_rows(old = .,
            new = mutate(., 
                         plot_date = ceiling_date(x = plot_date,
                                                  unit = "month") - 1),
            .id = "source") %>% 
  arrange(plot_date, source)

# Conditionally build supply dataset. ----

# x <- supply[[huc8_selected]]
# s_scene <- s_scene_selected
# d_scene <- d_scene_selected


build_plot_supply <- function(x, s_scene, d_scene) {
  if( !is.null(x) & !is.null(s_scene) ) {
    y <- x %>% 
      filter(s_scenario %in% s_scene) %>%
      mutate(source = "old",
             fill_color = NA,
             plot_group = "supply") %>%
      full_join(.,
                as_tibble(d_scene),
                by = character()) %>%
      select(source,
             d_scenario = value,
             s_scenario,
             plot_date,
             fill_color,
             af_monthly,
             af_daily,
             cfs,
             plot_group,
             plot_category)
  } else {
    y <- NULL
  }
  return(y)
}

plot_supply <- build_plot_supply(supply[[huc8_selected]], s_scene_selected, d_scene_selected)

# # Why doesn't this work?
# plot_supply <- ifelse((!is.null(supply[[huc8_selected]]) | !is.null(s_scene_selected)),
#                       build_plot_supply(supply[[huc8_selected]], s_scene_selected, d_scene_selected),
#                       NA)

# # But this does?
# if( !is.null(supply[[huc8_selected]]) & !is.null(s_scene_selected) ) {
#   plot_supply <- build_plot_supply(supply[[huc8_selected]], s_scene_selected, d_scene_selected)
# } else {
#   plot_supply <- NULL
# }

## Bind demand and supply datasets. ----
vsd_plot_data <- rbind(plot_demand, if(!is.null(plot_supply)) plot_supply)  

# Generate plot. ----

# Render.
g <- ggplot(data = vsd_plot_data,
            aes(x = plot_date,
                y = cfs)) +

# Demand.
geom_area(data = subset(vsd_plot_data, plot_group == "demand"),
                   position = "stack",
                   aes(fill = fill_color)) +

# Supply.
geom_point(data = subset(vsd_plot_data, plot_group == "supply"),
                    aes(color = s_scenario,
                        shape = s_scenario),
                    size = 7) +
geom_line(data = subset(vsd_plot_data, plot_group == "supply"),
                   aes(color = s_scenario),
                   linetype = "dashed") +

# X axis format.
scale_x_date(date_labels = "%b %d",
                      date_minor_breaks = "1 month") +

# Y axis format.
scale_y_continuous(labels = comma) +
  
  # Demand legend.
  scale_fill_manual(name = "Demand Priority:",
                    values = wa_demand_pal,
                    labels = c(paste(priority_selected, 
                                     "& Junior Post-14 Demand"),
                               paste(as.numeric(priority_selected) -1,
                                     "& Senior Post-14 Demand"),
                               "Statement Demand")) +
  
  # Supply legend.
  scale_shape_manual(name = "Supply Scenario:",
                     values = wa_supply_shapes) +
  scale_color_manual(name = "Supply Scenario:",
                     values = wa_supply_pal) +
  
  # Facet on demand scenario.
  facet_wrap(~ d_scenario, 
             ncol = 1,
             scales = "free_x") +
  
  # Labels.
  labs(y = "Cubic Feet per Second (cfs)") +
  
  # Theme.
  theme_minimal() +
  theme(
 #   legend.box = "vertical",
    legend.position = "bottom",
    legend.direction = "vertical",
    # strip.text.x = element_text(size = rel(1.5)),
    # axis.title = element_text(size = rel(1.2)),
    # axis.text = element_text(size = rel(1.2)),
    # legend.text = element_text(size = rel(1.2)),
    # legend.title = element_text(size = rel(1.2)),
    axis.title.x = element_blank()
  )


















