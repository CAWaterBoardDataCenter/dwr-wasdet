library(tidyr)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(ggplot2)
library(wesanderson)

## Testing variables.

scenario_selected <- 2019
priority_selected <- 1920

## Set project year.
project_year <- 2021

## Demand palette. ----

demand_order <- ordered(c("Junior Post-14",
                          "Post-14",
                          "Statement Demand",
                          "Environmental Demand"))

# GrandBudapest1 theme from wesanderson package.
demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(demand_pal) <- demand_order

# Load wr_info,
load("./output/dwast-data-2020-11-24.RData")

# Load diversion data.
diversions_raw <- read_csv("./demands/wruds_diversion_data_20201120.zip")
diversions <- diversions_raw %>% clean_names() %>% 
  select(wr_id = appl_id,
         scenario = year,
         rept_month = month,
         everything(),
         -water_right_id)


#### Attempt 01: single year/scenario. ----

##### Purr this to map through all scenarios..

# select a scenario for development. Use 2019 reporting year.
demand_test <- diversions %>% 
  filter(scenario == scenario_selected) %>% 
  select(-scenario)

# Attach HUC-8s to demand scenario.
demand_test <- demand_test %>% 
  left_join(., select(wr_info, 
                      wr_id, 
                      huc8_name, 
                      wr_class, 
                      priority,
                      demand_wt), 
            by = "wr_id")

# Scale diversion amounts by HUC-8 weighting factor.

demand_test <- demand_test %>% 
  mutate(amount = amount * demand_wt)

# Aggregate diversions by huc8_name and priority. 
demand_test <- demand_test %>% 
  group_by(huc8_name, priority, rept_month) %>% 
  summarise(demand = sum(amount, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(huc8_name, priority, rept_month) %>% 
  drop_na()

## Demands are AF/month. Plots will be on a daily time step. Convert to AF/day.
## map to daily time series with base year 1900 (non leap year)
## ignore leap years? 

# Create vector containing eries of dates for project year.
months_to_dates <- tibble(rept_date = seq(as.Date(paste0(project_year, 
                                                         "-01-01")),
                                          as.Date(paste0(project_year, 
                                                         "-12-31")),
                                          by = "days"),
                          rept_month = month(rept_date))

demand_test <- demand_test %>%
  right_join(., months_to_dates, by = "rept_month") %>%
  mutate(demand_daily = demand / days_in_month(rept_date)) %>% 
  select(huc8_name, rept_date, priority, demand, demand_daily) %>% 
  arrange(huc8_name, huc8_name, rept_date, priority, demand, demand_daily)

## Add fill directives based on priority year selected. ----
#------reactive filter on priority selection----------------
# Make p_year column. Introduces NAs by coercion, but that's the desired effect.
demand_test <- demand_test %>% 
  mutate(p_year = as.numeric(as.character(priority)))



  
## Filter, munge, plot. ----  

huc8_selected <- sample(unique(demand_test$huc8_name), 1)


plot_demand <- demand_test %>% 
  filter(huc8_name == huc8_selected) %>% 
  mutate(fill_color = if_else(priority == "Statement Demand",
                              "Statement Demand",
                              if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(p_year >= priority_selected,
                                              "Junior Post-14", "Post-14"))),
         fill_color = ordered(fill_color, levels = demand_order)) %>% 
  group_by(rept_date, fill_color) %>% 
  summarize(demand = sum(demand, na.rm = TRUE),
            demand_daily = sum(demand_daily, na.rm = TRUE),
            .groups = "drop")

# See if this plots
g <- ggplot(data = plot_demand,
            aes(x = rept_date,
                y = demand_daily))

g <- g + geom_area(aes(fill = fill_color,
                       group = fill_color),
                   position = "stack")

# g <- g + scale_fill_manual(values = demand_pal)

g

# 
# demand_test <- demand_test %>% 
#   select(-fill_color)
# 
# save(demand_test, file = "./output/demand_test_daily.RData")


