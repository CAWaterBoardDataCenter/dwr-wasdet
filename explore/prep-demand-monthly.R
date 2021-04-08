library(tidyr)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(ggplot2)
library(wesanderson)

## Demand palette. ----

demand_order <- ordered(c("Junior Post-14",
                          "Post-14",
                          "Statement Demand",
                          "Environmental Demand"))

# GrandBudapest1 theme from wesanderson package.
demand_pal <- wes_palettes$GrandBudapest1[c(2, 1, 4, 3)]
names(demand_pal) <- demand_order

diversions_raw <- read_csv("./demands/wruds_diversion_data_20201120.zip")
diversions <- diversions_raw %>% clean_names() %>% 
  select(wr_id = appl_id,
         scenario = year,
         rept_month = month,
         everything(),
         -water_right_id)

scenario_selected <- 2019
huc8_selected <- "Upper Cache"
priority_selected <- 1925

# Load wr_info
load("./output/dwast-data-2020-11-24.RData")

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


## Add fill directives based on priority year selected. ----
#------reactive filter on priority selection----------------
# Make p_year column. Introduces NAs by coercion, but that's the desired effect.
demand_test <- demand_test %>% 
  mutate(p_year = as.numeric(as.character(priority)))
  

demand_test <- demand_test %>% 
  mutate(fill_color = if_else(priority == "Statement Demand", 
                             "Statement Demand", 
                             if_else(priority == "Statement Demand", 
                                    "Statement Demand", 
                                    if_else(p_year >= priority_selected, 
                                            "Junior Post-14", "Post-14"))),
         fill_color = ordered(fill_color, levels = demand_order))

save(demand_test, file = "./output/demand_test.RData")

# See if this plots
g <- ggplot(data = demand_test,
            aes(x = as.factor(rept_month),
                y = demand,
                group = fill_color,
                fill = fill_color)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = demand_pal)
g










## Demands are AF/month. Plots will be on a daily time step. Convert to AF/day.
## map to daily time series with base year 1900 (non leap year)
## ignore leap years? 

# Create time series vector.

# month_to_dates <- tibble(date = seq(as.Date("1900-01-01"), 
#                             as.Date("1900-12-31"), 
#                             by = "days"),
#                  rept_month = month(date))
# 
# demand_test <- demand_test %>% 
#   right_join(., month_to_dates, by = "rept_month") %>% 
#   select(scenario, wr_id, date, demand)
