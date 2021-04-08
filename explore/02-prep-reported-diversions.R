library(tidyr)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(ggplot2)
library(purrr)

## Set project year.
project_year <- 2021

## Demand palette. ----

demand_order <- ordered(c("Junior Post-14",
                          "Post-14",
                          "Statement Demand",
                          "Environmental Demand"))
priority_order <- c(c(year(now()):1914),  
                    "Statement Demand", 
                    "Environmental Demand")

# Load last downloaded wr_info data.
# wr_info_files <- file.info(list.files("./output/", full.names = T))
# f_name <- rownames(wr_info_files)[which.max(wr_info_files$mtime)]
load("./output/dwast-data-2020-12-08.RData")

# Load diversion data, filter for reporting years 2011 on.
diversions_raw <- read_csv("./demands/wruds_diversion_data_20201120.zip")
diversions <- diversions_raw %>% clean_names() %>% 
  select(scenario = year,
         wr_id = appl_id,
         rept_month = month,
         everything(),
         -water_right_id) %>% 
  filter(scenario >=2011)

# Recode scenario name to be more descriptive.
diversions <- diversions %>% 
  mutate(scenario = paste0("Reported Diversions - ", scenario))

# Split diversions into list of tibbles by scenario.
demand <- split(diversions, diversions$scenario)

# Attach HUC-8s to demand scenario.
demand <- map(.x = demand,
              .f = ~left_join(., select(wr_info,
                                        wr_id, 
                                        huc8_name, 
                                        wr_class, 
                                        priority,
                                        demand_wt), 
                              by = "wr_id"))

# Scale diversion amounts by HUC-8 weighting factor.
demand <- map(.x = demand,
              .f = ~mutate(., amount_weighted = amount * demand_wt))

# Aggregate diversions by huc8_name and priority. 
agg_huc_pri <- function(x) {
  x <- x %>% 
    group_by(scenario,
             huc8_name, 
             priority, 
             rept_month) %>% 
    summarise(demand = sum(amount_weighted, na.rm = TRUE),
              .groups = "drop") %>% 
    arrange(huc8_name, 
            priority, 
            rept_month) %>% 
    drop_na()
}
demand <- map(.x = demand,
              .f = agg_huc_pri)
 
## Demands are AF/month. Plots will be on a daily time step. Convert to AF/day.
## map to daily time series with project_year.

# Create vector containing series of dates for project year.
months_to_dates <- tibble(rept_date = seq(as.Date(paste0(project_year, 
                                                         "-01-01")),
                                          as.Date(paste0(project_year, 
                                                         "-12-31")),
                                          by = "days"),
                          rept_month = as.numeric(month(rept_date)))

make_daily_demands <- function(x) {
  x <- x %>% 
    right_join(., months_to_dates, by = "rept_month") %>%
    mutate(demand_daily_af = demand / as.numeric(days_in_month(rept_date)),
           demand_daily_cfs = demand_daily_af * 0.504166667) %>% 
    select(scenario,
           huc8_name, 
           rept_date, 
           priority, 
           demand_daily_af,
           demand_daily_cfs) %>% 
    arrange(huc8_name, 
            rept_date, 
            priority, 
            demand_daily_af,
            demand_daily_cfs)
}
demand <- map(.x = demand,
              .f = make_daily_demands)

# Make p_year column. Introduces NAs by coercion, but that's the desired effect.
make_numeric_priority <- function(x) {
  x <- x %>% 
    mutate(p_year = ifelse(!is.na(as.numeric(priority)), 
                           as.numeric(priority),
                           NA))
}
demand <- map(.x = demand,
                   .f = make_numeric_priority)
  
#############################################################3


## Filter, munge, plot. ----  

scenario_selected <- sample(names(demand), 1)

plot_demand <- demand[[scenario_selected]]

huc8_selected <- sample(unique(plot_demand$huc8_name), 1)


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


