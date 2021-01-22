
## Load library packages.

if(!("package:tools" %in% search())) {
  suppressMessages(library(tools))
}
if(!("package:tidyr" %in% search())) {
  suppressMessages(library(tidyr))
}
if(!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if(!("package:readr" %in% search())) {
  suppressMessages(library(readr))
}
if(!("package:janitor" %in% search())) {
  suppressMessages(library(janitor))
}
if(!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}
if(!("package:stringr" %in% search())) {
  suppressMessages(library(stringr))
}
if(!("package:aws.s3" %in% search())) {
  suppressMessages(library(aws.s3))
}

### Initialization. ----

plot_year <- year(Sys.Date())

## Switches.


## Load and process historical flow statistics data.

# Load data.
supply_hist_stats_raw <- read_csv(file = "./supply-data/SupplyData_Stats.csv",
                             na = c("#DIV/0!", "#NUM!", "", "NA"))

# Filter/rename required fields.
supply_hist_stats <- supply_hist_stats_raw %>%
  select(-wy_mo) %>% 
  rename(huc8_name = huc8,
         rept_month = cy_mo) %>% 
  filter(stat %in% c("mean", "median", "p10", "p90"))

# Build scenario descriptions.
supply_hist_stats <- supply_hist_stats %>% 
  mutate(wy_type = if_else(wy_type == "C", 
                           "Critical Year",
                           if_else(wy_type == "D", 
                                   "Dry Year",
                                   ifelse(wy_type == "BN", 
                                          "Below Normal Year",
                                          ifelse(wy_type == "AN", "
                                                 Above Normal Year",
                                                 "Wet Year")))))
supply_hist_stats <- supply_hist_stats %>% 
  mutate(scenario = paste0("Historic: Estimated ", 
                                  toTitleCase(stat), 
                                  " Unimpaired Flow at ", 
                                  source_gage, ", ",
                                  wy_type)) %>% 
  select(huc8_name, 
         scenario,
         rept_month,
         af,
         cfs) %>% 
  arrange(huc8_name, scenario, rept_month) %>% 
  drop_na()

# Build plot_date.
supply_hist_stats <- supply_hist_stats %>% 
  mutate(plot_date = as.Date(paste(plot_year, rept_month, 15, sep = "-"))) %>% 
  select(-rept_month)

# Convert af to af/day.
supply_hist_stats <- supply_hist_stats %>% 
  mutate(af_day = af / as.numeric(days_in_month(plot_date))) %>% 
  select(huc8_name,
         scenario,
         plot_date,
         af_day,
         cfs) %>% 
  arrange(huc8_name,
          scenario,
          plot_date)

## Combine supply sources.
supply <- bind_rows(supply_hist_stats)

## Save data files locally and to S3 bucket. ----

# Save to S3 for Shiny app to pick up.
supply_create_date <- Sys.Date()
save(supply,
     supply_create_date,
     file = "./output/dwast-supplies.RData")
put_object(file = "./output/dwast-supplies.RData", 
           object = "dwast-supplies.RData", 
           bucket = "dwr-enf-shiny",
           multipart = TRUE)


# Explore a plot.
# library(ggplot2)
# 
# huc8_selected <- "McCloud"
# supply_selected <- supply_hist_stats %>% 
#   filter(huc8_name %in% huc8_selected)
# 
# ggplot(data = supply_selected,
#        aes(x = plot_date, y = cfs)) +
#   geom_rect(aes(xmin = plot_date - 10, 
#                 ymin = cfs - 0.005 * max(cfs), 
#                 xmax = plot_date + 10, 
#                 ymax = cfs + 0.005 * max(cfs), 
#                 fill = scenario)) +
#   geom_line(aes(color = scenario))







