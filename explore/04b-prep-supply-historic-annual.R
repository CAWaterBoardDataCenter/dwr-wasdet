
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
if(!("package:purrr" %in% search())) {
  suppressMessages(library(purrr))
}
if(!("package:aws.s3" %in% search())) {
  suppressMessages(library(aws.s3))
}

# Initialization. ----

plot_year <- year(Sys.Date())


## Load and process yearly historic flow data.

# Load data.
supply_hist_annual_raw <- read_csv(file = "./supply-data/historical/fnf_historic_years_combined.csv",
                                  na = c("#DIV/0!", "#NUM!", "", "NA"))

# Filter/rename required fields.
supply_hist_annual <- supply_hist_annual_raw %>%
  select(-wy_mo) %>% 
  rename(station_id = source_gage,
         rept_month = cy_mo,
         af_monthly = af)

supply_hist_annual <- supply_hist_annual %>% 
  mutate(s_scenario = paste0("Historic: ", cy, " Unimpaired Flow at ", station_id)) %>% 
  # Build plot_date.
  mutate(plot_date = as.Date(paste(plot_year, rept_month, 15, sep = "-"))) %>% 
  
  select(huc8_name,
         station_id,
         s_scenario,
         plot_date,
         af_monthly,
         cfs) %>% 
  drop_na()

# Convert af to af/day, add plot_category column
supply_hist_annual <- supply_hist_annual %>%
  mutate(af_daily = af_monthly / as.numeric(days_in_month(plot_date)),
         plot_category = "historic") %>% 
  relocate(af_daily, .after = af_monthly) %>% 
  relocate(plot_category, .after = s_scenario) %>%
  arrange(huc8_name,
          s_scenario,
          plot_date)



















