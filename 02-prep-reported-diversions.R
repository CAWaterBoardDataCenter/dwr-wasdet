library(tidyr)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(purrr)
library(future)
library(furrr)
library(aws.s3)

## Initialization. ----

download_divs <- FALSE

# Load functions.
source("f_getReportedDivs.R")


# set up parallel R sessions.
plan(multisession)

# Set project year.
project_year <- 2021

# Define variables.

demand_order <- ordered(c("Selected Priority and Junior",
                          "Post-14",
                          "Statement Demand",
                          "Environmental Demand"))
priority_order <- c(c(year(now()):1914),  
                    "Statement Demand", 
                    "Environmental Demand")

# Load last created wr_info data.
wr_info_files <- file.info(list.files(path = "./output/", 
                                      full.names = TRUE,
                                      pattern = "dwast-wrinfo"))
f_name <- rownames(wr_info_files)[which.max(wr_info_files$mtime)]
load(file = f_name)

# Download diversion data from WRUDS link.
if(download_divs) {
  divs_fname <- getReporteDivsCSV()
} else {
  div_data_files <- file.info(list.files("./wruds_downloads/", 
                                         full.names = T))
  divs_fname <- rownames(div_data_files)[which.max(div_data_files$mtime)]
}

# Load diversion data, filter for reporting years 2011 on.
diversions_raw <- read_csv(divs_fname)

diversions <- diversions_raw %>% clean_names() %>% 
  select(scenario = year,
         wr_id = appl_id,
         rept_month = month,
         everything(),
         -water_right_id) %>% 
  filter(scenario >= 2011)

# Recode scenario name to be more descriptive.
diversions <- diversions %>% 
  mutate(scenario = paste0("Reported Diversions - ", scenario))

# join wr_info.
diversions <- diversions %>% 
  left_join(., select(wr_info,
                      wr_id, 
                      huc8_name, 
                      wr_class, 
                      priority,
                      demand_wt), 
            by = "wr_id")

# Split diversions into list of tibbles by scenario.
demand <- split(diversions, diversions$huc8_name)

# Scale diversion amounts by HUC-8 weighting factor.
demand <- future_map(.x = demand,
              .f = ~mutate(., amount_weighted = amount * demand_wt))

# Aggregate diversions by huc8_name and priority. 
agg_huc_pri <- function(x) {
  x <- x %>% 
    group_by(huc8_name,
             scenario,
             priority, 
             rept_month) %>% 
    summarise(demand = sum(amount_weighted, na.rm = TRUE),
              .groups = "drop") %>% 
    arrange(scenario, 
            priority, 
            rept_month) %>% 
    drop_na()
}
demand <- future_map(.x = demand,
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
           demand_cfs = demand_daily_af * 0.504166667) %>% 
    select(huc8_name,
           scenario,
           rept_date, 
           priority, 
           demand_daily_af,
           demand_cfs) %>% 
    arrange(scenario,
            rept_date, 
            priority, 
            demand_daily_af,
            demand_cfs)
}
demand <- future_map(.x = demand,
              .f = make_daily_demands,
              .progress = TRUE)

# Make p_year column. Introduces NAs by coercion, but that's ok.
make_numeric_priority <- function(x) {
  x <- x %>% 
    mutate(p_year = ifelse(!is.na(as.numeric(priority)), 
                           as.numeric(priority),
                           NA))
}
demand <- map(.x = demand,
                   .f = make_numeric_priority)

# Kill parallel R sessions. 
plan(sequential)

## Save data files locally and to S3 bucket. ----

# Save to S3 for Shiny app to pick up.
demand_create_date <- Sys.Date()
save_fname <- paste0("./output/dwast-demands-",
                     create_date,".RData")
save(demand,
     demand_create_date,
     file = save_fname)
put_object(file = save_fname, 
          object = "dwast-demands.RData", 
          bucket = "dwr-enf-shiny",
          multipart = TRUE)
