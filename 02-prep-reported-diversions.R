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
# source("f_getReportedDivs.R")


# set up parallel R sessions.
plan(multisession)

# Set project year.
project_year <- 2021

# Define variables.

# demand_order <- ordered(c("Selected Priority and Junior",
#                           "Post-14",
#                           "Statement Demand",
#                           "Environmental Demand"))
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
  select(d_scenario = year,
         wr_id = appl_id,
         rept_month = month,
         af_monthly = amount,
         everything(),
         -water_right_id) %>% 
  filter(d_scenario >= 2011)

# Recode scenario name to be more descriptive.
diversions <- diversions %>% 
  mutate(d_scenario = paste0("Reported Diversions - ", d_scenario))

# join wr_info.
diversions <- diversions %>% 
  left_join(., select(wr_info,
                      huc8_name,
                      wr_id, 
                      owner,
                      wr_type,
                      wr_status,
                      wr_class, 
                      priority,
                      demand_wt), 
            by = "wr_id")

# Split diversions into list of tibbles by scenario.
demand <- split(x = diversions, 
                f = diversions$huc8_name)
demand <- future_map(.x = demand,
                     .f = ~ select(., -huc8_name))

# Scale diversion amounts by HUC-8 weighting factor.
demand <- future_map(.x = demand,
              .f = ~mutate(., af_monthly = af_monthly * demand_wt))

# Aggregate diversions by water right id. 
agg_dem_wrid <- function(x) {
  x <- x %>% 
    rowwise() %>% 
    group_by(d_scenario, wr_id, rept_month) %>% 
    mutate(af_monthly = sum(af_monthly, na.rm = TRUE) * demand_wt) %>% 
    select(-diversion_type) %>% 
    distinct() %>% 
    ungroup()
}
demand <- future_map(.x = demand,
                     .f = agg_dem_wrid)

## Demands are AF/month. Plots will be on a daily time step. Calculate 
## month-averaged daily AF and cfs. Map to date time series with project_year.

# Create vector containing series of dates for project year.
dates_to_map <- tibble(plot_date = seq(as.Date(paste0(project_year, 
                                                      "-01-01")),
                                       as.Date(paste0(project_year, 
                                                      "-12-31")),
                                       by = "month"),
                       rept_month = as.numeric(month(plot_date)))

make_daily_demands <- function(x) {
  x <- x %>% 
    right_join(., dates_to_map, by = "rept_month") %>%
    mutate(af_daily = af_monthly / as.numeric(days_in_month(plot_date)),
           cfs = af_daily * 0.504166667) %>% 
    select(d_scenario,
           wr_id,
           owner,
           wr_type,
           wr_status,
           wr_class, 
           priority,
           plot_date,
           af_monthly,
           af_daily,
           cfs) %>% 
    arrange(d_scenario,
            plot_date, 
            priority)
}
demand <- future_map(.x = demand,
                     .f = make_daily_demands,
                     .progress = TRUE)

# Make p_year column. Introduces NAs by coercion, but that's ok.
make_numeric_priority <- function(x) {
  x <- x %>% 
    # Check that it doesn't match any non-number
    #  numbers_only <- function(x) !grepl("\\D", x)
    mutate(p_year = ifelse(!grepl("\\D", .$priority), 
                           suppressWarnings(as.numeric(priority)),
                           NA))
}
demand <- map(.x = demand,
              .f = make_numeric_priority)

# Kill parallel R sessions. 
plan(sequential)

## Save data files locally and to S3 bucket. ----

# Save to S3 for Shiny app to pick up.
demand_create_date <- Sys.Date()
save(demand,
     demand_create_date,
     file = "./output/dwast-demands.RData")
# put_object(file = "./output/dwast-demands.RData", 
#           object = "dwast-demands.RData", 
#           bucket = "dwr-enf-shiny",
#           multipart = TRUE)























































