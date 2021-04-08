
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

## Load S3 keys. ----
source("load-s3-keys.R")

## Load and process historical flow statistics data.

# Load data.
supply_hist_stats_raw <- read_csv(file = "./supply-data/SupplyData_Stats.csv",
                                  na = c("#DIV/0!", "#NUM!", "", "NA"))

# Filter/rename required fields.
supply_hist_stats <- supply_hist_stats_raw %>%
  select(-wy_mo) %>% 
  rename(huc8_name = huc8,
         station_id = source_gage,
         rept_month = cy_mo,
         af_monthly = af) %>% 
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
  mutate(s_scenario = paste0("Historic: ", 
                             toTitleCase(stat), 
                             " Unimpaired Flow at ", 
                             station_id, ", ",
                             wy_type)) %>% 
  # Build plot_date.
  mutate(plot_date = as.Date(paste(plot_year, rept_month, 15, sep = "-"))) %>% 
  
  select(huc8_name,
      #   station_id,
         s_scenario,
         plot_date,
         af_monthly,
         cfs) %>% 
  drop_na()

# Convert af to af/day, add plot_category column
supply_hist_stats <- supply_hist_stats %>% 
  mutate(af_daily = af_monthly / as.numeric(days_in_month(plot_date)),
         plot_category = "historic") %>% 
  relocate(af_daily, .after = af_monthly) %>% 
  relocate(plot_category, .after = s_scenario) %>%
  arrange(huc8_name,
          s_scenario,
          plot_date)

## Load and process yearly historic flow data. ----

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
  #       station_id,
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



# Load and process B120 WSI supply data. ----

# PLACEHOLDER
# If it is a new month, download new WSI PDFs, scrape with PDFtables package,
# then save munged data in TBD project folder. Currently using hand-built
# source csv file while dashboard is in development.

supply_forecast_wsi_raw <- read_csv("./supply-data/wsi/b120-wsi-20210201.csv")

# Build plot_date.
supply_forecast_wsi <- supply_forecast_wsi_raw %>% 
  mutate(plot_date = as.Date(paste(plot_year, rept_month, 15, sep = "-"))) %>% 
  select(-rept_month)

# Convert af to af_daily and cfs, add plot_category.
supply_forecast_wsi <- supply_forecast_wsi %>% 
  mutate(af_daily = af_monthly / as.numeric(days_in_month(plot_date)),
         cfs = af_daily * 0.504166667,
         plot_category = "forecast") %>% 
  select(huc8_name,
         s_scenario,
         plot_category,
         plot_date,
         af_monthly,
         af_daily,
         cfs) %>% 
  arrange(huc8_name,
          s_scenario,
          plot_date)

## Combine supply sources.
supply <- bind_rows(supply_hist_stats,
                    supply_hist_annual,
                    supply_forecast_wsi)

# Split diversions into list of tibbles by scenario.
supply <- split(x = supply, 
                f = supply$huc8_name)
supply <- map(.x = supply,
              .f = ~ select(., -huc8_name))

# # Retrieve CDEC Real-Time Full Natural Flows. ----
# 
# ## Scrape start and stop dates. ----
# cdec_start_date <- paste0(plot_year, "-01-01")
# cdec_end_date <- as.character(as.Date(now()))
# 
# ## Load list of CDEC stations to pull data from. ----
# cdec_sources <- read_csv("./common/cdec-actual-fnf-stations-daily.csv")
# 
# ## Download data from CDEC. ----
# supply_rt_raw <- list()
# get_cdec_fnfs <- function(x) {
#   cat(paste0("Processing Station ", x, "...\n\n"))
#   source_url <- paste0("https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", x,
#                        "&SensorNums=8&dur_code=D&Start=", cdec_start_date, 
#                        "&End=", cdec_end_date)
#   supply_rt_raw[[x]] <- read_csv(file = source_url,
#                                  na = c("NA", "---"))
# }
# supply_rt_raw <- map(.x = cdec_sources$station_id,
#                      .f = get_cdec_fnfs)
# names(supply_rt_raw) <- cdec_sources$huc8_name
# 
# ## Munge data. ----
# clean_cdec_fnfs <- function(x) {
#   x <- x %>% 
#     clean_names() %>% 
#     rename(plot_date = obs_date,
#            cfs = value) %>% 
#     
#     # Remove negative negative flows. These are misleading and don't contribute
#     # to the story being told.
#     filter(cfs > 0) %>% 
#     
#     mutate(s_scenario = paste("Current Year: Unimpaired flow at", 
#                               station_id),
#            plot_category = "current",
#            plot_date = as.Date(plot_date),
#            af_daily = cfs / 0.504166667,
#            af_monthly = af_daily * as.numeric(days_in_month(plot_date))
#            ) %>% 
#     select(station_id,
#            s_scenario,
#            plot_category,
#            plot_date,
#            af_monthly,
#            af_daily,
#            cfs) %>% 
#     drop_na()
#   
# }
# supply_rt <- map(.x = supply_rt_raw,
#                  .f = clean_cdec_fnfs)
# 
# # Join real-time supply data to appropriate watershed table(s). ----
# merged_tables <- map2(supply[names(supply_rt)], supply_rt, bind_rows)
# supply <- supply[!names(supply) %in% names(merged_tables)]
# supply <- c(supply, merged_tables)

# Save data files locally and to S3 bucket. ----

# Save locally and to to S3 for dashboard to pick up.
supply_create_date <- Sys.Date()
outfile_loc <- "./output/wasdet-supplies_test0407.RData"
save(supply,
     supply_create_date,
     file = outfile_loc)
put_object(file = outfile_loc,
           object = "wasdet-supplies_test0407.RData",
           bucket = "dwr-shiny-apps",
           multipart = TRUE)
