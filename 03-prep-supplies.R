
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
  mutate(s_scenario = paste0("Historic: Estimated ", 
                                  toTitleCase(stat), 
                                  " Unimpaired Flow at ", 
                                  source_gage, ", ",
                                  wy_type)) %>% 
  select(huc8_name, 
         s_scenario,
         rept_month,
         af_monthly,
         cfs) %>% 
  arrange(huc8_name, s_scenario, rept_month) %>% 
  drop_na()

# Build plot_date.
supply_hist_stats <- supply_hist_stats %>% 
  mutate(plot_date = as.Date(paste(plot_year, rept_month, 15, sep = "-"))) %>% 
  select(-rept_month)

# Convert af to af/day.
supply_hist_stats <- supply_hist_stats %>% 
  mutate(af_daily = af_monthly / as.numeric(days_in_month(plot_date))) %>% 
  select(huc8_name,
         s_scenario,
         plot_date,
         af_monthly,
         af_daily,
         cfs) %>% 
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

# Convert af to af_daily and cfs.
supply_forecast_wsi <- supply_forecast_wsi %>% 
  mutate(af_daily = af_monthly / as.numeric(days_in_month(plot_date)),
         cfs = af_daily * 0.504166667) %>% 
  select(huc8_name,
         s_scenario,
         plot_date,
         af_monthly,
         af_daily,
         cfs) %>% 
  arrange(huc8_name,
          s_scenario,
          plot_date)

## Combine supply sources. ----
supply <- bind_rows(supply_hist_stats, supply_forecast_wsi)

 # Save data files locally and to S3 bucket. ----

# Save locally and to to S3 for dashboard to pick up.
supply_create_date <- Sys.Date()
outfile_loc <- "./output/wasdet-supplies.RData"
save(supply,
     supply_create_date,
     file = outfile_loc)
put_object(file = outfile_loc,
           object = "wasdet-supplies.RData",
           bucket = "dwr-enf-shiny",
           multipart = TRUE)
