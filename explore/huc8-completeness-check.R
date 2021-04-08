
library(readr)
library(tidyr)
library(dplyr)
library(rgdal)
library(spdplyr)
library(janitor)
library(openxlsx)

use_local_data <- TRUE

##' Load water right information from California Water Rights LIST on the CA
##' Open Data Portal website. We trust the data sets we publish to the public,
##' right?
##' 
##' Data set home page: https://data.ca.gov/dataset/water-rights
if(!use_local_data) {
  wr_info_url <- paste0("https://data.ca.gov/dataset/",
                        "9ae95238-12f9-47dd-bc62-e6409920607e/resource/",
                        "151c067a-088b-42a2-b6ad-99d84b48fb36/download/",
                        "water_rights_list_2020-09-15.csv")
  
  wr_info_raw <- readr::read_csv(wr_info_url)
  
  # Remove duplicate records, looking at all columns.
  # janitor::get_dupes(wr_info_raw)
  wr_info_processed <- wr_info_raw %>%
    janitor::clean_names(.) %>%
    distinct()
  # janitor::get_dupes(wr_info_processed)
} else {
  load("./wr_info_processed.RData")
}

## Build tibble of active water rights.

# Discard unneeded columns and keep water right types to include.
wr_info <- wr_info_processed %>% 
  dplyr::select(wr_id = application_number,
                owner = primary_owner_name,
                certificate_id,
                permit_id,
                license_id,
                wr_type = water_right_type,
                wr_status = water_right_status,
                application_recd_date,
                application_acceptance_date,
                priority_date) %>% 
  dplyr::filter(wr_type %in% c("Appropriative",
                               "Federal Claims",
                               "Federal Stockponds",
                               "Registration Cannabis",
                               "Registration Domestic",
                               "Registration Irrigation",
                               "Registration Livestock",
                               "Statement of Div and Use",
                               "Stockpond"),
                !wr_status %in% c("Cancelled", "Rejected"))

## Define priority dates and years for appropriative rights from 
## application_recd_date, application_acceptance_date, and priority_date.

# to-do. ######

## Load and process POD shapefile.

# Point to geodatabase.
dsn <- "./gis/WAV Geodatabase.gdb"
# ogrListLayers(dsn) # Run to get list of layers.

# Load POD shapefile
pods <- readOGR(dsn = dsn,
                layer = "Points_of_Diversion_20200601", 
                verbose = FALSE)

# Clean and filter for active rights in wr_info.
names(pods) <- make_clean_names(names(pods))
pods <- pods %>% 
  rename(wr_id = appl_id) %>% 
  filter(wr_id %in% wr_info$wr_id)

# Plot the PODs, should roughly look like a map of CA.
plot(pods, pch = 20)
