
## Load library packages.

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
if(!("package:sf" %in% search())) {
  suppressMessages(library(sf))
}
if(!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}
if(!("package:aws.s3" %in% search())) {
  suppressMessages(library(aws.s3))
}

### Initialization. ----

## Switches.
download_new_wrinfo <- FALSE
download_new_pods <- FALSE
save_data_gaps <- FALSE
report_multi_hucs <- FALSE

## Create project folders if they don't exist.  <-- purrr this!

# Create `data-gaps` folder if it doesn't exist.
if(!dir.exists("./data-gaps/")) dir.create("./data-gaps/")

# Create `dwr-files` folder if it doesn't exist.
if(!dir.exists("./dwr-files/")) dir.create("./dwr-files/")

# Create `gis` folder if it doesn't exist.
if(!dir.exists("./gis")) dir.create("./gis")

# Create `reports` folder if it doesn't exist.
if(!dir.exists("./reports/")) dir.create("./reports/")

# Create `output` folder if it doesn't exist.
if(!dir.exists("./output/")) dir.create("./output/")

## Define variables.

# Define water right types to include in dataset.
wr_type_list <- c("Appropriative",
                  "Federal Claims",
                  "Federal Stockponds",
                  "Registration Cannabis",
                  "Registration Domestic",
                  "Registration Irrigation",
                  "Registration Livestock",
                  "Statement of Div and Use",
                  "Stockpond")

# Define post-14 water right types.
post_14_wr_types <- c("Appropriative",
                      "Federal Stockponds",
                      "Registration Cannabis",
                      "Registration Domestic",
                      "Registration Irrigation",
                      "Registration Livestock",
                      "Stockpond")

# Define priority categories. For simpler data munging, don't create 
# ordered factor here. Do it in the screening tool.
#  - Environmental Flow > Statement Diversion > 1914 > 1915 ... > current year.
priority_order <- c(c(year(now()):1914),  
                    "Statement Demand", 
                    "Environmental Demand")

## Load and process Water Rights LIST data (wr_info). ----

# Load water right information from California Water Rights LIST on the CA
# Open Data Portal website. We trust the data sets we publish to the public,
# right?
# 
# Data set home page: https://data.ca.gov/dataset/water-rights
if(download_new_wrinfo) {
  wr_info_url <- paste0("https://data.ca.gov/dataset/",
                        "9ae95238-12f9-47dd-bc62-e6409920607e/resource/",
                        "151c067a-088b-42a2-b6ad-99d84b48fb36/download/",
                        "water_rights_list_2020-09-15.csv")
  wr_info_raw <- 
    read_csv(wr_info_url,
             col_types = cols(
               application_number = col_character(),
               primary_owner_name = col_character(),
               certificate_id = col_character(),
               permit_id = col_character(),
               license_id = col_character(),
               water_right_type = col_character(),
               water_right_status = col_character(),
               priority_date = col_date(),
               application_recd_date = col_date(format = "%Y-%m-%d %H:%M:%S"),
               application_acceptance_date = col_date(),
               effective_from_date = col_date(format = "%Y-%m-%d %H:%M:%S"),
               type_of_stock = col_character(),
               application_recd_date = col_date(format = "%Y-%m-%d %H:%M:%S"),
               offer_sent_date = col_date(format = "%Y-%m-%d"),
               accepted_offer_date = col_date(format = "%Y-%m-%d"),
               effective_to_date = col_date(format = "%Y-%m-%d"),
               seperately_owned = col_character(),
               supplemental_statement_cycle = col_character(),
               type_of_stock = col_logical(),
               rejection_date = col_date(format = "%Y-%m-%d %H:%M:%S"),
               surface_water_diversions = col_character(),
               state_well_number = col_character(),
               sub_type = col_character(),
               ini_reported_div_unit = col_character(),
               qow_diverted_unit = col_character(),
               max_rate_of_div_unit = col_character(),
               water_use_min_unit = col_character(),
               water_use_max_unit = col_character(),
               type_of_diversion_facility = col_character(),
               incomplete_statement = col_character()
             ))
  wr_info_dest <- paste0("./dwr-files/wr_info_", Sys.Date(), ".RData")
  save(wr_info_raw, file = wr_info_dest)
}

# Load last downloaded wr_info data.
wr_info_files <- file.info(list.files("./dwr-files/", full.names = T))
f_name <- rownames(wr_info_files)[which.max(wr_info_files$mtime)]
load(f_name)

# Clean column names, remove duplicate records and empty columns.
wr_info <- wr_info_raw %>%
  clean_names(.) %>%
  remove_empty("cols") %>% 
  distinct() %>% 
  filter(!is.na(application_number))

## Build tibble of active water rights. ----

# Discard unneeded columns and keep water right types to include.
wr_info <- wr_info %>% 
  select(wr_id = application_number,
         owner = primary_owner_name,
         wr_type = water_right_type,
         wr_status = water_right_status,
         application_recd_date,
         application_acceptance_date,
         priority_date) %>% 
  filter(wr_type %in% wr_type_list)

## Determine priority year for post-14 appropriative rights. ----

# Define wr_class as either `post-14` for post-14 rights, `statement`.
wr_info <- wr_info %>% 
  mutate(wr_class = if_else(wr_type %in% post_14_wr_types, 
                            "post14", 
                            "claimed")) %>% 
  relocate(wr_class, .after = wr_status)
wr_info <- split(wr_info, wr_info$wr_class)

# Filter out Post-14 appropriatives that have no priority-related date info.
no_p_dates_ <- wr_info$post14 %>%
  filter(is.na(application_recd_date) &
           is.na(application_acceptance_date) &
           is.na(priority_date))
wr_info$post14 <- wr_info$post14 %>%
  anti_join(., no_p_dates_, by = "wr_id")

# Save list of records with the missing data to the data-gaps folder.
if(save_data_gaps) {
  save_fn <- paste0("./data-gaps/missing-priority-info ",
                    Sys.Date(), ".csv")
  write_csv(no_p_dates_, save_fn)
}

# Determine priority date and year.
wr_info$post14 <- wr_info$post14 %>% 
  rowwise() %>% 
  mutate(priority_date = min(application_recd_date,
                             application_acceptance_date,
                             priority_date,
                             na.rm = TRUE),
         priority = as.character(year(priority_date))) %>% 
  select(-application_recd_date,
         -application_acceptance_date)

# Rebuild `wr_info$claimed` to match structure of `wr_info$post14`. Recode 
# `priority_date` to NA, `priority` to "Statement Demand."
wr_info$claimed <- wr_info$claimed %>% 
  mutate(priority_date = NA,
         priority = "Statement Demand") %>% 
  select(-application_recd_date,
         -application_acceptance_date)

# Bind wr_info back into one tibble.
wr_info <- wr_info %>% 
  bind_rows() %>% 
  arrange(wr_id)

## Load and process POD shapefile. ----

# Download latest version of POD geodatabase if switch is on.
if(download_new_pods) {
  if(!dir.exists("./gis")) dir.create("./gis")
  pod_file <- paste0("https://waterrightsmaps.waterboards.ca.gov/viewer/",
                     "Resources/Images/eWRIMS/download/eWRIMS_Data.gdb.zip")
  pod_gdb_dest <- paste0("./gis/eWRIMS_Data_", Sys.Date(), ".gdb")
  
  temp <- tempfile()
  download.file(url = pod_file,
                destfile = temp)
  unzip(temp, exdir = pod_gdb_dest)
  unlink(temp)
  rm(temp)
}

# Load last downloaded pod geospatial data.
gdb_files <- file.info(list.files("./gis", full.names = T))
dsn <- rownames(gdb_files)[which.max(gdb_files$mtime)]
pods_raw <- read_sf(dsn = dsn,
                        layer = st_layers(dsn)[[1]][[1]]) # rewrite with regex capture layer name
# plot(st_geometry(pods_raw)) # Should look like CA.

# Fix Water right ID mismatches.
#  1. For some reason, an extra 'R' is appended to 94 domestic registration 
#     APPL_IDs in the pod layer data table..
pods <- pods_raw %>% 
  mutate(APPL_ID = ifelse(grepl(".*R.*", APPL_ID),
                          str_remove(APPL_ID, "R"),
                          APPL_ID))

# Clean and filter for active rights in wr_info.
pods <- pods %>% 
  select(wr_id = APPL_ID,
         huc8_name = HU_8_NAME) %>% 
  filter(wr_id %in% wr_info$wr_id) %>% 
  arrange(wr_id, huc8_name)

# Filter out PODs that are missing HUC-8 info.
no_huc8s_ <- pods %>%
  filter(is.na(huc8_name))
pods <- pods %>% 
  filter(!is.na(huc8_name))

# Save list of records with the missing data to the data-gaps folder.
if(save_data_gaps) {
  save_fn <- paste0("./data-gaps/missing-huc8-info-",
                    Sys.Date(), ".csv")
  write_csv(no_huc8s_, save_fn)
}

# ID water rights that have no info in pods.
missing_pod_info_ <- wr_info %>%
  filter(!wr_id %in% pods$wr_id)

# Save list of records with the missing POD data to the data-gaps folder.
if(save_data_gaps) {
  save_fn <- paste0("./data-gaps/missing-pod-info-",
                    Sys.Date(), ".csv")
  write_csv(missing_pod_info_, save_fn)
}

# Create wr_id - huc8 list to attach to wr_info and assign default demand 
# weighting as even distribution among watersheds.
huc8_list <- pods %>% 
  st_drop_geometry() %>% 
  select(wr_id,
         huc8_name) %>% 
  filter(!is.na(huc8_name)) %>% 
  distinct() %>% 
  group_by(wr_id) %>% 
  mutate(demand_wt = 1 / n())

# Join huc8_name and demand_wt to wr_info.
wr_info <- wr_info %>% 
  left_join(., huc8_list, by = "wr_id")

# Create list of multi-huc rights if switched.
if(report_multi_hucs) {
  multi_huc_rights_ <- wr_info %>% 
    filter(demand_wt < 1)
  save_fn <- paste0("./reports/multi-huc8-rights-",
                    Sys.Date(), ".csv")
  write_csv(multi_huc_rights_, save_fn)
}

## Insert updated demand weighting factors. ----

# Placeholder. See:
# https://stackoverflow.com/questions/48724684/lookup-and-replace-column-values-using-dplyr-in-r
# For inspiration.

## Prepare POD layer. ----

# Add additional information to layer for more informative popups.
pods <- pods %>% 
  left_join(., wr_info, by = c("wr_id", "huc8_name")) %>% 
  select(-wr_class, -demand_wt) %>% 
  relocate(SHAPE, .after = last_col())

# Make numerical p_year column. Introduces NAs by coercion, but that's ok.
pods<- pods %>% 
  mutate(p_year = ifelse(!grepl("\\D", .$priority), 
                         suppressWarnings(as.numeric(priority)),
                         NA)) %>% 
  relocate(p_year, .after = priority)
 
# Reproject points to st_crs("+proj=longlat +datum=WGS84 +no_defs") (4326).
# This is the projection leaflet likes.
pods <- st_transform(pods, 4326)

## Prepare HUC-8 layer. ----
dsn <- "./common/CA_HUC-8_Watersheds"
layer <- "CA_HUC-8_Watersheds"
huc8_layer <- read_sf(dsn = dsn,
                         layer = layer)
huc8_layer <- st_transform(huc8_layer,
                              crs = 4326)

huc8_layer <- huc8_layer %>% 
  select(huc8_name = name,
         geometry)

## Save data files locally and to S3 bucket. ----

# Save to S3 for Shiny app to pick up.
create_date <- Sys.Date()
save(wr_info, 
     pods,
     huc8_layer,
     wr_type_list,
     create_date,
     file = "./output/wasdet-wrinfo.RData")
put_object(file = "./output/wasdet-wrinfo.RData", 
           object = "wasdet-wrinfo.RData", 
           bucket = "dwr-enf-shiny",
           multipart = TRUE)
