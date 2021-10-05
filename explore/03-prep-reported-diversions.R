
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
if(!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}

## Initialization. ----

# Switches.
use_local_file <- TRUE
save_data_gaps <- TRUE

# Load wr_info.Rdata.
load("./output/wr_info.RData")

## Load and Process reported diversions flat file. ----
## Load directly from jasperreports when intranet access available.

if(use_local_file) {
  file_loc <- "./dwr-flat-files/water_use_report_20201020.zip"
} else {
  file_loc <- "http://jasperreports/EwrimsFlatFile/water_use_report.csv"
}
diversions_raw <- readr::read_csv(file_loc)
diversions <- diversions_raw %>%
  dplyr::select(wr_id = APPL_ID,
         rep_year = YEAR,
         rep_month = MONTH,
         amount = AMOUNT,
         diversion_type = DIVERSION_TYPE)

# Convert numeric months to names
diversions <- diversions %>%
  dplyr::mutate(rep_month = ordered(month.abb[rep_month], levels = month.abb))

# Limit Reported Diversion scenarios to prior 10 years.
diversions <- diversions %>%
  dplyr::filter(diversion_type != "USE",
         rep_year %in% seq((as.numeric(format(Sys.Date(), "%Y"))-10), 
                           length.out = 10))

# For each water right id, aggregate diversions by year and month.
diversions <- diversions %>%
  dplyr::group_by(wr_id, rep_year, rep_month) %>%
  dplyr::summarise(diverted = sum(amount, na.rm = TRUE),
            .groups = "drop")

# Add demand type distinguishers.
diversions <- diversions %>%
  dplyr::mutate(demand_type = "Reported Diversions",
         demand_scenario = paste0("Reported Diversions - ", rep_year))

# Join wr_info to diversions.
diversions <- wr_info %>%
  dplyr::right_join(., diversions, by = "wr_id") %>% 
  arrange(demand_type,
          demand_scenario,
          priority,
          wr_id,
          rep_month)

# capture water rights that have no reported diversion data in eWRIMS.
never_reported <- wr_info %>% 
  dplyr::filter(!wr_id %in% diversions$wr_id)
readr::write_csv(never_reported, "./data-gaps/never_reported.csv")

# Rearrange diversions into list of tibbles by demand scenario.
diversions <- split(diversions, 
                      f = diversions$demand_scenario)
  
## save diversions for testing other modules.
save(diversions, file = "./output/diversions.RData")

