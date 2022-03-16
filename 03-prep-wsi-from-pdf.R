## Load library packages----
if(!("package:tidyr" %in% search())) {
  suppressMessages(library(tidyr))
}
if(!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if(!("package:readr" %in% search())) {
  suppressMessages(library(readr))
}
if(!("package:readxl" %in% search())) {
  suppressMessages(library(readxl))
}
if(!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}

wsi_months <- month.abb[c(10:12, 1:9)]

start_month <- 3
forecast_date <- "3/1/2022"


# Set required file paths. ----

dwr_wsi_file <- "./supply-data/wsi/WSI_2022-02_03.xlsx"

# Load stations.
stations <- read_csv(station_file)


# Load, munge,  and merge SR and SJ WSI data. 
srwsi_raw <- read_xlsx(path = dwr_wsi_file,
                       range = "SR_03_2022!A3:M47",
                       skip = 2,
                       col_names = c("exceedance", wsi_months)
                       )

srwsi <- srwsi_raw %>% 
  drop_na(exceedance) %>% 
  filter(exceedance > 0.0) %>% 
  bind_cols(description = c(rep("Shasta Lake", 6),
                 rep("Sacramento River above Bend Bridge", 6),
                 rep("Feather River at Oroville", 6),
                 rep("Yuba River near Smartville plus Deer Creek", 6),
                 rep("American River below Folsom Lake", 6)),
            station = c(rep(NA, 18),
                        rep("YRS", 6),
                        rep(NA, 6)),
            huc8_name = c(rep(NA, 18),
                        rep("Upper Yuba", 6),
                        rep(NA, 6))
            ) %>% 
  select(huc8_name, station, description, everything())
                  
sjwsi_raw <- read_xlsx(path = dwr_wsi_file,
                       range = "SJ_03_2022!A3:M38",
                       skip = 2,
                       col_names = c("exceedance", wsi_months)
)

sjwsi <- sjwsi_raw %>% 
  drop_na(exceedance) %>% 
  filter(exceedance > 0.0) %>% 
  bind_cols(description = c(rep("Stanislaus River below Goodwin Reservoir", 6),
                            rep("Tuolumne River below La Grange Reservoir", 6),
                            rep("Merced River below Merced Falls", 6),
                            rep("San Joaquin River inflow to Millerton Lake", 6)),
            station = c(rep("GDW", 6),
                        rep("TLG", 6),
                        rep("MMF", 6),
                        rep("MIL", 6)),
            huc8_name = c(rep("Upper Stanislaus", 6),
                        rep("Upper Tuolumne", 6),
                        rep("Upper Merced", 6),
                        rep("Upper San Joaquin", 6))
  ) %>% 
  select(huc8_name, 
         station, 
         description, 
         everything())

wsi <- bind_rows(srwsi, sjwsi) %>% 
  pivot_longer(cols = Oct:Sep, 
               names_to = "rept_month", 
               values_to = "af_monthly") %>% 
  mutate(rept_month = match(rept_month, month.abb),
         af_monthly = as.numeric(af_monthly) * 1000) %>% 
  filter(rept_month >= start_month & rept_month <= 9) %>% 
  arrange(huc8_name, 
          rept_month) %>% 
  drop_na(station) %>% 
  mutate(exceedance = paste0(as.character( 100 * exceedance), "%")) %>% 
  mutate(s_scenario = paste0("Forecast: Unimpaired flow at ",
                      station,
                      ", ",
                      exceedance,
                      " Probability Of Exceedance (",
                      forecast_date,
                      ")")) %>% 
  select(huc8_name, s_scenario, rept_month, af_monthly)


output_filename = paste0("b120-wsi-", 
              format(as.Date(forecast_date, "%d/%m/%Y"), "%Y%m%d"),
              ".csv")
write_csv(wsi,
          paste0("./supply-data/wsi/",
                 output_filename))
