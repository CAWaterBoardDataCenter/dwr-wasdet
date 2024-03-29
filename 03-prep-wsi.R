## Load library packages----
if(!("package:tidyverse" %in% search())) {
  suppressMessages(library(tidyverse))
}
if(!("package:readxl" %in% search())) {
  suppressMessages(library(readxl))
}
if(!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}

# set required filepaths----
SJWSI_file <- "./supply-data/sjwsi/SJWSI_Summary_20210301.csv"
SRWSI_file <- "./supply-data/srwsi/SRWSI_Summary_20210301.csv"
station_file <- "./supply-data/wsi-huc8-station-id-lookup.csv"

# import wsi file dates and station data----
sjwsi_datetime <- mdy(read_csv(SJWSI_file,
                               skip = 1,
                               n_max = 1,
                               col_names = FALSE)[[1]])
sjwsi_date_text <- paste(month(sjwsi_datetime), 
                         day(sjwsi_datetime), 
                         str_sub(year(sjwsi_datetime), -2, -1),
                         sep = "/")
srwsi_datetime <- mdy(read_csv(SRWSI_file,
                               skip = 1,
                               n_max = 1,
                               col_names = FALSE)[[1]])
srwsi_date_text <- paste(month(srwsi_datetime), 
                         day(srwsi_datetime), 
                         str_sub(year(srwsi_datetime), -2, -1),
                         sep = "/")

station_df <- read_csv(station_file) %>% 
  filter(complete.cases(.) == TRUE)

# create function to extract wsi data----
wsi_extract <- function(station_df) {
  if (station_df$source_file[i] == "SJWSI") {
    WSI_file = SJWSI_file
    date_text = sjwsi_date_text
  } else {
    WSI_file = SRWSI_file
    date_text = srwsi_date_text
  }
  wsi_extracted <- WSI_file %>% 
  read_csv(skip = station_df$start[i] - 1, n_max = 6) %>% 
  rename(exceedance = X1) %>% 
  select(1:13) %>% 
  pivot_longer(2:13, 
               names_to = "rept_month", 
               values_to = "af_monthly") %>% 
  mutate(rept_month = match(rept_month, month.abb),
         af_monthly = af_monthly * 1000) %>% 
  filter(rept_month >= month(sjwsi_datetime) & rept_month <= 9) %>% 
  arrange(desc(exceedance), rept_month) %>% 
  mutate(huc8_name = station_df$huc8_name[i],
    s_scenario = paste0("Forecast: Unimpaired flow at ",
                             station_df$station_id[i],
                             ", ",
                             exceedance,
                             " Probability Of Exceedance (",
                             date_text,
                             ")")) %>% 
  select(huc8_name, s_scenario, rept_month, af_monthly)
  return(wsi_extracted)
}

# for loop to extract data over station dataframe----
i = 1
for (i in 1:length(station_df[[1]])) {
  temp_extract <- wsi_extract(station_df)
  if(exists("wsi_extracted")){
    wsi_extracted <- bind_rows(wsi_extracted, temp_extract)
  } else {
    wsi_extracted <- temp_extract
  }
  i = i + 1
}
output_filename = paste0("b120-wsi-", 
              format(as.Date(sjwsi_datetime), '%Y%m%d'),
              ".csv")
write_csv(wsi_extracted,
          paste0("./supply-data/wsi/",
                 output_filename))
