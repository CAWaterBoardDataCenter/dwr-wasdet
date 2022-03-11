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

wsi_months <- month.abb[c(10:12, 1:9)]



# set required filepaths----
srwsi_file <- "./supply-data/srwsi/SRWSI_Summary_20210401.csv"
SJWSI_file <- "./supply-data/sjwsi/SJWSI-20220201.csv"
station_file <- "./supply-data/wsi-huc8-station-id-lookup.csv"

# Load stations.
stations <- read_csv(station_file)

srwsi_raw <- read_csv(srwsi_file,
                      n_max = 45)[1:13]
names(srwsi_raw) <- c("exceedance", wsi_months)

srwsi <- srwsi_raw %>% 
  filter(!row_number() %in% 1) %>% 
  drop_na(exceedance) %>% 
  bind_cols(description = c(rep("Shasta Lake Unimpaired Inflow", 6),
                 rep("Sacramento River above Bend Bridge Unimpaired Flow", 6),
                 rep("Feather River at Oroville Unimpaired Flow", 6),
                 rep("Yuba River near Smartville plus Deer Creek Unimpaired Flow", 6),
                 rep("American River below Folsom Lake Unimpaired Flow", 6)),
            )
                  



sjwsi_raw <- read_csv(sjwsi_file)







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
#i = 1
for (i in 1:length(station_df[[1]])) {
  temp_extract <- wsi_extract(station_df)
  if(exists("wsi_extracted")){
    wsi_extracted <- bind_rows(wsi_extracted, temp_extract)
  } else {
    wsi_extracted <- temp_extract
  }
# i = i + 1
}
output_filename = paste0("b120-wsi-", 
              as.numeric(format(as.Date(sjwsi_datetime), '%Y%m%d')),
              ".csv")
write_csv(wsi_extracted,
          paste0("./supply-data/wsi/",
                 output_filename))
