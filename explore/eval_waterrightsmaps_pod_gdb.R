
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
if(!("package:rgdal" %in% search())) {
  suppressMessages(library(rgdal))
}
if(!("package:spdplyr" %in% search())) {
  suppressMessages(library(spdplyr))
}
if(!("package:janitor" %in% search())) {
  suppressMessages(library(janitor))
}

## Initialization.

download_pods <- FALSE


## Download latest version of geodatabase. ----
if(download_pods) {
  if(!dir.exists("./gis")) dir.create("./gis")
  pod_file <- paste0("https://waterrightsmaps.waterboards.ca.gov/viewer/",
                     "Resources/Images/eWRIMS/download/eWRIMS_Data.gdb.zip")
  pod_gdb_dest <- paste0("./gis/eWRIMS_Data_", Sys.Date(), ".gdb")
  
  temp <- tempfile()
  download.file(url = pod_file,
                destfile = temp)
  unzip(temp, exdir = pod_gdb_dest)
  unlink(temp)
}


## Load and process POD shapefile. ----

# Point to geodatabase.
dsn <- "./gis/eWRIMS_Data_2020-09-25.gdb/"
# ogrListLayers(dsn) # Run to get list of layers.

# Load POD shapefile
pods <- readOGR(dsn = dsn,
                layer = "Points_of_Diversion_20200801", 
                verbose = FALSE)

# Clean and filter for active rights in wr_info.
names(pods) <- make_clean_names(names(pods))
pods <- pods %>% 
  rename(wr_id = appl_id) %>% 
  filter(wr_id %in% wr_info$wr_id)



