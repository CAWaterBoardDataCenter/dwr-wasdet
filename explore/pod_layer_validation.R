# POD Layer validation
# Are geospatial data available for all rights to be included?
# Look at Face value of missing pods


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
if(!("package:sp" %in% search())) {
  suppressMessages(library(sp))
}
if(!("package:rgdal" %in% search())) {
  suppressMessages(library(rgdal))
}
if(!("package:spdplyr" %in% search())) {
  suppressMessages(library(spdplyr))
}
if(!("package:rmapshaper" %in% search())) {
  suppressMessages(library(rmapshaper))
}
if(!("package:janitor" %in% search())) {
  suppressMessages(library(janitor))
}


# Load wr_info.
load("wr_info.RData")

## Load and process POD shapefile. ----

# identify most recently downloaded gdb.
gdb_files <- file.info(list.files("./gis", full.names = T))
dsn <- rownames(gdb_files)[which.max(gdb_files$mtime)]

# Load POD shapefile
pods_raw <- readOGR(dsn = dsn,
                    layer = ogrListLayers(dsn)[1], 
                    verbose = FALSE)
names(pods_raw) <- make_clean_names(names(pods_raw))

# Clean and filter for active rights in wr_info.
pods <- pods_raw %>% 
  select(appl_id,
         pod_num,
         appl_pod,
         hu_8_name) %>% 
  rename(wr_id = appl_id,
         huc8_name = hu_8_name) %>% 
  filter(wr_id %in% wr_info$wr_id) %>% 
  arrange(wr_id)
plot(pods) # Should look like California

# Filter out PODs that are missing HUC-8 info.
no_huc8s_ <- pods %>%
  filter(is.na(huc8_name))
pods <- pods %>% 
  filter(!wr_id %in% no_huc8s_$wr_id)

# remove missing HUC-8 rights from `wr-info`.
wr_info <- wr_info %>% 
  filter(!wr_id %in% no_huc8s_$wr_id)

pod_data <- pods@data

wr_info_missing_pod_ <- wr_info %>% 
  filter(!wr_id %in% pod_data$wr_id)
