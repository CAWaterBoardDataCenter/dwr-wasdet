
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
if(!("package:geojsonio" %in% search())) {
  suppressMessages(library(geojsonio))
}
if(!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}

## Load and process POD shapefile. ----
dsn <- "/Volumes/work-stuff/gis/eWRIMS_Data_20201008.gdb"
ogrListLayers(dsn)

# identify most recently downloaded gdb.
# gdb_files <- file.info(list.files("./gis", full.names = T))
# dsn <- rownames(gdb_files)[which.max(gdb_files$mtime)]

# Load "Points_of_Diversion_20201001" shapefile.
pods <- readOGR(dsn = dsn,
                layer = "Points_of_Diversion_20201001", 
                verbose = TRUE)

# Load "PODs_dissolved_wr_id" shapefile.
pods_dissolved_wr_id <- readOGR(dsn = dsn,
                                layer = "PODs_dissolved_wr_id", 
                                verbose = FALSE)






names(pods_raw) <- make_clean_names(names(pods_raw))

# Clean and filter for active rights in wr_info.
pods <- pods_raw %>% 
  select(appl_id,
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



# Determine POD counts for each water right id.
pods <- pods %>% 
  group_by(wr_id) %>% 
  mutate(n_pods = n()) %>% 
  arrange(desc(n_pods, wr_id)) 

pods <- pods %>% 
  group_by(wr_id) %>% 
  mutate(n_hucs = n_distinct(huc8_name),
            huc8_list = paste(unique(huc8_name), collapse =", "))
max(pods$n_pods)
max(pods$n_hucs)

## Dissolve PODs of one right test ----

pods_t1 <- pods %>% filter(wr_id == "A000360")
plot(pods_t1)

# Convert From SpatialPointsDataFrame to SpatialPoints
pods_t1_spatialPoints <- as(pods_t1, "SpatialPoints")



pods_t1_dissolved <- rmapshaper::ms_dissolve(input = pods_t1)
plot(pods_t1_dissolved) 

# ,
#             huc8_list = paste(huc8_name, collapse =", ")) 
# ## Dissolve points of multi-POD water rights into polygons. ----
# 
# pods_t <- rmapshaper::ms_dissolve(input = pods,
#                                  field = "wr_id",
#                                  copy_fields = c("huc8_name",
#                                                  "n_pods",
#                                                  "huc8_list"))
# # 
# 
# 
# 
# 
# 
# pods_t1_json <- geojson_json(pods_t1)
# plot(pods_t1_json)
# 
# 
# pods <- spTransform(pods, CRS("+proj=longlat +datum=WGS84 +no_defs"))














