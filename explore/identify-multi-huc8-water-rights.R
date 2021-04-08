
library(readr)
library(tidyr)
library(dplyr)
library(rgdal)
library(spdplyr)
library(janitor)
library(openxlsx)
library(ggplot2)

# Load wr_info.Rdata
load("wr_info.RData")

## Load and process POD shapefile.

# identify most recently downloaded gdb.
gdb_files <- file.info(list.files("./gis", full.names = T))
dsn <- rownames(gdb_files)[which.max(gdb_files$mtime)]

# Load POD shapefile
pods <- readOGR(dsn = dsn,
                    layer = ogrListLayers(dsn)[1], 
                    verbose = FALSE)

# Clean and filter for active rights in wr_info.
names(pods) <- make_clean_names(names(pods))
pods <- pods %>% 
  rename(wr_id = appl_id) %>% 
  filter(wr_id %in% wr_info$wr_id)

# Plot the PODs, should roughly look like a map of CA.
plot(pods, pch = 20)

## How many water rights have a POD in more than one HUC-8 watershed?

pods_tbl <- as_tibble(pods)

multi_huc8_rights <- pods_tbl %>% 
  distinct(wr_id, hu_8_name) %>% 
  arrange(hu_8_name) %>% 
  group_by(wr_id) %>% 
  summarize(huc8_count = n(),
            huc_8_list = paste(hu_8_name, collapse = ", "),
            .groups = "drop") %>% 
  arrange(desc(huc8_count)) %>% 
  filter(huc8_count > 1) %>% 
  left_join(., wr_info, by = "wr_id") %>% 
  select(wr_id, owner, wr_type, wr_status, huc8_count, huc_8_list)

# ggplot(multi_huc8_rights, aes(x = huc8_count)) + geom_bar()

# Create GeoJSON of PODs.

multi_huc8_rights_geo <- pods %>% 
  dplyr::filter(wr_id %in% multi_huc8_rights$wr_id)

plot(multi_huc8_rights_geo, pch = 20)


# Save resulting water right list.
if(!dir.exists("./output")) dir.create("./output")
f_name <- paste0("Water Rights with PODs in Multiple HUC-8s ",
                 Sys.Date(), ".xlsx")
openxlsx::write.xlsx(x = multi_huc8_rights,
                     file = paste0("./output/", f_name),
                     asTable = TRUE)

# Save shapefile of the resulting PODs.
if(!dir.exists("./pods")) dir.create("./pods")
writeOGR(obj = multi_huc8_rights_geo, 
         dsn = "pods", 
         layer = "pods", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)






































