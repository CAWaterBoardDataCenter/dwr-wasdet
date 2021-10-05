#library(rgdal)
library(sf)
library(geojsonio)


huc8_polygons <- read_sf(dsn = "./gis/CA HUC-8 Watersheds",
                         layer = "CA_HUC-8_Watersheds")
huc8_polygons <- st_transform(huc8_polygons,
                             crs = 4326)

plot(st_geometry(huc8_polygons))
