library(dplyr)
library(sf)
library(leaflet)

dsn <- "./common/CA_HUC-8_Watersheds"
layer <- "CA_HUC-8_Watersheds"

## Using sf

huc8_sf_layer <- read_sf(dsn = dsn,
                      layer = layer)
huc8_sf_layer <- st_transform(huc8_sf_layer,
                           crs = 4326)
huc8_sf_layer <- huc8_sf_layer %>% 
  select(huc8_name = name,
          shape_Leng,
         shape_Area,
         geometry)


leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addPolygons(data = huc8_sf_layer,
              weight = 2,
              col = "blue",
              fill = TRUE,
              fillOpacity = 0,
              label = huc8_sf_layer$huc8_name,
              labelOptions = labelOptions(textsize = "12px",
                                          sticky = TRUE))
