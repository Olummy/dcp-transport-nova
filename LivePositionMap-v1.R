#!/usr/bin/env Rscript


# Title: Truck Live Position Monitoring on Map
# File: LivePositionMap-v1.R
# Project: Prediction of truck arrival time to the source plant


# Install pacman package if needed
if(!require("pacman")) install.packages("pacman")

# load the required packages
pacman::p_load(
  httr,
  jsonlite,
  tidyjson,
  tidyverse,
  lubridate,
  geosphere,
  anytime,
  tictoc,
  stringi,
  maptools,
  geosphere,
  sf,
  sp,
  openxlsx,
  leaflet
)

#3.043485
#7.008686

geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"



3.043745
7.006449

3.043238
7.019887

#point <- matrix(c(3.043485, 7.008686), ncol = 2)

point2 <- matrix(c(3.043238, 7.019887), ncol = 2)


point_spatial <- SpatialPoints(point2, proj4string=CRS(geo_proj)) %>% 
  st_as_sf()

ibese_sp <- SpatialPoints(ibese, proj4string=CRS(geo_proj)) %>% 
  st_as_sf()

buffer_25km <- st_buffer(ibese_sp, 3000)

st_intersects(buffer_25km, point_spatial)

#YKS95XA
#YKS858XA

YKS858XA_df <- clean_tbl %>% 
  filter(TripID == "3000489311") 

leaflet(data = YKS858XA_df) %>% 
  addTiles(group = "OSM") %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>% 
  addMarkers(~Longitude,
             ~Latitude, popup = ~as.character(DateTimeReceived),
             label = ~as.character(DateTimeReceived))



leaflet(data = check_df) %>% 
  addTiles(group = "OSM") %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>% 
  addMarkers(~Longitude,
             ~Latitude, popup = ~as.character(DateTimeReceived),
             label = ~as.character(DateTimeReceived))



leaflet(data = as.data.frame(point)) %>% 
  addTiles(group = "OSM") %>%
  addCircles(~point[1,1],
             ~point[1,2], popup = ~as.character("Ibese Plant"),
             label = ~as.character("Ibese Plant"))


leaflet(data = quakes[1:20,]) %>% 
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>% 
  addCircleMarkers(~long, ~lat, popup = ~as.character(mag),
                   label = ~as.character(mag))