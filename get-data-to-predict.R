#!/usr/bin/env Rscript


# Title: Collect the inbound trucks data and predict the arrival time
# File: get-data-to-predict.R
# Project: Prediction of truck arrival time to the source plant

# clear the workspace

rm(list = ls())

# Install pacman package if needed
if(!require("pacman")) install.packages("pacman")

# load the required packages

pacman::p_load(
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
  leaflet,
  magrittr,
  janitor,
  arrow,
  magrittr
)


# load parquet files as tibble

tic("Load the parquet files")

df <- list.files("../IbeseLivePosition/data_parquet/",
                 full.names = TRUE,
                 include.dirs = TRUE) %>%
  tail(., 1) %>% 
  map_df(~read_parquet(., as_tibble = TRUE))

toc()

# data cleaning

remove <- c("/Date", "(", ")/", "[[:punct:]]")

var <- c("ActualSpeed", "Address", "Altitude", "AssetClass", "AssetLocationID", "AssetStatus", "CategoryID", "CategoryName", "City", "CustomerName", "DateTimeLocal", "DateTimeReceived", "DeliveryOrderNumber", "DepartureDateTime", "DestinationArea", "DestinationCity", "DestinationSite", "DestinationStreet", "DeviceType", "DirectionString", "Distance", "DriverCode", "DriverID", "Geofence", "IgnitionStatus", "Information", "JourneyDistance", "JourneyDuration", "JourneyIdleTime", "JourneyMaxSpeed", "LastIgnitionOff", "LastIgnitionOn", "Latitude", "Load", "Longitude", "NumSatellites", "Odometer", "Reason", "ReasonString", "Reference", "Region", "SiteName", "SpeedOverGround", "Status", "Street", "TrackTrue", "TripDestination", "TripID", "TripSource", "TripType", "UTCDateTime", "WaybillNumber")



df %<>% 
  dplyr::select(all_of(var)) %>% 
  mutate(DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove, collapse = "|")),
         DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        "\\+0100|\\+0000"),
         DateTimeLocal = as.numeric(DateTimeLocal),
         DateTimeLocal = DateTimeLocal/1000,
         DateTimeLocal = anytime(DateTimeLocal),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           paste(remove, collapse = "|")),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           "\\+0100|\\+0000"),
         DateTimeReceived = as.numeric(DateTimeReceived),
         DateTimeReceived = DateTimeReceived/1000,
         DateTimeReceived = anytime(DateTimeReceived),
         DepartureDateTime = str_remove_all(DepartureDateTime, 
                                            paste(remove, collapse = "|")),
         DepartureDateTime = str_remove_all(DepartureDateTime, 
                                            "\\+0100|\\+0000"),
         DepartureDateTime = as.numeric(DepartureDateTime),
         DepartureDateTime = DepartureDateTime/1000,
         DepartureDateTime = anytime(DepartureDateTime),
         UTCDateTime = str_remove_all(UTCDateTime, 
                                      paste(remove, collapse = "|")),
         UTCDateTime = str_remove_all(UTCDateTime, 
                                      "\\+0100|\\+0000"),
         UTCDateTime = as.numeric(UTCDateTime),
         UTCDateTime = UTCDateTime/1000,
         UTCDateTime = anytime(UTCDateTime),
         LastIgnitionOff = str_remove_all(LastIgnitionOff, 
                                          paste(remove, collapse = "|")),
         LastIgnitionOff = str_remove_all(LastIgnitionOff, 
                                          "\\+0100|\\+0000"),
         LastIgnitionOff = as.numeric(LastIgnitionOff),
         LastIgnitionOff = LastIgnitionOff/1000,
         LastIgnitionOff = anytime(LastIgnitionOff),
         LastIgnitionOn = str_remove_all(LastIgnitionOn, 
                                         paste(remove, collapse = "|")),
         LastIgnitionOn = str_remove_all(LastIgnitionOn, 
                                         "\\+0100|\\+0000"),
         LastIgnitionOn = as.numeric(LastIgnitionOn),
         LastIgnitionOn = LastIgnitionOn/1000,
         LastIgnitionOn = anytime(LastIgnitionOn)) %>% 
  filter(!between(TripID, 9000000000, Inf)) %>% 
  filter(!TripID == 0)

df %<>% mutate(TripID = as.numeric(TripID))

#TODO: Automate the workflow


options(scipen = 999)

ibese <- matrix(c(3.043568,7.006293), ncol = 2)


ibese_geofence <- c("Ibese", "IBESE", "Vehicle Park", "Vehicle Park 2")


inbound_df <- df %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0) %>% 
  filter(!between(TripID, 9000000000, Inf)) %>% 
  filter(TripID != 0) %>% 
  select(Reference, DateTimeReceived, 
         Longitude, Latitude, TripID, Geofence, Altitude) %>% 
  arrange(Reference, DateTimeReceived) %>% 
  group_by(Reference, TripID) %>% 
  mutate(.after = DateTimeReceived,
         TimeDiff = as.numeric(difftime(DateTimeReceived, 
                                        dplyr::lag(DateTimeReceived, 
                                            default = first(DateTimeReceived)), 
                                        units = "hours")),
         LongLat = matrix(c(Longitude, Latitude), ncol = 2),
         DistCovered = distGeo(LongLat, dplyr::lag(LongLat)),
         DistCovered = DistCovered/1000,
         DistToPlant = distGeo(LongLat, ibese),
         DistToPlant = DistToPlant/1000) %>%
  mutate(Direction = if_else(dplyr::lag(DistToPlant) < DistToPlant, "Inbound", "Outbound")) %>% 
  filter(Direction == "Inbound") %>% 
  filter(TripID != 3000071349)


geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


position <- inbound_df %$% 
  matrix(c(Longitude, Latitude), ncol = 2)

position_sf <- SpatialPoints(position, 
                             proj4string=CRS(geo_proj)) %>% 
  st_as_sf()

plant <- matrix(c(3.05120587348938, 7.00300851960855), ncol = 2)

ibese_sp <- SpatialPoints(plant, proj4string=CRS(geo_proj)) %>% 
  st_as_sf()

plant_buffer <- st_buffer(ibese_sp, 3000)


row_id <- st_intersects(plant_buffer, position_sf)[[1]]



Actual_tbl <- inbound_df %>%
  ungroup() %>% 
  .[row_id,] %>% 
  arrange(TripID, Reference, DateTimeReceived) %>% 
  group_by(TripID, Reference) %>% 
  filter(row_number()== 1) %>% 
  #filter(str_detect(Geofence, paste(ibese_geofence, collapse = "|"))) %>% 
  #filter(min(DateTimeReceived)) %>% 
  #do(tail(.,2)) %>% 
  mutate(Date = as.Date(DateTimeReceived)) %>% 
  #filter(Date == as.Date("2022-06-15")) %>% 
  select(TripID, Reference, DateTimeReceived, 
         Longitude, Latitude, Geofence) %>% 
  mutate(Time = round_date(DateTimeReceived, unit = "hour")) %>% 
  rename(Arrival = DateTimeReceived) %>% 
  #do(head(., 1))
  slice_head(n = 1) %>% 
  filter(TripID != 3000071349)


breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")



## Inbound trips that are yet to arrive

trips_to_predict <- inbound_df %>% ungroup() %>% 
  anti_join(Actual_tbl %>% ungroup() %>% select(TripID), by = "TripID")

trips_to_predict %<>% 
  select(-LongLat) %>% 
  mutate(Day = wday(DateTimeReceived, label = TRUE, abbr = TRUE), .after = DateTimeReceived,
         Month = month(DateTimeReceived, label = TRUE, abbr = TRUE),
         Hour = hour(DateTimeReceived),
         TimeOfDay = cut(x= hour(DateTimeReceived), breaks = breaks, labels = labels, include.lowest=TRUE)) %>% 
  select(-Direction) %>% 
  filter(DistToPlant > 0)
  #filter(between(DistToPlant, 20, 120))


trips_to_predict %>% 
  write_parquet("../IbeseLivePosition/ml_data/trips_to_predict.parquet")

# get the inbound trucks arrival
source("C:/Users/olumide.oyalola/Documents/WorkingDirectory/Dangote/DCP-Transport-Nova/dcp-ibese-truck-arrival-prediction.R", echo=FALSE)

