#!/usr/bin/env Rscript


# Title: Truck Live Position Monitoring
# File: LivePosition-v4.R
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
  leaflet,
  magrittr,
  janitor,
  arrow
)

#inservice - tripid == 0; the tripid changes from zero
# the truck is expected to be within the geofence of the plant.
# use 2km buffer
# the time elapse from when the tripID=0 to when it turned to another value except zero.

## 2 weeks files

# Read json Files

tic("read two weeks data: ")

historical <- list.files("../IbeseLivePosition/data/",
                         full.names = TRUE,
                         include.dirs = TRUE) %>%
  tail(., 545) %>% 
  # .[1:90] %>% 
  map_df(~jsonlite::fromJSON(paste(readLines(.), collapse=""), flatten = TRUE)) 

toc()

historical_tbl <- historical%>% 
  as_tibble() %>% 
  mutate(time = anytime::anytime(time)) %>% 
  arrange(time)

message <- historical_tbl %>% 
  select(1:5)

response <- historical_tbl$result

all_live_position <- message %>% 
  bind_cols(response)


emptycols <- sapply(all_live_position, function (k) all(is.na(k)))

all_live_position_v2 <- all_live_position[!emptycols] %>% # remove empty cols
  janitor::remove_constant(.)

remove <- c("/Date", "(", ")/", "[[:punct:]]")

remove2 <- c("\\+0100", "\\+0000")

var <- c("ActualSpeed", "Address", "Altitude", "AssetClass", "AssetLocationID", "AssetStatus", "CategoryID", "CategoryName", "City", "CustomerName", "DateTimeLocal", "DateTimeReceived", "DeliveryOrderNumber", "DepartureDateTime", "DestinationArea", "DestinationCity", "DestinationSite", "DestinationStreet", "DeviceType", "DirectionString", "Distance", "DriverCode", "DriverID", "Geofence", "IgnitionStatus", "Information", "JourneyDistance", "JourneyDuration", "JourneyIdleTime", "JourneyMaxSpeed", "LastIgnitionOff", "LastIgnitionOn", "Latitude", "Load", "Longitude", "NumSatellites", "Odometer", "Reason", "ReasonString", "Reference", "Region", "SiteName", "SpeedOverGround", "Status", "Street", "TrackTrue", "TripDestination", "TripID", "TripSource", "TripType", "UTCDateTime", "WaybillNumber")

all_live_position_v3 <- all_live_position_v2 %>% 
  #filter(AssetClass %in% c(34, 42)) %>% 
  mutate(DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove, collapse = "|")),
         DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove2, collapse = "|")),
         DateTimeLocal = as.numeric(DateTimeLocal),
         DateTimeLocal = DateTimeLocal/1000,
         DateTimeLocal = anytime(DateTimeLocal),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           paste(remove, collapse = "|")),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           paste(remove2, collapse = "|")),
         DateTimeReceived = as.numeric(DateTimeReceived),
         DateTimeReceived = DateTimeReceived/1000,
         DateTimeReceived = anytime(DateTimeReceived),
         DepartureDateTime = str_remove_all(DepartureDateTime, 
                                            paste(remove, collapse = "|")),
         DepartureDateTime = str_remove_all(DepartureDateTime, 
                                            paste(remove2, collapse = "|")),
         DepartureDateTime = as.numeric(DepartureDateTime),
         DepartureDateTime = DepartureDateTime/1000,
         DepartureDateTime = anytime(DepartureDateTime),
         UTCDateTime = str_remove_all(UTCDateTime, 
                                      paste(remove, collapse = "|")),
         UTCDateTime = str_remove_all(UTCDateTime, 
                                      paste(remove2, collapse = "|")),
         UTCDateTime = as.numeric(UTCDateTime),
         UTCDateTime = UTCDateTime/1000,
         UTCDateTime = anytime(UTCDateTime),
         LastIgnitionOff = str_remove_all(LastIgnitionOff, 
                                          paste(remove, collapse = "|")),
         LastIgnitionOff = str_remove_all(LastIgnitionOff, 
                                          paste(remove2, collapse = "|")),
         LastIgnitionOff = as.numeric(LastIgnitionOff),
         LastIgnitionOff = LastIgnitionOff/1000,
         LastIgnitionOff = anytime(LastIgnitionOff),
         LastIgnitionOn = str_remove_all(LastIgnitionOn, 
                                         paste(remove, collapse = "|")),
         LastIgnitionOn = str_remove_all(LastIgnitionOn, 
                                         paste(remove2, collapse = "|")),
         LastIgnitionOn = as.numeric(LastIgnitionOn),
         LastIgnitionOn = LastIgnitionOn/1000,
         LastIgnitionOn = anytime(LastIgnitionOn)) %>% 
  select(all_of(var)) %>% # drop dummy TripID
  filter(!between(TripID, 9000000000, Inf))

write_parquet(all_live_position_v3, "../IbeseLivePosition/data_parquet/livePosition-archive2.parquet")



saveRDS(object = all_live_position_v3, file = "2wks-All-LivePosition.RDS")


hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)


write_csv(all_live_position_v3,
          file = "historical-livePosition-data.csv"
)

# Filter out records with TripID = 0 and put them in a separate file.

tripID_is_zero <- all_live_position_v3 %>% 
  filter(TripID == 0)

saveRDS(object = tripID_is_zero, file = "tripIDisZero.RDS")


write_csv(tripID_is_zero,
          file = "TripID-is-zero.csv"
)

# Actual Arrival Time


ibese_geofence <- c("Ibese", "IBESE", "Vehicle Park", "Vehicle Park 2")
ibese <- matrix(c(3.043568,7.006293), ncol = 2)


inbound_df <- all_live_position_v3 %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0) %>% 
  filter(!between(TripID, 9000000000, Inf)) %>%
  filter(!TripID == 0) %>% 
  select(Reference, AssetClass, DateTimeReceived, 
         Longitude, Latitude, TripID, Geofence) %>% 
  arrange(Reference, DateTimeReceived) %>% 
  group_by(Reference, TripID) %>% 
  mutate(.after = DateTimeReceived,
         TimeDiff = as.numeric(difftime(DateTimeReceived, 
                                        lag(DateTimeReceived, 
                                            default = first(DateTimeReceived)), 
                                        units = "hours")),
         LongLat = matrix(c(Longitude, Latitude), ncol = 2),
         DistCovered = distGeo(LongLat, lag(LongLat)),
         DistCovered = DistCovered/1000,
         DistToPlant = distGeo(LongLat, ibese),
         DistToPlant = DistToPlant/1000) %>%
  mutate(Direction = if_else(lag(DistToPlant) < DistToPlant, "Inbound", "Outbound")) %>% 
  filter(Direction == "Inbound")


Actual_tbl <- inbound_df %>%  
  filter(str_detect(Geofence, paste(ibese_geofence, collapse = "|"))) %>% 
  filter(DateTimeReceived == min(DateTimeReceived)) %>% 
  # do(tail(.,2)) %>% 
  mutate(Date = as.Date(DateTimeReceived)) %>% 
  #filter(Date == as.Date("2022-06-15")) %>% 
  select(TripID, Reference, DateTimeReceived, 
         Longitude, Latitude, Geofence) %>% 
  mutate(Time = round_date(DateTimeReceived, unit = "hour")) %>% 
  arrange(DateTimeReceived)




