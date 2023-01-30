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
  janitor
)



tic("Load the json files")

historical <- list.files("../IbeseLivePosition/data/",
                         full.names = TRUE,
                         include.dirs = TRUE) %>%
  tail(., 350) %>% 
  # .[1:90] %>% 
  map_df(~jsonlite::fromJSON(paste(readLines(.), collapse=""), flatten = TRUE)) 

toc()

historical_tbl <- historical %>% 
  as_tibble()


message <- historical_tbl %>% 
  select(1:5)

response <- historical_tbl$result

all_live_position <- message %>% 
  bind_cols(response) %>% 
  select(-time, -AltitudeUnits, -AnalogueInput0, -AnalogueInput1, -AnalogueInput4, -AssetID,
         -BatteryPercentage, -CabNumber, -Country, -DestinationCountry, -DigitalInput0,
         -DigitalInput3, -DigitalOutput1, -DriverForename, -DriverMobileNumber,
         -DriverSurname, -SiteID, -OBDStatus.AssetID, -OBDStatus.FuelLevelRemaining,
         -OBDStatus.LocationID, -OBDStatus.OBDStatusID, -OBDStatus.UTCDateTime) %>% # drop dummy TripID
  filter(!between(TripID, 9000000000, Inf)) %>% 
  filter(!TripID == 0) %>% 
  remove_constant() %>% 
  remove_empty()


# sum(!duplicated(all_live_position$AssetID)) # unique
# 
# sum(duplicated(all_live_position$AssetID)) # duplicate

  

## Data cleaning

remove <- c("/Date", "(", ")/", "[[:punct:]]")

remove2 <- c("+0100", "+0000")


# remove constant and empty columns; also variables that has 1-1 relationship
# with another variable


clean_tbl <- all_live_position %>% 
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
         LastIgnitionOn = anytime(LastIgnitionOn))
  

# 
# ibese_tbl <- clean_tbl %>% 
#   filter(str_detect(Geofence, "Ibese"))


## Analysis

# check zeros long and lat!
# check for the actual arrival time! (time at which the truck entered the plant geo-fence)

# trips_with_zero_location <- clean_tbl %>% 
#   filter(AssetStatus == "InService") %>% 
#   filter(Longitude == 0 | Latitude == 0) %>% 
#   mutate(time = anytime(time)) %>% 
#   select(Reference, AssetID, DateTimeReceived, Longitude, Latitude) %>% 
#   group_by(Reference) %>% 
#   arrange(Reference, DateTimeReceived)




options(scipen = 999)

ibese <- matrix(c(3.043568,7.006293), ncol = 2)

prediction_df <- clean_tbl %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0, TripID != 0) %>% 
  select(TripID, Reference, DateTimeLocal, DateTimeReceived, 
         Longitude, Latitude) %>% 
  arrange(Reference, DateTimeReceived) %>% 
  group_by(TripID, Reference) %>% 
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
  do(tail(.,2)) %>% 
  mutate(Direction = if_else(lag(DistToPlant) < DistToPlant, "Inbound", "Outbound")) %>% 
  filter(Direction == "Inbound") %>% 
  mutate(AverageSpeed = DistCovered/TimeDiff,
         TimetoPlant = DistToPlant/AverageSpeed,
         ArrivalTime = DateTimeReceived + seconds(TimetoPlant * 3600)) %>% 
  select(TripID, Reference, ArrivalTime, DistToPlant) %>% 
  mutate(ArrivalTime = round_date(ArrivalTime, unit = "hour")) %>% 
  #filter(as.Date(ArrivalTime) == today()) %>% 
  arrange(ArrivalTime, Reference)

#write_csv(sampled_df, "Arrival.csv")


# examine the actual time the truck entered into the plant geo-fence while returning to the plant




ibese_geofence <- c("Ibese", "IBESE", "Vehicle Park", "Vehicle Park 2")


inbound_df <- clean_tbl %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0) %>% 
  filter(!between(TripID, 9000000000, Inf)) %>% 
  filter(TripID != 0) %>% 
  select(Reference, DateTimeReceived, 
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

#check_df <- inbound_df[row_id,]


# position <- inbound_df %$% matrix(c(Longitude, Latitude), ncol = 2)
# 
# geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# position_sf <- SpatialPoints(position, 
#                              proj4string=CRS(geo_proj)) %>% 
#   st_as_sf()
# 
# row_id <- st_intersects(buffer, position_sf)[[1]]
# 
# check_df <- inbound_df[row_id,]

Actual_tbl <- inbound_df %>%
  .[row_id,] %>% 
  #filter(str_detect(Geofence, paste(ibese_geofence, collapse = "|"))) %>% 
  filter(DateTimeReceived == min(DateTimeReceived)) %>% 
  # do(tail(.,2)) %>% 
  mutate(Date = as.Date(DateTimeReceived)) %>% 
  #filter(Date == as.Date("2022-06-15")) %>% 
  select(TripID, Reference, DateTimeReceived, 
         Longitude, Latitude, Geofence) %>% 
  mutate(Time = round_date(DateTimeReceived, unit = "hour")) %>% 
  arrange(DateTimeReceived)
  
set.seed(1234)

sample_df <- Actual_tbl %>% 
  ungroup() %>% 
  filter(TripID != 0) %>% 
  filter(DateTimeReceived > as.Date("2022-06-22")) %>% 
  slice_sample(n = 10) %>% 
  select(TripID, Reference, AssetID)



hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)


write.xlsx(current_tbl,
           file = "livePosition-2022-07-15 12-33-15-912475.xlsx",
           colNames = TRUE, borders = "rows", headerStyle = hs
)


clean_tbl %>% 
  filter(Reference == "XA295WNN") %>% 
  View(.)



library(arrow)


tic()

df <- list.files("../IbeseLivePosition/data_parquet/",
                 full.names = TRUE,
                 include.dirs = TRUE) %>%
  map_df(~read_parquet(., as_tibble = TRUE))

toc()

tic()

df2 <- jsonlite::fromJSON(paste(readLines("../IbeseLivePosition/data/livePosition-2022-06-10 08-00-10-937088.json"), collapse=""), flatten = TRUE)

toc()


# ParquetFile_IbeseLivePositionJob
# 
# This job will run a python script to ping an API and save the response body as a parquet file with the timestamp
# 
# Browse: C:ProgramDataAnaconda3pythonw.exe
# Add Argument: "IbeseLivePositionScript.py"
# start in : C:Usersolumide.oyalolaDocumentsWorkingDirectoryDangoteIbeseLivePosition
