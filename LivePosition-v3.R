#!/usr/bin/env Rscript


# Title: Truck Live Position Monitoring
# File: LivePosition-v3.R
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
  openxlsx
)


# Read csv Files

tic("Load the csv files")
live_position <- list.files("../LivePositionNewman/newman/",
                            full.names = TRUE,
                            include.dirs = TRUE) %>% 
            map_df(~ read_csv(.))
toc()

tic("convert the json object to data.frame")

body_df <- map_df(live_position$body, jsonlite::fromJSON)

toc()

status_response <- body_df %>% 
  select(1:5)

results <- body_df$result

result_tbl <- status_response %>% 
  bind_cols(results)

sum(!duplicated(result_tbl$AssetID)) # unique

sum(duplicated(result_tbl$AssetID)) # duplicate



## Data cleaning

remove <- c("/Date", "(", ")/", "[[:punct:]]")



all_live_position <- result_tbl %>% 
  mutate(DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove, collapse = "|")),
         DateTimeLocal = str_split_fixed(DateTimeLocal, 
                                         pattern = "\\+",
                                         n = 2)[1],
         DateTimeLocal = as.numeric(DateTimeLocal),
         DateTimeLocal = anytime(DateTimeLocal/1000),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           paste(remove, collapse = "|")),
         DateTimeReceived = str_split_fixed(DateTimeReceived, 
                                            pattern = "\\+",
                                            n = 2)[1],
         DateTimeReceived = as.numeric(DateTimeReceived),
         DateTimeReceived = anytime(DateTimeReceived/1000))


## Analysis

# check zeros long and lat!
# check for the actual arrival time! (time at which the truck entered the plant geo-fence)

trips_with_zero_location <- all_live_position %>% 
  filter(AssetStatus == "InService") %>% 
  filter(Longitude == 0 | Latitude == 0) %>% 
  mutate(time = anytime(time)) %>% 
  select(Reference, AssetID, time, Longitude, Latitude) %>% 
  group_by(Reference) %>% 
  arrange(Reference, time)

options(scipen = 999)

ibese <- matrix(c(3.043568,7.006293), ncol = 2)

prediction_df <- all_live_position %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0) %>% 
  select(Reference, AssetID, time, DateTimeLocal, DateTimeReceived, 
         Longitude, Latitude) %>% 
  arrange(Reference, time, DateTimeLocal) %>% 
  group_by(Reference, AssetID) %>% 
  mutate(.after = time,
         time = anytime(time),
         TimeDiff = as.numeric(difftime(time, 
                                        lag(time, 
                                            default = first(time)), 
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
         ArrivalTime = time + seconds(TimetoPlant * 3600)) %>% 
  select(Reference, AssetID, ArrivalTime) %>% 
  mutate(ArrivalTime = round_date(ArrivalTime, unit = "hour")) %>% 
  #filter(as.Date(ArrivalTime) == today()) %>% 
  arrange(ArrivalTime, Reference, AssetID)

#write_csv(sampled_df, "Arrival.csv")


geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

ibese_sp <- SpatialPoints(ibese, 
                          proj4string=CRS(geo_proj)) %>% 
  st_as_sf()


buffer <- st_buffer(ibese_sp, 5000) # 5km buffer

position <- matrix(c(all_live_position$Longitude, all_live_position$Latitude), ncol = 2)

position_sf <- SpatialPoints(position, 
                             proj4string=CRS(geo_proj)) %>% 
  st_as_sf()

row_id <- st_intersects(buffer, position_sf)[[1]]

check_df <- all_live_position[row_id,]


# examine the actual time the truck entered into the plant geo-fence while returning to the plant

Actual_df <- check_df %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0) %>% 
  select(Reference, AssetID, time, DateTimeLocal, DateTimeReceived, 
         Longitude, Latitude) %>% 
  arrange(Reference, time, DateTimeLocal) %>% 
  group_by(Reference, AssetID) %>% 
  mutate(.after = time,
         time = anytime(time),
         TimeDiff = as.numeric(difftime(time, 
                                        lag(time, 
                                            default = first(time)), 
                                        units = "hours")),
         LongLat = matrix(c(Longitude, Latitude), ncol = 2),
         DistCovered = distGeo(LongLat, lag(LongLat)),
         DistCovered = DistCovered/1000,
         DistToPlant = distGeo(LongLat, ibese),
         DistToPlant = DistToPlant/1000) %>%
  do(tail(.,2)) %>% 
  mutate(Direction = if_else(lag(DistToPlant) < DistToPlant, "Inbound", "Outbound")) %>% 
  filter(Direction == "Inbound") %>% 
  select(Reference, AssetID, time) %>% 
  mutate(time = round_date(time, unit = "hour")) %>% 
  arrange(time)



joined_df <- Actual_df %>% 
  inner_join(prediction_df) %>% 
  rename(Actual = time, 
         Prediction = ArrivalTime)


hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)


write.xlsx(joined_df,
           file = "Predicted_vs_Actual.xlsx",
           colNames = TRUE, borders = "rows", headerStyle = hs
)

