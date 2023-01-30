
# Title: Truck Live Position Monitoring
# File: LivePosition-v2.R
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
  tictoc
)


# Read Json Files

tic("Load the json files")

live_position <- list.files("livePositionJson/",
                            full.names = TRUE,
                            include.dirs = TRUE) %>% 
  map_df(~jsonlite::fromJSON(paste(readLines(.), collapse=""), 
                             flatten = TRUE)) %>% 
  as_tibble() %>% 
  mutate(time = anytime::anytime(time)) %>% 
  arrange(time)

toc()


status_response <- live_position %>% 
  select(1:5)

results <- live_position$result

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

sampled_df <- all_live_position %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0) %>% 
  select(Reference, AssetID, time, DateTimeLocal, DateTimeReceived, 
         Longitude, Latitude, ActualSpeed, Distance) %>% 
  arrange(Reference, time, DateTimeLocal) %>% 
  group_by(Reference, AssetID) %>% 
  mutate(.after = time, 
         TimeDiff = as.numeric(difftime(time, 
                                        lag(time, 
                                            default = first(time)), 
                                        units = "mins")),
         longlat = matrix(c(Longitude, Latitude), ncol = 2),
         DistCovered = distGeo(longlat, lag(longlat)))

