library(httr)
library(jsonlite)
library(tidyjson)
library(tidyverse)
library(lubridate)
library(geosphere)

# File 1

file1 <- jsonlite::fromJSON(paste(readLines("livePositionJson/liveposition.json"), collapse=""), flatten = TRUE) %>% 
  as_tibble() %>% 
  mutate(time = anytime::anytime(time)) %>% 
  arrange(time)

singleFile <- file1 %>% 
  select(1:5)

Records <- file1$result

Result <- singleFile %>% 
  bind_cols(Records)

sum(!duplicated(Result$AssetID)) # unique

sum(duplicated(Result$AssetID)) # duplicate

remove <- c("/Date", "(", ")/", "[[:punct:]]")



File1_df <- Result %>% 
  mutate(DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove, collapse = "|")),
         DateTimeLocal = str_split_fixed(DateTimeLocal, 
                                         pattern = "\\+",
                                         n = 2)[1],
         DateTimeLocal = as.numeric(DateTimeLocal),
         DateTimeLocal = anytime::anytime(DateTimeLocal/1000),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           paste(remove, collapse = "|")),
         DateTimeReceived = str_split_fixed(DateTimeReceived, 
                                            pattern = "\\+",
                                            n = 2)[1],
         DateTimeReceived = as.numeric(DateTimeReceived),
         DateTimeReceived = anytime::anytime(DateTimeReceived/1000)) %>% 
  arrange(CabNumber, DateTimeLocal)

# sum(!duplicated(Result550$AssetID))
# 
# 
# write_csv(Result550, "livePosition-0601_550pm.csv")


# File 2

file2 <- jsonlite::fromJSON(paste(readLines("livePositionJson/livePosition0601_550.json"), collapse=""), flatten = TRUE) %>% 
  as_tibble() %>% 
  mutate(time = anytime::anytime(time)) %>% 
  arrange(time)

singleFile <- file2 %>% 
  select(1:5)

Records <- file2$result

Result <- singleFile %>% 
  bind_cols(Records)

sum(!duplicated(Result$AssetID)) # unique

sum(duplicated(Result$AssetID)) # duplicate

remove <- c("/Date", "(", ")/", "[[:punct:]]")



File2_df <- Result %>% 
  mutate(DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove, collapse = "|")),
         DateTimeLocal = str_split_fixed(DateTimeLocal, 
                                         pattern = "\\+",
                                         n = 2)[1],
         DateTimeLocal = as.numeric(DateTimeLocal),
         DateTimeLocal = anytime::anytime(DateTimeLocal/1000),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           paste(remove, collapse = "|")),
         DateTimeReceived = str_split_fixed(DateTimeReceived, 
                                            pattern = "\\+",
                                            n = 2)[1],
         DateTimeReceived = as.numeric(DateTimeReceived),
         DateTimeReceived = anytime::anytime(DateTimeReceived/1000)) %>% 
  arrange(CabNumber, DateTimeLocal)

# sum(!duplicated(Result550$AssetID))
# 
# 
# write_csv(Result550, "livePosition-0601_550pm.csv")



# File 3

file3 <- jsonlite::fromJSON(paste(readLines("livePositionJson/LivePosition0602-File1.json"), collapse=""), flatten = TRUE) %>% 
  as_tibble() %>% 
  mutate(time = anytime::anytime(time)) %>% 
  arrange(time)

singleFile <- file3 %>% 
  select(1:5)

Records <- file3$result

Result <- singleFile %>% 
  bind_cols(Records)

sum(!duplicated(Result$AssetID)) # unique

sum(duplicated(Result$AssetID)) # duplicate

remove <- c("/Date", "(", ")/", "[[:punct:]]")



File3_df <- Result %>% 
  mutate(DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove, collapse = "|")),
         DateTimeLocal = str_split_fixed(DateTimeLocal, 
                                         pattern = "\\+",
                                         n = 2)[1],
         DateTimeLocal = as.numeric(DateTimeLocal),
         DateTimeLocal = anytime::anytime(DateTimeLocal/1000),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                           paste(remove, collapse = "|")),
         DateTimeReceived = str_split_fixed(DateTimeReceived, 
                                            pattern = "\\+",
                                            n = 2)[1],
         DateTimeReceived = as.numeric(DateTimeReceived),
         DateTimeReceived = anytime::anytime(DateTimeReceived/1000)) %>% 
  arrange(CabNumber, DateTimeLocal)

# sum(!duplicated(Result550$AssetID))
# 
# 
# write_csv(Result550, "livePosition-0601_550pm.csv")

## merge file1 and file2

combine_df <- File1_df %>% 
  bind_rows(File2_df) %>% 
  bind_rows(File3_df)

## combine


live_position <- list.files("livePositionJson/",
                             full.names = TRUE,
                             include.dirs = TRUE) %>% 
  map_df(~jsonlite::fromJSON(paste(readLines(.), collapse=""), flatten = TRUE)) %>% 
  as_tibble() %>% 
  mutate(time = anytime::anytime(time)) %>% 
  arrange(time)


message <- live_position %>% 
  select(1:5)

records <- live_position$result

result_tbl <- message %>% 
  bind_cols(records)

sum(!duplicated(result_tbl$AssetID)) # unique

sum(duplicated(result_tbl$AssetID)) # duplicate

remove <- c("/Date", "(", ")/", "[[:punct:]]")



result_tbl2 <- result_tbl %>% 
  mutate(DateTimeLocal = str_remove_all(DateTimeLocal, 
                                        paste(remove, collapse = "|")),
         DateTimeLocal = str_split_fixed(DateTimeLocal, 
                  pattern = "\\+",
                  n = 2)[1],
         DateTimeLocal = as.numeric(DateTimeLocal),
         DateTimeLocal = anytime::anytime(DateTimeLocal/1000),
         DateTimeReceived = str_remove_all(DateTimeReceived, 
                                        paste(remove, collapse = "|")),
         DateTimeReceived = str_split_fixed(DateTimeReceived, 
                                         pattern = "\\+",
                                         n = 2)[1],
         DateTimeReceived = as.numeric(DateTimeReceived),
         DateTimeReceived = anytime::anytime(DateTimeReceived/1000)) %>% 
  arrange(CabNumber, DateTimeLocal)
         
sum(!duplicated(result_tbl2$AssetID))


                          
write_csv(result_tbl2, "livePosition-0601.csv")


## some analysis here

options(scipen = 999)

sub_df <- combine_df %>% 
  filter(AssetStatus == "InService", Longitude > 0, 
         Latitude > 0) %>% 
  select(DateTimeLocal, DateTimeReceived, AssetID, Longitude, Latitude, ActualSpeed, Distance) %>% 
  arrange(AssetID, DateTimeLocal) %>% 
  group_by(AssetID) %>% 
  mutate(.before = AssetID, TimeDiff = as.numeric(difftime(DateTimeLocal, lag(DateTimeLocal, default = first(DateTimeLocal)), units = "mins")),
         longlat = matrix(c(Longitude, Latitude), ncol = 2),
         DistCovered = distGeo(longlat, lag(longlat)))




#distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)