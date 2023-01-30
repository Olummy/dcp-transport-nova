
library(httr)
library(jsonlite)
library(tidyjson)
library(tidyverse)



historical_tbl <- list.files("Historical_JSON/",
                          full.names = TRUE,
                          include.dirs = TRUE) %>% 
  map_df(~jsonlite::fromJSON(paste(readLines(.), collapse=""), flatten = TRUE)) %>% 
  as_tibble() %>% 
  mutate(time = anytime::anytime(time)) %>% 
  arrange(time)


message <- historical_tbl %>% 
  select(1:5)

records <- historical_tbl$result

result_tbl <- message %>% 
  bind_cols(records)

sum(!duplicated(result_tbl$TripID)) # no duplicates

write_csv(result_tbl, "Historical Trip Data-May2022.csv")
