#!/usr/bin/env Rscript


# Purpose: Using the stacked ensemble tidymodels to predict the arrival of the daily inbound trucks
# File: get-data-to-predict.R
# Project: Prediction of truck arrival time to the source plant

# clear the workspace

rm(list = ls())


if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  arrow,
  tidymodels,
  bonsai,
  modeltime,
  tidyverse,
  stacks,
  tictoc,
  vetiver,
  pins,
  lubridate,
  plumber,
  openxlsx,
  DALEX,
  DALEXtra
)


tidymodels_prefer()
options(scipen = 999)


tbl <- read_parquet("../IbeseLivePosition/ml_data/trips_to_predict.parquet", as_tibble = TRUE)


b <- board_folder(path = "pins-r")

v_dce6f <- vetiver_pin_read(board = b, name = "dcp_ibese_truck_arrival", version = "20220916T114219Z-dec6f")



predict_df <- tbl %>% 
  bind_cols(predict(v_dce6f, .)) %>% 
  mutate(Predicted = DateTimeReceived + seconds(.pred), .after = Reference) %>%
  select(TripID, Reference, Predicted, Latitude, Longitude, DistToPlant) %>% 
  group_by(TripID, Reference) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(as.Date(Predicted) == today() + days(1)) %>% 
  .[order(.$Predicted),]


hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)

filename <- paste0("Ibese Daily Inbound Trucks Arrival Prediction-", strftime(today(), format = "%m-%d-%y"), ".xlsx")

write.xlsx(predict_df,
           file = filename,
           colNames = TRUE, borders = "rows", headerStyle = hs
)



