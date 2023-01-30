#R vetiver

# Wed Jul 13 14:56:02 2022 ------------------------------

if(!require(pacman))install.packages("pacman")

pacman::p_load(
  vetiver,
  parsnip,
  workflows,
  pins,
  plumber,
  tidyverse
)

data(Sacramento, package = "modeldata")

rf_spec <- rand_forest(mode = "regression")

rf_form <- price ~ type + sqft + beds + baths

rf_fit <- workflow(rf_form, rf_spec) %>% 
  fit(Sacramento)

v <- vetiver_model(rf_fit, "sacramento_rf")

v

model_board <- board_temp()
model_board %>% vetiver_pin_write(v)

# deploy your pinned vetiver_model via a plumber API

pr() %>% 
  vetiver_api(v) %>% 
  pr_run(port = 7777)

# If the deployed model endpoint is running via one R process (either remotely on a server or locally, perhaps via a background job in the RStudio IDE), you can make predictions with that deployed model and new data in another, separate R process. First, create a model endpoint:

endpoint <- vetiver_endpoint("http://127.0.0.1:7777/predict")

endpoint

# Such a model API endpoint deployed with vetiver will return predictions for appropriate new data.

new_sac <- Sacramento %>% 
  slice_sample(n = 20) %>% 
  select(type, sqft, beds, baths)

predict(endpoint, new_sac)
