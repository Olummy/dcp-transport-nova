library(shiny)
library(tidymodels)
library(vetiver)
library(pins)
library(arrow)
library(tidyverse)
library(bonsai)
library(stacks)
library(DT)
library(lubridate)



# Load the model
b <- board_folder(path = "../pins-r/")
model <- vetiver_pin_read(board = b, name = "dcp_ibese_truck_arrival", 
                          version = "20230110T094207Z-69661")


# Define the UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "journal"), #darkly, sandstone, flatly, united
  # App title ----
  titlePanel("Hello Trip Arrival!"),
  # Sidebar layout with input and output definitions ----
    fluidRow(
      column(# Input: File upload
        4, fileInput("file_path", label = "Select data to Predict",
                  multiple = FALSE,
                  accept = c(".parquet"), width = "100%"))),
    
    # Main panel for displaying outputs ----
      fluidRow(
        column(10, DT::dataTableOutput(outputId = "prediction", width = "100%"))
      ),
  fluidRow(
    column(# Button
      3, downloadButton("downloadData", "Download the Predictions", width = "100%"))
  )
    )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  reactiveDF <- reactive({
    req(input$file_path)
    test_df <- arrow::read_parquet(input$file_path$datapath, as_tibble = TRUE)
    return(test_df)
  })
  
  # output$actual <- DT::renderDataTable({
  #   
  #   req(input$file_path)
  #   
  #   return(DT::datatable(reactiveDF(), options = list(pageLength = 5), filter = c('top')))
    
 # })
  
  
  reactiveModel <- reactive({
    modelPrediction <- reactiveDF() %>% 
      bind_cols(predict(model, .)) %>% 
      mutate(Predicted = DateTimeReceived + seconds(.pred), .after = Reference) %>%
      select(TripID, Reference, Predicted, Latitude, Longitude, DistToPlant) %>% 
      group_by(TripID, Reference) %>% 
      slice_tail(n = 1) %>% 
      ungroup() %>% 
      #filter(as.Date(Predicted) == today() + days(1)) %>% 
      .[order(.$Predicted),]
    return(modelPrediction)
  })
  
  output$prediction <- DT::renderDataTable({
    
   req(input$file_path)
    
    return(DT::datatable(reactiveModel(), options = list(pageLength = 5), filter = c('top')))
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveModel(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)
