# Load required libraries
library(shiny)
library(pool)
library(dplyr)
library(DBI)
library(stringr)
# Define server logic
server <- shinyServer(function(input, output, session) {
    # Add single quotation marks around a string
    single_quoted <- function(my_string){
      return(paste("'", my_string, "'", sep = ""))
    }
    
    # Source database related functions
    # local = TRUE -> objects are inside server function
    source('server_functions/database_functions.R', local = TRUE)
    
    # Source separate server files for each tab
    source('upload/upload_server.R', local = TRUE)
    source('search/search_server.R', local = TRUE)
})
