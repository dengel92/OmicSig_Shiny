# Load required libraries
library(shiny)
library(pool)
library(dplyr)
library(DBI)
library(DT)
library(stringr)

# Define server logic
server <- shinyServer(function(input, output, session) {
    # Source database related functions
    # local = TRUE -> objects are inside server function
    source('server_functions/database_functions.R', local = TRUE)
    
    # Source separate server files for each tab
    # local = TRUE -> objects are inside server function
    source('compare/compare_server.R' , local = TRUE)
    source('compare_bulk/compare_bulk_server.R' , local = TRUE)
    source('server_functions/comparing/compare_heatmap.R' , local = TRUE)   
    source('upload/upload_server.R', local = TRUE)
    source('search/search_server.R', local = TRUE)
    source('hypeR_overrep/hypeR_overrep_server.R' , local = TRUE)
})
