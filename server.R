# Load required libraries
library(shiny)
library(pool)
library(dplyr)
library(DBI)
library(stringr)
# Define server logic
server <- shinyServer(function(input, output, session) {
    #
    single_quoted <- function(my_string){
      return(paste("'", my_string, "'", sep = ""))
    }
    
    # Source database related functions
    source('server_functions/database_functions.R', local = TRUE)
    
    # Source separate server files for each tab
    # local = TRUE -> objects are inside server function
    source('compare/compare_server.R' , local = TRUE)
    source('upload/upload_server.R', local = TRUE)
    source('search/search_server.R', local = TRUE)
    
    # Summary page (test)
    # Test function to get a feel for RShiny-MySQL interaction
    simplequery <- reactive({
      # Connect to database
      conn <- new_conn_handle()
      # Disconnect from database when exiting simplequery()
      on.exit(dbDisconnect(conn), add = TRUE)
      # Query the database
      query_obj <-
        dbGetQuery(conn,
          statement = "Select * from sigrepo.features limit 1,5;")
      # Returns/outputs the results of the query
      query_obj
    })
    # Display query results
    output$featurename_table <- renderTable(simplequery())
    
})
