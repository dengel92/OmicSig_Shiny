# Load required libraries
library(shiny)
library(pool)
library(dplyr)
library(DBI)
library(stringr)
# Define server logic
server <- shinyServer(function(input, output, session) {
    
    # Create a connection to the database
    new_conn_handle <- function() {
        dbConnect(
            drv = RMySQL::MySQL(),
            dbname = "sigrepo",
            host = "sigrepo.bu.edu",
            port = 4253,
            username = "guest",
            password = "guest"
        )
    }
    
    #generic sql function
    sql_generic <- function(query){
      conn = new_conn_handle()
      # Disconnect from database when exiting simplequery()
      on.exit(dbDisconnect(conn), add = TRUE)
      this_query =
        dbGetQuery(conn,
          statement = query)
      return(this_query)
    }
    
    #
    single_quoted <- function(my_string){
      return(paste("'",my_string,"'",sep=""))
    }
    
    # Source separate server files for each tab
    # local = TRUE -> objects are inside server function
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
