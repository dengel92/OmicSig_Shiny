#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(pool)
library(dplyr)
library(DBI)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$testout <- function(){
    print("Hi there")
  }
  

  
  simplequery<-reactive({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "sigrepo",
      host = "localhost",
      username = "guest",
      password = "guest")
    on.exit(dbDisconnect(conn), add = TRUE)
    query_obj<-dbGetQuery(conn,statement="Select * from sigrepo.features limit 1,5;")
    query_obj
  })
  
  output$featurename_table<-renderTable(simplequery())
  
})
