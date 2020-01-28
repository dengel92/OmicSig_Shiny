# Load libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(markdown)
library(RMySQL)
library(pool)
library(dplyr)
library(DBI)

# Source the files containing the UI and server definitions
source('ui.R')
source('server.R')

# Run the app
shinyApp(ui, server)
