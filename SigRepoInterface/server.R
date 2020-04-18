# Load required libraries
library(shiny)
library(pool)
library(dplyr)
library(DBI)
library(DT)
library(stringr)
library(shinycssloaders)

# Define server logic
server <- shinyServer(function(input, output, session) {
    # Source database related functions
    # local = TRUE -> objects are inside server function
    source('../command_line/database_functions.R', local = TRUE)
    source('server_functions/database_functions_interface.R')
    # Source OmicSignature Function Files 
    source('../OmicSignature/check_functions/Function_json.R')
    source('../OmicSignature/check_functions/Function_objCheck.R')
    source('../OmicSignature/check_functions/Function_write_sig.R')
    source('../OmicSignature/OmicSignature.R')
    #
    # Source separate server files for each tab
    # local = TRUE -> objects are inside server function
    source('help/help_server.R', local = TRUE)
    source('compare/compare_server.R' , local = TRUE)
    source('compare_bulk/compare_bulk_server.R' , local = TRUE)
    source('server_functions/comparing/compare_heatmap.R' , local = TRUE)   
    source('upload/upload_server.R', local = TRUE)
    source('search/search_server.R', local = TRUE)
    source('hypeR/hypeR_server.R' , local = TRUE)
})
