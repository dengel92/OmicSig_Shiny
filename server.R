# Load required libraries
library(shiny)
library(pool)
library(dplyr)
library(DBI)

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
    
    # QC of rds file to ensure it's not corrupted and in the right format(later)
    checkRDS <- function(rds_file,input_name){
      #observe+trycatch meant to capture error and display in UI,
      #rather than crashing the app when an error happens.
      observe({
        tryCatch(
          readRDS(rds_file),
          error = function(e) {
            shinyalert("improper file. Make sure your file is RDS and not corrupted.")
          }
        )
      })
    }
    
    #input : file input object, target directory
    #output : copied file
    copy_file <- function(input_file, destination){
      #check if input is empty or not. if empty, do nothing.
      if(input_file!=""){
        file.copy(input_file,destination)
      }
    }
    
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
    
    # Autocomplete for species
    autocomplete_species <- reactive({
        # Connect to database
        species_handle <- new_conn_handle()
        # Disconnect from database when exiting autocomplete_species()
        on.exit(dbDisconnect(species_handle, add = TRUE))
        # Query database
        species_obj <- dbGetQuery(
            species_handle,
            statement = "
                        select
                            concat(species,'[',taxonomic_id,']')
                            as species
                        from species;
                        "
        )
        # Return results of query
        return((species_obj$species))
    })
    # Update options in dropdown menu
    observe({
        updateSelectizeInput(session,
            "species_id",
            choices = c("", autocomplete_species()))
    })
    # End autocomplete for species
    
    # Autocomplete for platform
    autocomplete_platform <- reactive({
        # Connect to database
        platform_handle <- new_conn_handle()
        # Disconnect from database when exiting autocomplete_platform()
        on.exit(dbDisconnect(platform_handle, add = TRUE))
        # Query database
        platform_obj <- dbGetQuery(
            platform_handle,
            statement = "
                        select
                            platform_name
                        from assay_platforms;
                        "
        )
        # Return results of query
        return((platform_obj$platform_name))
    })
    # Update options in dropdown menu
    observe({
        updateSelectizeInput(session,
            "platform_id",
            choices = c("", autocomplete_platform()))
    })
    # End autocomplete for species
    ###
    
    #checking quality of files uploaded, yielding in error
    #if rds file is 'bad'
    #for testing purposes(writing, reading, etc), I comment out the checkRDS line, so that I can
    #just work with whatever file I want 
    observeEvent(input$rds_file_1, {
      #checkRDS(input$rds_file_1,"rds_file_1")
      #file.copy(input$rds_file_1$datapath,"~/ree.txt")
    })
    observeEvent(input$rds_file_2, {
      checkRDS(input$rds_file_2,"rds_file_2")
    })
    observeEvent(input$rds_file_3, {
      checkRDS(input$rds_file_3,"rds_file_3")
    })
    #end QC for file uploads
    
    #When you click the 'submit' button...
    observeEvent(input$add_signature, {
        #where will these rds files live?
        #can expand on this function as we become more confident in
        #backend(DB) setup(i.e. hierarchical file system)
        rds_dir<-"/data_files/rds/"
        #alerting user about where the file's going.
        shinyalert(paste("writing file to ",rds_dir,input$rds_file_1$name,sep=''))
        copy_file(input$rds_file_1$datapath,paste(rds_dir,input$rds_file_1$name,sep=''))
        #need to add a 'hashkey' ability. That is, instead of just copying the file as '/path/to/<filename>', we would either
        # 1) write as '/path/to/<random hash name of file>' and reflect in DB
        # 2) write the file with the original name, but have soft links named <hash name> that point to the original file, with the hash key
        #     also reflected in the DB
        #also need to add alerts confirming to the user that the operation is/was successful
    })
    
})
