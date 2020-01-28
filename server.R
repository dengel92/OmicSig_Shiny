# The following libraries used here are loaded in app.R
# library(shiny)
# library(pool)
# library(dplyr)
# library(DBI)

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
        # Print the results of the query?
        query_obj
    })
    # Display query results
    output$featurename_table <- renderTable(simplequery())
    
    # Autocomplete for Species
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
    
    # Show alert that adding signatures hasn't been implemented yet :(
    observeEvent(input$add_signature, {
        shinyalert("If you're reading this, then this function is still in development aw jeez")
    })
    
})
