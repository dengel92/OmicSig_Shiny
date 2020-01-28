#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

# library(shiny)
# library(pool)
# library(dplyr)
# library(DBI)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
    # simplequery & featurename_table are test functions
    # to get a feel for RShiny-MySQL interaction
    
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
    
    simplequery <- reactive({
        conn <- new_conn_handle()
        on.exit(dbDisconnect(conn), add = TRUE)
        query_obj <-
            dbGetQuery(conn, statement = "Select * from sigrepo.features limit 1,5;")
        query_obj
    })
    output$featurename_table <- renderTable(simplequery())
    
    
    #Autocomplete for Species
    autocomplete_species <- reactive({
        species_handle <- new_conn_handle()
        on.exit(dbDisconnect(species_handle, add = TRUE))
        species_obj <- dbGetQuery(
            species_handle,
            statement = "
                                                    select
                                                        concat(species,'[',taxonomic_id,']')
                                                        as species
                                                    from species;
                                                    "
        )
        return((species_obj$species))
    })
    observe({
        updateSelectizeInput(session,
            "species_id",
            choices = c("", autocomplete_species()))
    })
    #end autocomplete for species
    
    observeEvent(input$add_signature, {
        shinyalert("If you're reading this, then this function is still in development aw jeez")
    })
    
    
})
