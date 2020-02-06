# Server logic for search page

# Get choices for species
get_species <- reactive({
  # Connect to database
  species_handle <- new_conn_handle()
  # Disconnect from database when exiting get_species()
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
  updateCheckboxGroupInput(session,
    "search_species",
    choices = c(get_species())
  )
})

output$search_selected_species <- renderText(
  c("<p><font size=3><b>", "Selected Species:</b></font></p><p><font size=2>", input$search_species, "</p></font>")
)
