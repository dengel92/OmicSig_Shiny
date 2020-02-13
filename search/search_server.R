# Server logic for search page

# Update species checkboxes with list of species from database 
observe({
  updateCheckboxGroupInput(session,
    "search_species",
    choices = c(get_species())
  )
})

# Show which species have been selected so far
output$search_selected_species <- renderText(
    c(
        "<p><font size=3><b>",
        "Selected Species:</b></font></p><p><font size=2>",
        input$search_species,
        "</p></font>"
    )
)

# Update other dropdowns based on input to species
observeEvent(input$search_species, {
    # If a species is selected
    if (!is.null(input$search_species)) {
        # Query database to find platforms matching selected species
        sql_obj <-
            sql_finding_query(
                target_table = "platform_signature_view",
                field_where = "species",
                field_where_value = input$search_species
            )
        # Update platform dropdown
        updateSelectizeInput(session,
            "search_platform_name",
            choices = sql_obj$platform_name)
    } else {
        # If user unselects all species, use full list of platforms again
        updateSelectizeInput(session,
            "search_platform_name",
            choices = get_platforms())
    }
},
    ignoreNULL = FALSE)

# Update platform dropdown with list of platforms from database
observe({
    updateSelectizeInput(session,
        "search_platform_name",
        choices = get_platforms())
})

# Show which platforms have been selected so far
output$search_selected_platforms <- renderText(
    c(
        "<p><font size=3><b>",
        "Selected Platforms:</b></font></p><p><font size=2>",
        if (length(input$search_platform_name) < 1) {
            "No platforms selected"
        } else {
            input$search_platform_name},
        "</p></font>"
    )
)

### NOT FULLY IMPLEMENTED YET
### Currently only looks at species input
# Show table of matching signatures when you hit the search button
observeEvent(input$search, {
    #shinyalert("Uh oh", "Seems this isn't fully implemented yet... :(")
    output$search_results <- renderTable({
        # Ensure that the table updates only once, immediately after clicking
        isolate(
            # Search database for matching signatures
            sql_obj <-
                sql_finding_query(
                    target_table = "platform_signature_view",
                    field_where = "species",
                    field_where_value = input$search_species
                )
        )
        return(sql_obj)
    })
})
