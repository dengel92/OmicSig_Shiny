# Server logic for search page

# Update species dropdown with list of species from database
observe({
    updateSelectizeInput(session,
        "search_species",
        choices = c("", get_species()))
})

# Show which species have been selected so far
output$search_selected_species <- renderText(
    c(
        "<p><font size=3><b>",
        "Selected Species:</b></font></p><p><font size=2>",
        if (length(input$search_species) < 1) {
            "No species selected"
        } else {
            input$search_species},
        "</p></font>"
    )
)

# Update other dropdowns based on input to species
observeEvent(input$search_species, {
    # Query database to find platforms matching selected species
    sql_obj <- sql_finding_query(target_table = "platform_signature_view",
        field_where = "species",
        field_where_value = input$search_species)
    # Update platform dropdown
    updateSelectizeInput(session,
        "search_platform_name",
        choices = c(sql_obj$platform_name))
})

# Update platform dropdown with list of platforms from database
observe({
    updateSelectizeInput(session,
        "search_platform_name",
        choices = c(get_platforms()))
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