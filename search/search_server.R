# Server logic for search page

# Show which search terms have been selected so far
output$search_terms <- renderText(
    c(
        "<p><font size=3><b>",
        "Selected Search Terms:</b></font></p><p><font size=2>",
        # Show selected species
        if (length(input$search_species) < 1) {
            "No species selected</br>"
        } else {
            paste(paste(input$search_species, collapse = ', '), '</br>')
        },
        # Show selected platforms
        if (length(input$search_platform_name) < 1) {
            "No platforms selected"
        } else {
            paste(input$search_platform_name, collapse = ', ')
        },
        "</p></font>"
    )
)

# Update species dropdown with list of species from database
observe({
    updateSelectizeInput(session,
        "search_species",
        choices = get_species())
})

# Update other dropdowns based on input to species
observeEvent(input$search_species,
    ignoreNULL = FALSE,
    handlerExpr = {
        # If a species is selected
        if (!is.null(input$search_species)) {
            # Query database to find platforms matching selected species
            sql_obj <-
                sql_finding_query(
                    fields = "platform_name",
                    target_table = "platform_signature_view",
                    wheres = list("species" = input$search_species)
                )
            # Update platform dropdown
            updateSelectizeInput(
                session,
                "search_platform_name",
                choices = c(sql_obj$platform_name, input$search_platform_name),
                selected = input$search_platform_name
            )
        } else {
            # If user unselects all species, use full list of platforms again
            updateSelectizeInput(
                session,
                "search_platform_name",
                choices = get_platforms(),
                selected = input$search_platform_name
            )
        }
    })

# Update platform dropdown with list of platforms from database
observe({
    updateSelectizeInput(session,
        "search_platform_name",
        choices = get_platforms())
})

# Update other dropdowns based on input to platform
observeEvent(input$search_platform_name,
    ignoreNULL = FALSE,
    handlerExpr = {
        # If a platform is selected
        if (!is.null(input$search_platform_name)) {
            # Query database to find species matching selected platform(s)
            sql_obj <-
                sql_finding_query(
                    fields = "species",
                    target_table = "platform_signature_view",
                    wheres = list("platform_name" = input$search_platform_name)
                )
            # Update species dropdown
            updateSelectizeInput(
                session,
                "search_species",
                choices = c(sql_obj$species, input$search_species),
                selected = input$search_species
            )
        } else {
            # If user unselects all platforms, use full list of species again
            updateSelectizeInput(
                session,
                "search_species",
                choices = get_species(),
                selected = input$search_species
            )
        }
    })

# Show table of matching signatures when you hit the search button
observeEvent(input$search, {
    # Construct where clause for species
    where_species = NULL
    if (!is.null(input$search_species)) {
        where_species = list('species' = input$search_species)
    }
    # Construct where clause for platforms
    where_platform = NULL
    if (!is.null(input$search_platform_name)) {
        where_platform = list('platform_name' = input$search_platform_name)
    }
    # Combine all where clauses
    wheres = c(where_species, where_platform)
    output$search_results <- renderTable({
        # Ensure that the table updates only once, immediately after clicking
        isolate(
            # Search database for matching signatures
            sql_obj <-
                sql_finding_query(target_table = "platform_signature_view",
                    wheres = wheres)
        )
        return(sql_obj)
    })
})