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
            paste("Species: ",
                paste(input$search_species, collapse = ", "),
                "</br>")
        },
        # Show selected platforms
        if (length(input$search_platform_name) < 1) {
            "No platforms selected</br>"
        } else {
            paste("Platform(s): ",
                paste(input$search_platform_name, collapse = ", "),
                "</br>")
        },
        # Show selected experiment types
        if (length(input$search_experiment_type) < 1) {
            "No experiment types selected</br>"
        } else {
            paste(
                "Experiment type(s): ",
                paste(input$search_experiment_type, collapse = ", "),
                "</br>"
            )
        },
        # Show selected signature names
        if (length(input$search_signature_name) < 1) {
            "No signature names selected</br>"
        } else {
            paste(
                "Signature name(s): ",
                paste(input$search_signature_name, collapse = ", "),
                "</br>"
            )
        },
        "</p></font>"
    )
)

# Update species dropdown
observe({
    # Query database to find species matching selected other fields
    sql_obj <-
        sql_finding_query(
            fields = "species",
            target_table = "platform_signature_view",
            wheres = list(
                "platform_name" = input$search_platform_name,
                "exp_type_id" = input$search_experiment_type,
                "signature_name" = input$search_signature_name
            )
        )
    # Update dropdown menu
    updateSelectizeInput(
        session,
        "search_species",
        choices = c(sql_obj$species, input$search_species),
        selected = input$search_species
    )
})

# Update platform dropdown
observe({
    # Query database to find platforms matching selected other fields
    sql_obj <-
        sql_finding_query(
            fields = "platform_name",
            target_table = "platform_signature_view",
            wheres = list(
                "species" = input$search_species,
                "exp_type_id" = input$search_experiment_type,
                "signature_name" = input$search_signature_name
            )
        )
    # Update dropdown menu
    updateSelectizeInput(
        session,
        "search_platform_name",
        choices = c(sql_obj$platform_name, input$search_platform_name),
        selected = input$search_platform_name
    )
})

# Update experiment types dropdown
observe({
    # Query database to find experiment types matching selected other fields
    sql_obj <-
        sql_finding_query(
            fields = "exp_type_id",
            target_table = "platform_signature_view",
            wheres = list(
                "species" = input$search_species,
                "platform_name" = input$search_platform_name,
                "signature_name" = input$search_signature_name
            )
        )
    # Update dropdown menu
    updateSelectizeInput(
        session,
        "search_experiment_type",
        choices = c(sql_obj$exp_type_id, input$search_experiment_type),
        selected = input$search_experiment_type
    )
})

# Update signatures names dropdown
observe({
    # Query database to find signature names matching selected other fields
    sql_obj <-
        sql_finding_query(
            fields = "signature_name",
            target_table = "platform_signature_view",
            wheres = list(
                "species" = input$search_species,
                "exp_type_id" = input$search_experiment_type,
                "platform_name" = input$search_platform_name
            )
        )
    # Update dropdown menu
    updateSelectizeInput(
        session,
        "search_signature_name",
        choices = c(sql_obj$signature_name, input$search_signature_name),
        selected = input$search_signature_name
    )
})

# Display output and download button after clicking search button
observeEvent(input$search, {
    # Construct where clauses for each field
    wheres = list(
        "species" = input$search_species,
        "platform_name" = input$search_platform_name,
        "exp_type_id" = input$search_experiment_type,
        "signature_name" = input$search_signature_name
    )
    # Search database for matching signatures
    sql_obj <-
        sql_finding_query(target_table = "platform_signature_view",
            wheres = wheres)
    
    # Display table of search results
    output$search_results <- renderDataTable({
        # Ensure that the table updates only once, immediately after clicking
        isolate(
            search_table <- sql_obj
        )
        # Make signature name a link to the corresponding signature directory
        search_table$signature_name <- create_link(search_table$signature_name)
        return(search_table)
    }, escape = FALSE)
    
    # Download button for search results
    output$search_results_download <- downloadHandler(
        filename = paste("SigRepo_search_results.tsv"),
        content = function(file) {
            write.table(
                sql_obj,
                file,
                row.names = FALSE,
                quote = FALSE,
                col.names = TRUE,
                sep = "\t"
            )
        }
    )
})