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

# General function for updating a dropdown menu
# Input:
#   field: the name of the search field whose dropdown menu should be updated
#   wheres: list of all of the possible where clauses for sql_finding_query()
update_dropdown = function(field, wheres) {
    # Query the database to find values of field that match selected values of
    #   other fields
    sql_obj <-
        sql_finding_query(fields = field,
            target_table = "platform_signature_view",
            wheres = wheres[names(wheres) != field])
    # Determine the ID for the dropdown menu that should be updated
    search_id <- paste0("search_", field)
    # Update dropdown menu
    updateSelectizeInput(
        session,
        search_id,
        choices = c(sql_obj[[field]], input[[search_id]]),
        selected = input[[search_id]]
    )
}

observe({
    # Construct list of all possible where clauses for query
    wheres <- list(
        "species" = input$search_species,
        "platform_name" = input$search_platform_name,
        "exp_type_id" = input$search_experiment_type,
        "signature_name" = input$search_signature_name
    )
    
    # Update species
    update_dropdown("species", wheres)
    
    # Update platforms
    update_dropdown("platform_name", wheres)
    
    # Update experiment types
    update_dropdown("exp_type_id", wheres)
    
    # Update signatures names
    update_dropdown("signature_name", wheres)
    
    # Display output and download button after clicking search button
    observeEvent(input$search, {
        # Search database for matching signatures
        sql_obj <-
            sql_finding_query(target_table = "platform_signature_view",
                wheres = wheres)
        
        # Display table of search results
        output$search_results <- renderDataTable({
            # Ensure that the table updates only once, immediately after clicking
            isolate(search_table <- sql_obj)
            # Make signature name a link to the corresponding signature directory
            search_table$signature_name <-
                create_link(search_table$signature_name)
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
})
