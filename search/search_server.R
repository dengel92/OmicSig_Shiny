# Server logic for search page

# General function for generating html output showing selected search terms
# Inputs:
#   search_id: the id of the dropdown menu whose selected values are displayed
#   display_name: the word or phrase to display in the html output representing
#       the dropdown menu field
selected_html <- function(search_id, display_name) {
    # Display "no <display_name> selected" if there is no input to this dropdown
    if (length(input[[search_id]]) < 1) {
        paste0("no ", display_name, " selected</br>")
    } else {
        # Display "<display_name>: <value1>, <value2>, ..." otherwise
        paste0(display_name,
            ": <b>",
            paste(input[[search_id]], collapse = ", "),
            "</b></br>")
    }
}

# Show which search terms have been selected so far
output$search_terms <- renderText(
    c(
        "<p><font size=3><b>",
        "Selected Search Terms:</b></font></p><p><font size=2>",
        # Show selected species
        selected_html("search_species", "species"),
        # Show selected platforms
        selected_html("search_platform_name", "platforms"),
        # Show selected experiment types
        selected_html("search_exp_type_id", "experiment types"),
        # Show selected cell lines
        selected_html("search_cell_line", "cell lines"),
        # Show selected perturbagens
        selected_html("search_perturbagen_id", "perturbagens"),
        # Show selected signature names
        selected_html("search_signature_name", "signatures"),
        # Show selected submitters
        selected_html("search_submitter_id", "submitters"),
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
    updateSelectizeInput(session,
        search_id,
        choices = c(sql_obj[[field]], input[[search_id]]),
        selected = input[[search_id]])
}

observe({
    # Construct list of all possible where clauses for query
    wheres <- list(
        "species" = input$search_species,
        "platform_name" = input$search_platform_name,
        "exp_type_id" = input$search_experiment_type,
        "cell_line" = input$search_cell_line,
        "perturbagen_id" = input$search_perturbagen_id,
        "signature_name" = input$search_signature_name,
        "submitter_id" = input$search_submitter_id
    )
    
    # Update species dropdown menu
    update_dropdown("species", wheres)
    
    # Update platforms dropdown menu
    update_dropdown("platform_name", wheres)
    
    # Update experiment types dropdown menu
    update_dropdown("exp_type_id", wheres)
    
    # Update cell lines dropdown menu
    update_dropdown("cell_line", wheres)
    
    # Update perturbagens dropdown menu
    update_dropdown("perturbagen_id", wheres)
    
    # Update signatures names dropdown menu
    update_dropdown("signature_name", wheres)
    
    # Update submitters dropdown menu
    update_dropdown("submitter_id", wheres)
})

# Display output and download button after clicking search button
observeEvent(input$search, {
    # Construct list of all possible where clauses for query
    wheres <- list(
        "species" = input$search_species,
        "platform_name" = input$search_platform_name,
        "exp_type_id" = input$search_experiment_type,
        "cell_line" = input$search_cell_line,
        "perturbagen_id" = input$search_perturbagen_id,
        "signature_name" = input$search_signature_name,
        "submitter_id" = input$search_submitter_id
    )
    
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