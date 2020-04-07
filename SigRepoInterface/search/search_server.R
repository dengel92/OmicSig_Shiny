# Server logic for search page

#' General function for generating html output showing selected search terms
#' 
#' @param search_id the dropdown menu id whose selected values should be displayed
#' @param display_name the word or phrase to display in the html output representing
#'       the dropdown menu field
selected_html <- function(search_id, display_name) {
    # Display "no <display_name> selected" if there is no input to this dropdown
    if (length(input[[search_id]]) < 1) {
        paste0("no ", display_name, " selected</br>")
    } else {
        # Otherwise display "<display_name>: <value1>, <value2>, ..."
        paste0(display_name,
            ": <b>",
            paste(input[[search_id]], collapse = ", "),
            "</b></br>")
    }
}

#' General function for updating a dropdown menu
#'
#' @param field the name of the search field whose dropdown menu should be updated
#' @param wheres list of all of the possible where clauses for sql_finding_query()
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

#' General function for clearing selections of a dropdown menu
#'
#' @param field the name of the search field whose dropdown menu should be updated
clear_dropdown = function(field) {
    # Query the database to find all values of field
    sql_obj <-
        sql_finding_query(fields = field,
            target_table = "platform_signature_view")
    # Determine the ID for the dropdown menu that should be updated
    search_id <- paste0("search_", field)
    # Update dropdown menu
    updateSelectizeInput(session,
        search_id,
        choices = c(sql_obj[[field]]),
        selected = NULL)
}

# Clear selected terms after clicking clear button
observeEvent(input$clear, {
    isolate({
        # Update species dropdown menu
        clear_dropdown("species")
        
        # Update platforms dropdown menu
        clear_dropdown("platform_name")
        
        # Update experiment types dropdown menu
        clear_dropdown("exp_type_id")
        
        # Update source types dropdown menu
        clear_dropdown("source_type")
        
        # Update perturbagens dropdown menu
        clear_dropdown("perturbagen_id")
        
        # Update signatures names dropdown menu
        clear_dropdown("signature_name")
        
        # Update submitters dropdown menu
        clear_dropdown("submitter_id")
    })
})

# Show which search terms have been selected so far
output$search_terms <- renderText(
    c(
        # Section header
        "<p><font size=3><b>",
        "Selected Search Terms:</b></font></p><p><font size=2>",
        
        # Show selected species
        selected_html("search_species", "species"),
        
        # Show selected platforms
        selected_html("search_platform_name", "platforms"),
        
        # Show selected experiment types
        selected_html("search_exp_type_id", "experiment types"),
        
        # Show selected source types
        selected_html("search_source_type", "source types"),
        
        # Show selected perturbagens
        selected_html("search_perturbagen_id", "perturbagens"),
        
        # Show selected signature names
        selected_html("search_signature_name", "signatures"),
        
        # Show selected submitters
        selected_html("search_submitter_id", "submitters"),
        "</p></font>"
    )
)

observe({
    isolate({
        # Construct list of all possible where clauses for query
        wheres <- list(
            "species" = input$search_species,
            "platform_name" = input$search_platform_name,
            "exp_type_id" = input$search_exp_type_id,
            "source_type" = input$search_source_type,
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
        
        # Update source types dropdown menu
        update_dropdown("source_type", wheres)
        
        # Update perturbagens dropdown menu
        update_dropdown("perturbagen_id", wheres)
        
        # Update signatures names dropdown menu
        update_dropdown("signature_name", wheres)
        
        # Update submitters dropdown menu
        update_dropdown("submitter_id", wheres)
    })
})

# Display output and download button after clicking search button
observeEvent(input$search, {
    # Construct list of all possible where clauses for query
    wheres <- list(
        "species" = input$search_species,
        "platform_name" = input$search_platform_name,
        "exp_type_id" = input$search_exp_type_id,
        "source_type" = input$search_source_type,
        "perturbagen_id" = input$search_perturbagen_id,
        "signature_name" = input$search_signature_name,
        "submitter_id" = input$search_submitter_id
    )
    
    # Make sure at least one search term is selected before querying database
    if (length(compact(wheres)) < 1) {
        shinyalert("Please select at least one search term!")
    } else {
        # Search database for matching signatures
        sql_obj <-
            sql_finding_query(target_table = "platform_signature_view",
                wheres = wheres)
        
        # Display error message instead of table if query produces no results
        if (dim(sql_obj)[1] < 1) {
            shinyalert("No results found. Please modify your search.")
        } else {
            # Display table of search results
            output$search_results <- renderDataTable({
                # Ensure that the table updates only once after clicking search
                isolate(search_table <- sql_obj)
                # Make signature name a link to that signature's directory
                search_table$signature_name <-
                    create_link(search_table$signature_name)
                return(search_table)
            }, escape = FALSE)
            
            # Download button for full search results table
            output$search_results_download <- downloadHandler(
                filename = paste("SigRepo_search_results_all.tsv"),
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
            
            # Download button for selected rows from search results table
            output$selected_search_results_download <-
                downloadHandler(
                    filename = paste("SigRepo_search_results_selected.tsv"),
                    content = function(file) {
                        for (signature in sql_obj[input$search_results_rows_selected, ]) {
                            print(signature)}
                        write.table(
                            sql_obj[input$search_results_rows_selected, ],
                            file,
                            row.names = FALSE,
                            quote = FALSE,
                            col.names = TRUE,
                            sep = "\t"
                        )
                    }
                )
        }
    }
    
})

# Select/unselect all rows depending on checkbox input
observeEvent(input$select_all, {
    # Create an object to manipulate existing table
    dt_proxy <- dataTableProxy("search_results")
    if (input$select_all) {
        # Select all rows
        DT::selectRows(dt_proxy, input$search_results_rows_all)
    } else {
        # Unselect all rows
        DT::selectRows(dt_proxy, NULL)
    }
})
output$selected_rows <-
    renderPrint(print(input$search_results_rows_selected))
