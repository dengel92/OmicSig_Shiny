# Server logic for search page

# Source file containing general functions for the search page
source("search/search_functions.R", local = TRUE)

# Make a data frame storing all the relevant information about each dropdown
dropdowns <-
    rbind.data.frame(
        c("search_species", "species", "species"),
        c("search_platform_name", "platform_name", "platforms"),
        c("search_experiment_type", "experiment_type", "experiment types"),
        c("search_source_type", "source_type", "source types"),
        c("search_phenotype_id", "phenotype_id", "phenotypes"),
        c("search_signature_name", "signature_name", "signatures"),
        c("search_submitter", "submitter", "submitters"),
        stringsAsFactors = FALSE
    )
colnames(dropdowns) <- c("dropdown_id", "dropdown_name", "display")

# Clear selected terms after clicking clear button
observeEvent(input$clear, {
    isolate({
        # Clear all dropdowns
        for (dropdown in dropdowns$dropdown_name) {
            clear_dropdown(dropdown)
        }
    })
})

# Show which search terms have been selected so far
output$search_terms <- renderText(
    c(
        # Section header
        "<p><font size=3><b>",
        "Selected Search Terms:</b></font></p><p><font size=2>",
        
        # Show selected terms for each dropdown menu
        apply(dropdowns, 1, function(x) {selected_html(x[1], x[3])}),
        
        # Show selected upload date range
        selected_html("search_upload_date", "upload date (start, end)"),
        "</p></font>"
    )
)

# Construct list of all possible in clauses for query
ins <- reactive({
    # Initialize list to store result
    ins_list <- list()
    # Loop through rows of dropdowns dataframe
    for (row in 1:dim(dropdowns)[1]) {
        # Extract field name and dropdown id
        name <- dropdowns[row, ]$dropdown_name
        id <- dropdowns[row, ]$dropdown_id
        # Add an element whose name is the field and whose value is the
        #   input from the dropdown
        ins_list[[name]] = input[[id]]
    }
    return(ins_list)
})

# Construct list of all possible between clauses for query
betweens <- reactive({
    list(
        "upload_date" = input[["search_upload_date"]]
    )
})

observe({
    # Disable dropdown menus until they finish updating
    lapply(dropdowns$dropdown_id, disable)
    
    # Construct list of all possible between clauses for query
    betweens <- list("upload_date" = input$search_upload_date)
    
    # Update dropdown menus
    for (dropdown in dropdowns$dropdown_name) {
        update_dropdown(dropdown, ins(), betweens())
    }
    
    # Re-enable dropdown menus
    lapply(dropdowns$dropdown_id, enable)
})

# Display output and download button after clicking search button
observeEvent(input$search, {
    # Make sure at least one search term is selected before querying database
    if (length(compact(c(ins, betweens))) < 1) {
        shinyalert("Please select at least one search term!")
    } else {
        # Search database for matching signatures
        sql_obj <-
            sql_finding_query(target_table = "platform_signature_view",
                ins = ins(), betweens = betweens())
        
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
