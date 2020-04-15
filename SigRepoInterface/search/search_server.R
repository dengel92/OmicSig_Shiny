# Server logic for search page

# Source file containing general functions for the search page
source("search/search_functions.R", local = TRUE)

# Make a data frame storing all the relevant information about each input widget
widgets <-
    rbind.data.frame(
        c("search_species", "species", "species", "dropdown"),
        c("search_platform_name", "platform_name", "platforms", "dropdown"),
        c("search_experiment_type", "experiment_type", "experiment types", "dropdown"),
        c("search_source_type", "source_type", "source types", "dropdown"),
        c("search_phenotype_id", "phenotype_id", "phenotypes", "dropdown"),
        c("search_signature_name", "signature_name", "signatures", "dropdown"),
        c("search_submitter", "submitter", "submitters", "dropdown"),
        c("search_feature_name", "feature_name", "feature names", "dropdown_special"),
        c("search_upload_date", "upload_date", "upload date (start, end)", "date"),
        stringsAsFactors = FALSE
    )
colnames(widgets) <- c("id", "name", "display", "type")

# Extract the row numbers containing the information for the dropdown menus
dropdown_rows = which(widgets$type == "dropdown")

# Clear selected terms after clicking clear button
observeEvent(input$clear, {
    isolate({
        # Clear all dropdowns
        for (dropdown in widgets[dropdown_rows, ]$name) {
            clear_dropdown(dropdown, "platform_signature_view")
        }
        clear_dropdown("feature_name", "feature_signature_view")
        
        # Reset date range to default
        updateDateRangeInput(session, "search_upload_date",
            start = "2020-01-01", end = Sys.Date())
    })
})

# Show which search terms have been selected so far
output$search_terms <- renderText(
    c(
        # Section header
        "<p><font size=3><b>",
        "Selected Search Terms:</b></font></p><p><font size=2>",
        
        # Show selected terms for each dropdown menu
        apply(widgets, 1, function(x) {selected_html(x[1], x[3])}),
        
        "</p></font>"
    )
)

# Construct list of all possible in clauses for query
ins <- reactive({
    # Initialize list to store result
    ins_list <- list()
    dropdowns = widgets[dropdown_rows, ]
    # Loop through rows of dropdowns dataframe
    for (row in 1:dim(dropdowns)[1]) {
        # Extract field name and dropdown id
        name <- dropdowns[row, ]$name
        id <- dropdowns[row, ]$id
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

# Construct list of features
features <- reactive({
    list(
        "feature_name" = input[["search_feature_name"]]
    )
})

observe({
    # Disable widgets until they finish updating
    lapply(widgets$id, disable)
    
    # Update dropdown menus
    for (dropdown in widgets[dropdown_rows, ]$name) {
        update_dropdown(dropdown, "platform_signature_view", ins(), betweens())
    }
    
    # Update feature names dropdown menu
    signatures <- list()
    if (length(input[["search_signature_name"]]) == 0) {
        # If search_signature_name is blank, get a list of all its choices
        signatures <-
            get_field_values("signature_name",
                "platform_signature_view",
                ins(),
                betweens())
        signatures <- list("signature_name" = signatures)
    } else {
        # If signature names are selected, get the list of selected signatures
        signatures <- list("signature_name" = input[["search_signature_name"]])
    }
    # Get list of features corresponding to currently available signature names
    features <- get_field_values("feature_name", "feature_signature_view",
        signatures, NULL)
    updateSelectizeInput(session,
        "search_feature_name",
        choices = c(features, input[["search_feature_name"]]),
        selected = input[["search_feature_name"]])
    
    # Re-enable widgets
    lapply(widgets$id, enable)
})

# Display output and download button after clicking search button
observeEvent(input$search, {
    # Make sure at least one search term is selected before querying database
    if (length(compact(c(ins(), betweens(), features()))) < 1) {
        shinyalert("Please select at least one search term!")
        return()
    }
    ins <- ins()
    # If any features are selected, handle them first
    if (length(compact(features())) > 0) {
        # Get the list of signature names corresponding to selected features
        feature_signatures <-
            get_field_values(
                "signature_name",
                "feature_signature_view",
                ins = features(),
                betweens = NULL
            )
        # Get the list of selected signature names
        selected_signatures <- ins$signature_name
        # Get the intersection of the two lists of signature names
        intersect_signatures <-
            intersect(feature_signatures, selected_signatures)
        if (length(intersect_signatures) < 1 & length(selected_signatures) >= 1) {
            # If the intersection is empty and signature names are selected
            #   then set signature_name to an empty string for sql_finding_query()
            ins$signature_name = ""
        } else if (length(intersect_signatures) < 1 & length(selected_signatures) < 1) {
            # If the intersection is empty and no signature names are selected
            #   then set signature_name to the signatures corresponding to the
            #   selected features for sql_finding_query()
            ins$signature_name = feature_signatures
        } else {
            # Otherwise set signature_name to the intersection for sql_finding_query()
            ins$signature_name = intersect_signatures
        }
    }
    # Search database for matching signatures
    sql_obj <-
        sql_finding_query(target_table = "platform_signature_view",
            ins = ins,
            betweens = betweens())
    
    # Display error message instead of table if query produces no results
    if (dim(sql_obj)[1] < 1) {
        shinyalert("No results found. Please modify your search.")
        return()
    }
    
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
                    print(signature)
                }
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
