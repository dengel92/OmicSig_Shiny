# Server logic for search page

# Source file containing general functions for the search page
source("search/search_functions.R", local = TRUE)

# Make a data frame storing all the relevant information about each input widget
# id = the input ID of the widget
# name = the name of the database field corresponding to the widget
# display = the text to display in the selected search terms section of the app
# type = the type of widget (i.e. dropdown)
widgets <-
    rbind.data.frame(
        c("search_signature_name", "signature_name", "signatures", "dropdown_signature"),
        c("search_species", "species", "species", "dropdown"),
        c("search_experiment_type", "experiment_type", "experiment types", "dropdown"),
        c("search_platform_name", "platform_name", "platforms", "dropdown"),
        c("search_source_type", "source_type", "source types", "dropdown"),
        c("search_phenotype", "phenotype", "phenotypes", "dropdown"),
        c("search_feature_type", "feature_type", "feature types", "dropdown_feature"),
        c("search_feature_name", "feature_name", "feature names", "dropdown_feature"),
        c("search_keyword", "keyword", "keywords", "dropdown_keyword"),
        c("search_submitter", "submitter", "submitters", "dropdown"),
        c("search_upload_date", "upload_date", "upload date (start, end)", "date"),
        stringsAsFactors = FALSE
    )
colnames(widgets) <- c("id", "name", "display", "type")

# Extract the row numbers for each type of dropdown menu
dropdown_rows <- which(widgets$type == "dropdown")
dropdown_signature_rows <- which(widgets$type == "dropdown_signature")
dropdown_feature_rows <- which(widgets$type == "dropdown_feature")
dropdown_keyword_rows <- which(widgets$type == "dropdown_keyword")

# Clear selected terms after clicking clear button
observeEvent(input$clear, {
    isolate({
        # Clear all dropdowns
        for (dropdown in widgets[c(dropdown_signature_rows, dropdown_rows), ]$name) {
            clear_dropdown(dropdown, "platform_signature_view")
        }
        for (dropdown in widgets[dropdown_feature_rows, ]$name) {
            clear_dropdown(dropdown, "feature_signature_view")
        }
        for (dropdown in widgets[dropdown_keyword_rows, ]$name) {
            clear_dropdown(dropdown, "keyword_signature_view")
        }
        
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

# Construct list of in clauses
ins <- reactive({
    # Initialize list to store result
    ins_list <- list()
    dropdowns <- widgets[c(dropdown_signature_rows, dropdown_rows), ]
    # Loop through rows of dropdowns dataframe
    for (row in 1:dim(dropdowns)[1]) {
        # Extract field name and dropdown id
        name <- dropdowns[row, ]$name
        id <- dropdowns[row, ]$id
        # Add an element whose name is the field and whose value is the
        #   input from the dropdown
        ins_list[[name]] <- input[[id]]
    }
    return(ins_list)
})

# Construct list of between clauses
betweens <- reactive({
    list(
        "upload_date" = input[["search_upload_date"]]
    )
})

# Construct list of feature types and names
features <- reactive({
    list(
        "feature_name" = input[["search_feature_name"]],
        "feature_type" = input[["search_feature_type"]]
    )
})

# Construct list of keywords
keywords <- reactive({
    list(
        "keyword" = input[["search_keyword"]]
    )
})

observe({
    # Disable widgets until they finish updating
    lapply(widgets$id, disable)
    
    # Get list of in clauses from reactive function
    ins <- ins()
    feature_signatures <- list("signature_name" = ins[["signature_name"]])
    keyword_signatures <- feature_signatures
    
    # If any features are selected, update signature_name in list of in clauses
    # Also get list of all signature names corresponding to selected feature
    #   names and types to use when updating keyword dropdown menu
    if (length(compact(features())) > 0) {
        feature_signatures <- get_intersection("signature_name", "feature_signature_view",
            features(), ins())
        ins <- feature_signatures
        feature_signatures <- list("signature_name" = feature_signatures[["signature_name"]])
    }
    
    # If any keywords are selected, update signature_name in list of in clauses
    # Also get list of all signature names corresponding to selected keywords
    #   to use when updating feature name and type dropdown menus
    if (length(compact(keywords())) > 0) {
        keyword_signatures <- get_intersection("signature_name", "keyword_signature_view",
            keywords(), ins())
        ins <- get_intersection("signature_name", "keyword_signature_view",
            keywords(), ins)
        keyword_signatures <- list("signature_name" = keyword_signatures[["signature_name"]])
    }
    
    # Update signature name dropdown menu
    if (length(ins[["signature_name"]]) > 0) {
        if (length(compact(c(features(), keywords()))) > 0) {
            updateSelectizeInput(
                session,
                "search_signature_name",
                choices = c(ins[["signature_name"]], input[["search_signature_name"]]),
                selected = input[["search_signature_name"]]
            )
        } else {
            update_dropdown("signature_name", "platform_signature_view",
                ins, betweens())
        }
    } else {
        signatures <- get_field_values("signature_name",
            "platform_signature_view", ins, betweens())
        updateSelectizeInput(
            session,
            "search_signature_name",
            choices = c(signatures, input[["search_signature_name"]]),
            selected = input[["search_signature_name"]]
        )
        # Also update list of signature names to use when updating feature and
        #   keyword dropdowns
        feature_signatures <- list("signature_name" = signatures)
        keyword_signatures <- feature_signatures
    }
    
    # Update features dropdown menus
    for (dropdown in widgets[dropdown_feature_rows, ]$name) {
        update_dropdown(dropdown, "feature_signature_view",
            c(keyword_signatures, features()), NULL)
    }
    
    # Update keyword dropdown menu
    for (dropdown in widgets[dropdown_keyword_rows, ]$name) {
        update_dropdown(dropdown, "keyword_signature_view", feature_signatures,
            NULL)
    }
    
    # Update remaining dropdown menus
    for (dropdown in widgets[dropdown_rows, ]$name) {
        update_dropdown(dropdown, "platform_signature_view", ins, betweens())
    }
    
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
    
    # Get list of in clauses from reactive function
    ins <- ins()
    
    # If any features are selected, update signature_name in list of in clauses
    if (length(compact(features())) > 0) {
        ins <- get_intersection("signature_name", "feature_signature_view",
            features(), ins)
    }
    
    # If any keywords are selected, update signature_name in list of in clauses
    if (length(compact(keywords())) > 0) {
        ins <- get_intersection("signature_name", "keyword_signature_view",
            keywords(), ins)
    }
    
    # Search database for matching signatures
    sql_obj <-
        sql_finding_query(
            fields = c(
                "signature_name",
                "species",
                "experiment_type",
                "platform_name",
                "source_type",
                "phenotype",
                "submitter",
                "upload_date"
            ),
            target_table = "platform_signature_view",
            ins = ins,
            betweens = betweens()
        )
    
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

# Update displayed table with list of selected rows
output$selected_rows <-
    renderPrint(print(input$search_results_rows_selected))
