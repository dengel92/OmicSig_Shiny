## Final (?) list of search terms to implement
##### experiment_type
##### phenotype
##### platform_name
##### signature_name
##### source_type
##### species
##### submitter_name
##### upload_date
# feature_name
# feature_type
# keyword

# Search signatures page structure
search_ui <- tabPanel("Search",
    # Create a layout with a sidebar and a main panel
    sidebarLayout(
        # Add the sidebar
        sidebarPanel(
            # Add some space at the top
            dq_space(),
            
            # Button for clearing selected terms
            actionButton("clear", "Clear Search Terms"),
            
            # Show checkbox for displaying selected search terms
            checkboxInput("show_selected", "Show selected search terms"),
            
            # Show selected search terms if checkbox is checked
            conditionalPanel(
                condition = "input.show_selected == 1",
                htmlOutput(outputId = "search_terms")
            ),
            
            # Dropdown menu for selecting species
            selectizeInput(
                "search_species",
                label = "Choose species",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting platform
            selectizeInput(
                inputId = "search_platform_name",
                label = "Choose platform(s)",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting experiment type
            selectizeInput(
                inputId = "search_experiment_type",
                label = "Choose experiment type(s)",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting source type
            selectizeInput(
                inputId = "search_source_type",
                label = "Choose source type(s)",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting phenotype
            # NEED TO CHANGE TO search_phenotype WHEN VIEW IS UPDATED!
            selectizeInput(
                inputId = "search_phenotype_id",
                label = "Choose phenotype(s)",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting signature name
            selectizeInput(
                inputId = "search_signature_name",
                label = "Choose signature name(s)",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Checkbox to toggle additional search options
            checkboxInput("more_filters", "Display additional filters"),
            
            # Additional search options that appear only when "more_filters"
            #   is checked
            conditionalPanel(
                condition = "input.more_filters == 1",
                
                # Dropdown menu for selecting feature names
                selectizeInput(
                    inputId = "search_feature_name",
                    label = "Choose features",
                    choices = NULL,
                    multiple = TRUE
                ),
                
                # Dropdown menu for selecting submitter name
                selectizeInput(
                    inputId = "search_submitter",
                    label = "Choose submitter name(s)",
                    choices = NULL,
                    multiple = TRUE
                ),
                
                # Date range input for selecting upload date
                dateRangeInput("search_upload_date",
                    label = "Choose upload date range",
                    start = "2020-01-01", end = Sys.Date()
                )
            ),
            
            # Button to submit search terms
            actionButton("search", "Search Signatures")
        ),
        
        # Add the main panel
        mainPanel(
            # Add some space at the top
            dq_space(),
            
            # Show checkbox for selecting/unselecting all rows
            conditionalPanel(
                condition = "output.search_results",
                checkboxInput("select_all", "Select/unselect all")
            ),
            
            # Show table of signatures matching search terms
            div(DT::dataTableOutput("search_results"),
                # If the output table is too wide, add a scrollbar
                style = "overflow-y: scroll"),
            
            # Download buttons will appear only when table is displayed
            conditionalPanel(
                condition = "output.search_results",
                
                # Download button for full search results table
                downloadButton("search_results_download", "Download Table"),
                
                # Download button for selected rows
                downloadButton(
                    "selected_search_results_download",
                    "Download Selected Signatures"
                )
            )
        )
    ))