# Search signatures page structure
search_ui <- tabPanel("Search",
    # Create a layout with a sidebar and a main panel
    sidebarLayout(
        # Add the sidebar
        sidebarPanel(
            # Show selected search terms
            htmlOutput(outputId = "search_terms"),
            
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
                inputId = "search_exp_type_id",
                label = "Choose experiment type(s)",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting cell line
            selectizeInput(
                inputId = "search_cell_line",
                label = "Choose cell line(s)",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting perturbagen
            selectizeInput(
                inputId = "search_perturbagen_id",
                label = "Choose perturbagen(s)",
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
                condition = "input.more_filters >= 1",
                # Dropdown menu for selecting submitter name
                selectizeInput(
                    inputId = "search_submitter_id",
                    label = "Choose submitter name(s)",
                    choices = NULL,
                    multiple = TRUE
                )
            ),
            
            # Button to submit search terms
            actionButton("search", "Search Signatures")
        ),
        
        # Add the main panel
        mainPanel(
            # Show checkbox for selecting/unselecting all rows
            conditionalPanel(condition = "input.search >= 1",
                checkboxInput("select_all", "Select/unselect all")),
            # Show table of signatures matching search terms
            div(DT::dataTableOutput("search_results"),
                # If the output table is too wide, add a scrollbar
                style = "overflow-y: scroll"),
            # Download button will appear only after clicking search button
            conditionalPanel(condition = "input.search >= 1",
                downloadButton("search_results_download"))
        )
    ))