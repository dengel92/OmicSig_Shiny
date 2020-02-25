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
                label = "Choose platform",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting experiment group
            selectizeInput(
                inputId = "search_experiment_type",
                label = "Choose experiment types",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Dropdown menu for selecting signature name
            selectizeInput(
                inputId = "search_signature_name",
                label = "Choose signature names",
                choices = NULL,
                multiple = TRUE
            ),
            
            # Button to submit search terms
            actionButton("search", "Search Signatures")
        ),
        
        # Add the main panel
        mainPanel(
            # Show table of signatures matching search terms
            div(DT::dataTableOutput("search_results"),
                # If the output table is too wide, add a scrollbar
                style = "overflow-y: scroll"),
            # Download button will appear only after clicking search button
            conditionalPanel(condition = "input.search >= 1",
                downloadButton("search_results_download")))
    ))