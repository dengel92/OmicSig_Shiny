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
            
            # Button to submit search terms
            actionButton("search", "Search Signatures")
        ),
        
        # Add the main panel
        mainPanel(
            # If the output table is too wide, add a scrollbar
            style = "overflow-y:scroll",
            # Show table of signatures matching search terms
            tableOutput("search_results"))
    ))