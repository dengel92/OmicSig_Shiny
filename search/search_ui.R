# search signatures page structure
search_ui <- tabPanel("Search",
    sidebarLayout(
        sidebarPanel(
            htmlOutput(outputId = "search_selected_species"),
            htmlOutput(outputId = "search_selected_platforms"),
            selectizeInput(
                "search_species",
                label = "Choose species",
                choices = NULL,
                multiple = TRUE
            ),
            selectizeInput(
                inputId = "search_platform_name",
                label = "Platform",
                choices = NULL,
                multiple = TRUE
            ),
            actionButton("search", "Search Signatures")
        ),
        
        mainPanel(
            tableOutput("search_results")
        )
    ))