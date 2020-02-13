# search signatures page structure
search_ui <- tabPanel("Search",
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "search_signature_name",
                label = "Signature Name",
                placeholder = NULL
            ),
            textInput(
                inputId = "search_group_id",
                label = "Collection Name",
                placeholder = NULL
            ),
            htmlOutput(
                outputId = "search_selected_species"
            ),
            checkboxGroupInput("search_species",
                label = "Choose species",
                choices = NULL,
                selected = NULL),
            selectizeInput(
                inputId = "search_platform_name",
                label = "Platform",
                choices = NULL
            ),
            textInput(
                inputId = "search_cell_line",
                label = "Cell Line(ATCC)",
                placeholder = NULL
            ),
            selectizeInput(
                inputId = "search_keywords",
                label = "Keywords",
                multiple = TRUE,
                choices = c("a", "b", "c"),
                options=list(create=TRUE)
            )
        ),
            actionButton("search", "Search Signatures")
        ),
        
        mainPanel(
            tableOutput("search_results")
        )
    )