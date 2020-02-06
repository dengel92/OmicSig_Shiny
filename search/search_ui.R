# search signatures page structure
search_ui <- tabPanel("Search",
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "signature_name",
                label = "Signature Name",
                placeholder = NULL
            ),
            textInput(
                inputId = "group_id",
                label = "Collection Name",
                placeholder = NULL
            ),
            checkboxGroupInput("organism_check",
                label = "Choose organism(s)",
                choices = NULL,
                selected = NULL),
            selectizeInput(
                inputId = "platform_name",
                label = "Platform",
                choices = NULL
            ),
            textInput(
                inputId = "cell_line",
                label = "Cell Line(ATCC)",
                placeholder = NULL
            ),
            selectizeInput(
                inputId = "keywords",
                label = "Keywords",
                multiple = TRUE,
                choices = c("a", "b", "c"),
                options=list(create=TRUE)
            )
        ),
        
        # 
        mainPanel("Display table of matching signatures?")
    ))