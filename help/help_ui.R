# Help page structure
help_ui <- tabPanel("Help",
    navbarPage(
        "",
        tabPanel("Upload help",
            htmlOutput(outputId = "upload_help")),
        tabPanel("Search help",
            htmlOutput(outputId = "search_help"))
    ))
