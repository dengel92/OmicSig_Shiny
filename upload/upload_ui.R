# upload signature page structure
upload_ui <- tabPanel("Upload",
    sidebarLayout(
        sidebarPanel(
            fileInput(
                inputId = "rds_file_1",
                label = "Signature File (Lvl1)",
                multiple = FALSE,
                accept = c("text/csv","text/rds",".rds"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            fileInput(
                inputId = "rds_file_2(Lvl2)",
                label = "Signature File",
                multiple = FALSE,
                accept = c("text/csv","text/rds",".rds"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            fileInput(
                inputId = "rds_file_3(Lvl3)",
                label = "Signature File",
                multiple = FALSE,
                accept = c("text/csv","text/rds",".rds"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            HTML(
                paste(
                    "Would be good to link to an example rds file here",
                    "(Maybe for all levels)"
                ),
                sep = "<br/></br>"
            ),
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
            selectizeInput(
                inputId = "species_id",
                label = "Species",
                choices = NULL
            ),
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
                label = "Keywords(Optional)",
                multiple = TRUE,
                choices = c("a", "b", "c"),
                options=list(create=TRUE)
            ),
            actionButton("add_signature", "Add Signature")
        ),
        
        # 
        mainPanel("could put something here. Display preview of uploaded file?")
    ))