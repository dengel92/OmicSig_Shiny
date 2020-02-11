# upload signature page structure
upload_ui <- tabPanel("Upload",
    sidebarLayout(
        sidebarPanel(
          tags$h2("Upload Form"),
          tags$b("Example Files: "),
          tags$a(href = "http://localhost/example_files/example_lv1.txt", "Level 1",download=T),
          tags$a(href = "http://localhost/example_files/example_lv2.txt", "Level 2",download=T),
          tags$a(href = "http://localhost/example_files/example_lv3.txt", "Level 3",download=T),
          br(),br(),br(),
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
                inputId = "rds_file_2",
                label = "Signature File (Lvl2)",
                multiple = FALSE,
                accept = c("text/csv","text/rds",".rds"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            fileInput(
                inputId = "rds_file_3",
                label = "Signature File (Lvl3)",
                multiple = FALSE,
                accept = c("text/csv","text/rds",".rds"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
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