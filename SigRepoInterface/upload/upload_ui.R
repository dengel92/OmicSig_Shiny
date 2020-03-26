# upload signature page structure
upload_ui <- tabPanel("Upload",
    sidebarLayout(
        sidebarPanel(
            # Add some space at the top
            dq_space(),
            tags$h2("Upload Form"),
            tags$b("Example Files: "),
            tags$a(href = "http://sigrepo.bu.edu:3838/example_files/example_lv1.txt", "Level 1", download =
                    "level1_example.txt"),
            tags$a(href = "http://sigrepo.bu.edu:3838/example_files/example_lv2.txt", "Level 2", download =
                    "level2_example.txt"),
            tags$a(href = "http://sigrepo.bu.edu:3838/example_files/example_lv3.txt", "Level 3", download =
                    "level3_example.txt"),
            br(),
            br(),
            br(),
            fileInput(
                inputId = "rds_file_1",
                label = "Signature File (Lvl1)",
                multiple = FALSE,
                accept = c("text/csv", "text/rds", ".rds"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            fileInput(
                inputId = "rds_file_2",
                label = "Signature File (Lvl2)",
                multiple = FALSE,
                accept = c("text/csv", "text/txt", ".txt", "text/rds", ".rds"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            fileInput(
                inputId = "rds_file_3",
                label = "Signature File (Lvl3)",
                multiple = FALSE,
                accept = c("text/csv", "text/rds", ".rds"),
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
            textInput(
                inputId = "source_type",
                label = "Source",
                placeholder = NULL
            ),
            selectizeInput(
                inputId = "keywords",
                label = "Keywords(Optional)",
                multiple = TRUE,
                choices = c("a", "b", "c"),
                options = list(create = TRUE)
            ),
            actionButton("add_signature", "Add Signature")
        ),
        
        #
        mainPanel(
            # Add some space at the top
            dq_space(),
            
            tags$h2("Upload directly with OmicSig object file"),
            tags$a(href = "http://sigrepo.bu.edu:3838/example_files/Sum149_AhR_obj.json", "Example JSON File", download =
                    "level1_example.txt"),
            fileInput(
                inputId = "omicobj_upload",
                label = "Object File",
                multiple = FALSE,
                accept = c("text/json", ".json", "text/txt", ".txt"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            textInput(
                inputId = "signature_object_name",
                label = "Signature Name",
                placeholder = NULL
            ),
            actionButton("upload_object", "Upload with File"),
            #if feature symbols in signature don't match up in DB
            #will get message in this div
            div(textOutput("errday"))
        )
    ))
