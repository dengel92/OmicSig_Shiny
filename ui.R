# Load required libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(markdown)
library(RMySQL)

# function that appends elements to a navbar
# intended purpose is to add a text search input in navbar
# but can be applied with multiple different elements
navbarPageWithInputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form", inputs)
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(navbar[[3]][[1]]$children[[1]], form)
    navbar
}

# Define UI
ui <- fluidPage(
    #allows for alert feature
    useShinyalert(),
    theme = shinytheme("sandstone"),
    tags$h1("SigRepo"),
    navbarPageWithInputs(
        # original navbar setup
        "SigRepo",
        # upload signature page structure
        tabPanel("Upload",
            sidebarLayout(
                sidebarPanel(
                    fileInput(
                        inputId = "rds_file_1",
                        label = "Signature File",
                        multiple = FALSE,
                        accept = c("text/csv","text/rds",".rds"),
                        width = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"
                    ),
                    fileInput(
                      inputId = "rds_file_2",
                      label = "Signature File",
                      multiple = FALSE,
                      accept = c("text/csv","text/rds",".rds"),
                      width = NULL,
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"
                    ),
                    fileInput(
                      inputId = "rds_file_3",
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
                        inputId = "platform_id",
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
                mainPanel("could put something here")
            )),
        # Summary page. has placeholder elements for now
        tabPanel("Summary",
            tableOutput("featurename_table")),
        # second parameter in navbar appending function above
        inputs = textInput(
            inputId = "search",
            label = "",
            placeholder = "REEEEE",
            width = "100px"
        )
    )
)
