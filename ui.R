# Load required libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(markdown)
library(RMySQL)

# Source separate UI files for each tab
source('upload/upload_ui.R')
source('search/search_ui.R')
source('compare/compare_ui.R')

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
        upload_ui,
        # search signatures page structure
        search_ui,
        # compare signature page structure
        compare_ui,
        # Summary page. has placeholder elements for now
        tabPanel("Summary",
            tableOutput("featurename_table")),
        # second parameter in navbar appending function above
        inputs = textInput(
            inputId = "search",
            label = "",
            placeholder = "Search...",
            width = "100px"
        )
    )
)
