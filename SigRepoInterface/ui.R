# Load required libraries
library(shiny)
library(dqshiny)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(markdown)
library(RMySQL)
library(plotly)

# Source separate UI files for each tab
source('upload/upload_ui.R')
source('search/search_ui.R')
source('compare/compare_ui.R')
source('compare_bulk/compare_bulk_ui.R')
source('hypeR/hypeR_overrep_ui.R')
source('hypeR/hypeR_enrich_ui.R')
source('help/help_ui.R')

# Function that appends elements to a navbar
# Intended purpose is to add a text search input in navbar,
#   but can be applied with multiple different elements
navbarPageWithInputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form", inputs)
    navbar[[3]][[1]]$children[[1]] <-
        htmltools::tagAppendChild(navbar[[3]][[1]]$children[[1]], form)
    navbar
}

# Define UI
ui <- fluidPage(
    shinyjs::useShinyjs(),
    # Allows for alert feature
    useShinyalert(),
    theme = shinytheme("sandstone"),
    # Page header/title
    tags$h1("SigRepo"),
    navbarPageWithInputs(
        # Navbar header
        "SigRepo",
        # Locks header to top of screen when scrolling
        position = "fixed-top",
        # UI structure for each separate tab
        help_ui,
        upload_ui,
        search_ui,
        # Compare and compare_bulk show up in a dropdown
        navbarMenu("Compare",
            compare_ui,
            compare_bulk_ui),
        # hypeR overrep and enrichment show up in a dropdown
        navbarMenu("hypeR",
            hypeR_overrep_ui,
            hypeR_enrich_ui),
        # second parameter in navbar appending function above
        inputs = textInput(
            inputId = "search",
            label = "",
            placeholder = "Search...",
            width = "100px"
        )
    )
)
