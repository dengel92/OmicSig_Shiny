#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(markdown)
library(RMySQL)


# Define UI for application that will do something at some point

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

ui_questionmark <- fluidPage(
  useShinyalert(),
  theme=shinytheme("sandstone"),
  tags$h1("REEEEEEEE"),
  navbarPageWithInputs(
  "SigRepo",
  tabPanel("Upload",
           sidebarLayout(
             sidebarPanel(
               fileInput(inputId="rds_file", label="Signature File", multiple = FALSE, accept = NULL,
                         width = NULL, buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               HTML(
                 paste("Would be good to link to an example rds file here",
                          "(Maybe for all levels)"), sep="<br/></br>"),
               textInput(inputId="signature_name",label="Signature Name",placeholder=NULL),
               textInput(inputId="group_id",label="Group Name",placeholder=NULL),
               selectizeInput(inputId="species_id",label="Species",choices=NULL),
               textInput(inputId="platform_id",label="Platform",placeholder=NULL),
               textInput(inputId="cell_line",label="Cell Line(ATCC)",placeholder=NULL),
               textInput(inputId="keywords",label="Keywords(Optional)"),
               actionButton("add_signature","Add Signature")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               "could put something here"
             )
           )),
  tabPanel("Summary",
           tableOutput("featurename_table")
           ), 
  inputs=textInput(
    inputId="search",
    label="",
    placeholder="REEEEE",
    width="100px"
  )
))

