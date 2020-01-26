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
  theme=shinytheme("sandstone"),
  tags$h1("REEEEEEEE"),
  navbarPageWithInputs(
  "SigRepo",
  tabPanel("Plot",
           sidebarLayout(
             sidebarPanel(
               textInput(inputId="firstone",label="name",placeholder=NULL),
               textInput(inputId="second",label="signature type",placeholder=NULL),
               textInput(inputId="third",label="species",placeholder=NULL),
               textInput(inputId="fourth",label="platform",placeholder=NULL),
               textInput(inputId="fifth",label="etc",placeholder=NULL)
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

