library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Upload Signature"),
    
    tabPanel("Search Signatures",
        "Here you can search for all signatures matching certain criteria.")
  )
)

server <- function(input, output) {

}

shinyApp(ui, server)
