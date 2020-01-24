library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Upload Signature"),
    
    tabPanel("Search Signatures")
  )
)

server <- function(input, output) {

}

shinyApp(ui, server)
