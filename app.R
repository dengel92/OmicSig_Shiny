library(shiny)

ui <- fluidPage(
    tabsetPanel(
        tabPanel("Upload Signature"
        )
    )
)

server <- function(input, output) {
    
}

shinyApp(ui, server)