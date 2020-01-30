# Source the files containing the UI and server definitions
source('ui.R')
source('server.R')

# Run the app
shinyApp(ui, server)