# Server logic for help page

output$upload_help <-
    renderText("This is the help page for uploading signatures")

output$search_help <-
    renderText("This is the help page for searching signatures")

observeEvent(input$upload_faqs, print(input$upload_faqs))

observeEvent(input$search_faqs, print(input$search_faqs))