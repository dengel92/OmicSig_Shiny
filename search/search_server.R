# Server logic for search page

# Update species checkboxes with list of species from database 
observe({
  updateCheckboxGroupInput(session,
    "search_species",
    choices = c(get_species())
  )
})

# Show which species have been selected so far
output$search_selected_species <- renderText(
    c(
        "<p><font size=3><b>",
        "Selected Species:</b></font></p><p><font size=2>",
        input$search_species,
        "</p></font>"
    )
)
