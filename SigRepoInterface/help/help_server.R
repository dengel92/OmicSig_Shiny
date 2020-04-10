# Server logic for help page

output$about_text <-
  renderUI({
    HTML(paste(readLines("help/about.txt"), collapse = "</br></br>"))
  })
