# compare signature page structure
compare_ui <- tabPanel("Compare",
  tags$div(class="um",
           actionButton("add","add row?"),
           actionButton("subtract","subtract row?")
  )
)
