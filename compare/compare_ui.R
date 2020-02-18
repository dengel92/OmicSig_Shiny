compare_ui <- tabPanel(
  "Compare",
  # If you guys know a cooler/different layout from sidebarLayout,
  # Please tell me, please
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "compare_1",
        label = "Signature 1",
        choices = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        "compare_2",
        label = "Signature 2",
        choices = NULL,
        multiple = FALSE
      ),
      numericInput("compare_background_number", label = "Background", value = 22000),
      actionButton("compare_signatures", "Compare Signatures")
    ),
    mainPanel(
      htmlOutput("compare_result"),
      plotOutput("compare_result_Venn"),
      htmlOutput("compare_show_signatures")
    )
  )
)
