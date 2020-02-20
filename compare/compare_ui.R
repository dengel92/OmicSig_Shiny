compare_ui <- tabPanel(
  "Compare",
  # If you guys know a cooler/different layout from sidebarLayout,
  # Please tell me, please
  tags$style(HTML(
      "
        .result_header {
            font-weight: 4;
        }
    
        #first_sig_compare_header{
            color: #228822;
        }

        #second_sig_compare_header{
            color: #882288;
        }

        #ty_bb{
            color: #AB6611;
        }
      "
  )),
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
