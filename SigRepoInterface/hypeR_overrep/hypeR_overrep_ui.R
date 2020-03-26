hypeR_overrep_ui <- tabPanel("hypeR_overrep",
  sidebarLayout(
    sidebarPanel(
      # Add some space at the top
      dq_space(),
      
      selectizeInput(
        "overrep_signature",
        label = "select signature",
        choices = NULL,
        multiple = FALSE
      ),
      actionButton("overrep_analysis", label = "Over-representation analysis")
    ),
    mainPanel(
      # Add some space at the top
      dq_space(),
      
      htmlOutput("overrep_introduction"),
      checkboxInput("overrep_show_signatures", "Display signature table"),
      conditionalPanel(
        condition = "input.overrep_show_signatures >= 1",
        c("Signature you selected:"),
        downloadButton("overrep_download_signature", "Download signature table"),
        tableOutput("overrep_signature_df")
      ),
      htmlOutput("overrep_success"),
      conditionalPanel(
        condition = "output.overrep_success",
        downloadButton("overrep_download", label = "Download overrep result"),
        tableOutput("overrep_result"),
        htmlOutput("overrep_show_features")
      )
    )
  ))
