gsea_ui <- tabPanel(
  "GSEA Analysis",
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "gsea_signature",
        label = "select signature",
        choices = NULL,
        multiple = FALSE
      ),
      actionButton("gsea_analysis", "Perform GSEA analysis")
    ),
    mainPanel(
      htmlOutput("gsea_introduction"),
      HTML("<hr color='brown' >"),
      c("Signature you selected:"),
      tableOutput("gsea_signature_df"),
      c("GSEA result:"),
      tableOutput("gsea_result"),
      htmlOutput("gsea_show_features")
    )
  )
)
