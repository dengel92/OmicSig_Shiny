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
      actionButton("gsea_analysis", label = "Perform GSEA analysis")
    ),
    mainPanel(
      htmlOutput("gsea_introduction"),
      checkboxInput("gsea_show_signatures", "Display signature table"),
      conditionalPanel(
        condition = "input.gsea_show_signatures >= 1",
        c("Signature you selected:"),
        tableOutput("gsea_signature_df"),
        downloadButton("gesa_download_signature", "Download signature table")
      ),
      htmlOutput("gsea_success"),
      conditionalPanel(
        condition = "output.gsea_success",
        tableOutput("gsea_result"),
        downloadButton("gesa_download", label = "Download GSEA result"),
        htmlOutput("gsea_show_features")
      )
    )
  )
)
