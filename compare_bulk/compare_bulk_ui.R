compare_bulk_ui <- tabPanel(
  "Compare Bulk",
  # If you guys know a cooler/different layout from sidebarLayout,
  # Please tell me, please
      
      tags$h2("Signature Comparisons"),
      
      fluidRow(
          column(8,offset=2,
                 DT::dataTableOutput("compare_table"))
      ),
  fluidRow(
      column(2, offset = 6, downloadButton("download_result","Download result"))
  )
)
