hypeR_overrep_ui <- tabPanel(
  "hypeR_overrep_analysis",
  sidebarLayout(
    sidebarPanel(
      dq_space(),
      selectizeInput(
        "hypeR_overrep_signature",
        label = "select a signature",
        choices = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        "hypeR_overrep_species",
        label = "select a species",
        choices = c(
          "Homo sapiens", "Mus musculus", "Drosophila melanogaster", "Gallus gallus",
          "Saccharomyces cerevisiae", "Bos taurus", "Caenorhabditis elegans", "Canis lupus familiaris",
          "Danio rerio", "Rattus norvegicus", "Sus scrofa"
        ),
        multiple = FALSE, selected = "Homo sapiens"
      ),
      selectizeInput(
        "hypeR_overrep_gsets",
        label = "select gene set(s)",
        choices = c(
          "C1__[Positional]", "C2_CGP_[Chemical and Genetic Perturbations]",
          "C2_CP_[Canonical Pathways]", "C2_CP:BIOCARTA_[Canonical BIOCARTA]",
          "C2_CP:KEGG_[Canonical KEGG]", "C2_CP:PID_[Canonical PID]",
          "C2_CP:REACTOME_[Canonical REACTOME]", "C3_MIR_[Motif miRNA Targets]",
          "C3_TFT_[Motif Transcription Factor Targets]", "C4_CGN_[Cancer Gene Neighborhoods]",
          "C4_CM[Cancer Modules]", "C5_BP[GO Biological Process]", "C5_CC[GO Cellular Component]",
          "C5_MF[GO Molecular Function]", "C6__[Oncogenic Signatures]",
          "C7__[Immunologic Signatures]", "H__[Hallmark]"
        ),
        multiple = TRUE
      ),
      actionButton("hypeR_overrep_analysis", label = "Over-representation analysis")
    ),
    mainPanel(
      dq_space(),
      htmlOutput("hypeR_overrep_introduction"),
      checkboxInput("hypeR_show_signatures", "Display signature table"),
      conditionalPanel(
        condition = "input.hypeR_show_signatures > 0",
        c("Signature you selected:"),
        downloadButton("hypeR_download_signature", "Download signature table"),
        DT::dataTableOutput("hypeR_signature_df")
      ),


      shinycssloaders::withSpinner(htmlOutput("hypeR_overrep_success")),
      conditionalPanel(
        condition = "output.hypeR_overrep_success",
        checkboxInput("hypeR_overrep_show_description", "Show result description"),
        conditionalPanel(
          condition = "input.hypeR_overrep_show_description",
          htmlOutput("hypeR_overrep_description")
        ),
        downloadButton("hypeR_overrep_download", label = "Download Overrep result"),
        dq_space(),
        shinycssloaders::withSpinner(DT::dataTableOutput("hypeR_overrep_result"))
      )
    )
  )
)
