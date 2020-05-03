hypeROverrepUI <- tabPanel(
  "hypeROverrepTab",
  sidebarLayout(
    sidebarPanel(
      dq_space(),
      selectizeInput(
        "hypeROverrepSignature",
        label = "select a signature",
        choices = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        "hypeROverrepSpecies",
        label = "select a species",
        choices = c(
          "Homo sapiens", "Mus musculus", "Drosophila melanogaster", "Gallus gallus",
          "Saccharomyces cerevisiae", "Bos taurus", "Caenorhabditis elegans", "Canis lupus familiaris",
          "Danio rerio", "Rattus norvegicus", "Sus scrofa"
        ),
        multiple = FALSE, selected = "Homo sapiens"
      ),
      selectizeInput(
        "hypeROverrepGsets",
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
      actionButton("hypeROverrepAnalysis", label = "Over-representation analysis"),
      dq_space(),
      fileInput("hypeROverrepCustomGset", label = "Upload customize geneset:"),
      textInput("hypeROverrepCustomGsetName", label = "geneset name", value = "CustomizeGeneSet"),
      actionButton("hypeROverrepCustomGsetAnalysis", label = "Over-representation analysis using customized geneset")
    ),
    mainPanel(
      dq_space(),
      htmlOutput("hypeROverrepIntro"),
      checkboxInput("hypeROverrepShowSignatures", "Display signature table"),
      conditionalPanel(
        condition = "input.hypeROverrepShowSignatures > 0",
        c("Signature you selected:"),
        downloadButton("hypeROverrepDownloadSignature", "Download signature table"),
        DT::dataTableOutput("hypeROverrepSignatureDF")
      ),
      shinycssloaders::withSpinner(htmlOutput("hypeROverrepSuccess")),
      conditionalPanel(
        condition = "output.hypeROverrepSuccess",
        downloadButton("hypeROverrepResultDownload", label = "Download Overrep result"),
        dq_space(),
        shinycssloaders::withSpinner(DT::dataTableOutput("hypeROverrepResult"))
      ),
      shinycssloaders::withSpinner(htmlOutput("hypeROverrepCustomGsetSuccess")),
      conditionalPanel(
        condition = "output.hypeROverrepCustomGsetSuccess",
        downloadButton("hypeROverrepCustomGsetResultDownload", label = "Download Overrep result"),
        dq_space(),
        shinycssloaders::withSpinner(DT::dataTableOutput("hypeROverrepCustomGsetResult"))
      ),
      dq_space(),
      checkboxInput("hypeROverrepShowDescription", "Show result description"),
      conditionalPanel(
        condition = "input.hypeROverrepShowDescription",
        htmlOutput("hypeROverrepDescription")
      )
    )
  )
)
