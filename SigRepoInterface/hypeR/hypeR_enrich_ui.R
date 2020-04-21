hypeR_enrich_ui <- tabPanel(
  "hypeR_enrich_analysis",
  sidebarLayout(
    sidebarPanel(
      dq_space(),
      selectizeInput(
        "hypeR_enrich_signature",
        label = "select a signature",
        choices = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        "hypeR_enrich_species",
        label = "select a species",
        choices = c(
          "Homo sapiens", "Mus musculus", "Drosophila melanogaster", "Gallus gallus",
          "Saccharomyces cerevisiae", "Bos taurus", "Caenorhabditis elegans", "Canis lupus familiaris",
          "Danio rerio", "Rattus norvegicus", "Sus scrofa"
        ),
        multiple = FALSE, selected = "Homo sapiens"
      ),
      selectizeInput(
        "hypeR_enrich_gsets",
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
      actionButton("hypeR_enrich_analysis", label = "Rank-based enrichment analysis"),
      dq_space(),
      fileInput("hypeR_enrich_cust_gset", label = "Upload customize geneset:"),
      textInput("hypeR_enrich_cust_gset_name", label = "geneset name", value = "customize_gset"),
      actionButton("hypeR_enrich_cust_analysis", label = "Rank-based enrichment analysis using customized geneset")
    ),
    mainPanel(
      dq_space(),
      htmlOutput("hypeR_enrich_introduction"),
      checkboxInput("hypeR_show_all_features", "Display all features table"),
      conditionalPanel(
        condition = "input.hypeR_show_all_features > 0",
        downloadButton("hypeR_download_all_features", "Download signature table"),
        dq_space(),
        shinycssloaders::withSpinner(DT::dataTableOutput("hypeR_all_features_df"))
      ),
      shinycssloaders::withSpinner(htmlOutput("hypeR_enrich_success")),
      conditionalPanel(
        condition = "output.hypeR_enrich_success",
        downloadButton("hypeR_enrich_download", label = "Download Enrichment result"),
        dq_space(),
        shinycssloaders::withSpinner(DT::dataTableOutput("hypeR_enrich_result"))
      ),
      shinycssloaders::withSpinner(htmlOutput("hypeR_enrich_cust_success")),
      conditionalPanel(
        condition = "output.hypeR_enrich_cust_success",
        downloadButton("hypeR_enrich_cust_download", label = "Download Enrichment result"),
        dq_space(),
        shinycssloaders::withSpinner(DT::dataTableOutput("hypeR_enrich_cust_result"))
      ),
      dq_space(),
      checkboxInput("hypeR_enrich_show_description", "Show result description"),
      conditionalPanel(
        condition = "input.hypeR_enrich_show_description",
        htmlOutput("hypeR_enrich_description")
      )
    )
  )
)
