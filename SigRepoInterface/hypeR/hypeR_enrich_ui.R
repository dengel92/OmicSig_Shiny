hypeREnrichUI <- tabPanel(
    "hypeREnrichTab",
    sidebarLayout(
        sidebarPanel(
            dq_space(),
            selectizeInput(
                "hypeREnrichSignature",
                label = "select a signature",
                choices = NULL,
                multiple = FALSE
            ),
            selectizeInput(
                "hypeREnrichSpecies",
                label = "select a species",
                choices = c(
                    "Homo sapiens", "Mus musculus", "Drosophila melanogaster", "Gallus gallus",
                    "Saccharomyces cerevisiae", "Bos taurus", "Caenorhabditis elegans", "Canis lupus familiaris",
                    "Danio rerio", "Rattus norvegicus", "Sus scrofa"
                ),
                multiple = FALSE, selected = "Homo sapiens"
            ),
            selectizeInput(
                "hypeREnrichGsets",
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
            actionButton("hypeREnrichAnalysis", label = "Rank-based enrichment analysis"),
            dq_space(),
            fileInput("hypeREnrichCustomGset", label = "Upload customize geneset:"),
            textInput("hypeREnrichCustomGsetName", label = "geneset name", value = "CustomizeGeneSet"),
            actionButton("hypeREnrichCustomGsetAnalysis", label = "Rank-based enrichment analysis using customized geneset")
        ),
        mainPanel(
            dq_space(),
            htmlOutput("hypeREnrichIntro"),
            checkboxInput("hypeREnrichShowAllFeatures", "Display all features table"),
            conditionalPanel(
                condition = "input.hypeREnrichShowAllFeatures > 0",
                downloadButton("hypeREnrichDownloadAllFeatures", "Download signature table"),
                dq_space(),
                shinycssloaders::withSpinner(DT::dataTableOutput("hypeREnrichAllFeaturesDF"))
            ),
            shinycssloaders::withSpinner(htmlOutput("hypeREnrichSuccess")),
            conditionalPanel(
                condition = "output.hypeREnrichSuccess",
                downloadButton("hypeREnrichResultDownload", label = "Download Enrichment result"),
                dq_space(),
                shinycssloaders::withSpinner(DT::dataTableOutput("hypeREnrichResult"))
            ),
            shinycssloaders::withSpinner(htmlOutput("hypeREnrichCustomGsetSuccess")),
            conditionalPanel(
                condition = "output.hypeREnrichCustomGsetSuccess",
                downloadButton("hypeREnrichCustomGsetResultDownload", label = "Download Enrichment result"),
                dq_space(),
                shinycssloaders::withSpinner(DT::dataTableOutput("hypeREnrichCustomGsetResult"))
            ),
            dq_space(),
            checkboxInput("hypeREnrichShowDescription", "Show result description"),
            conditionalPanel(
                condition = "input.hypeREnrichShowDescription",
                htmlOutput("hypeREnrichDescription")
            )
        )
    )
)

