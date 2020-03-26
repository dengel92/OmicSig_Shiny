# Help page structure

# Set up accordion menu headers and contents
accordion_titles <-
    c("About",
        "Why are these panels all the same length?",
        "Uploading a new signature")

accordion_contents <- list(
    paste0(
        "Our signatures are mainly from publications, publically available ",
        "datasets, and data collected in the lab. To collect gene signatures ",
        "from microarray data, we performed differential analysis on these ",
        "datasets by using limma, an R Bioconductor package for analyzing ",
        "microarray data. Afterwards, gene signatures were defined with genes ",
        "that passed certain thresholds. \nSignatures are represented in three ",
        "levels (\"lvs\"), becoming more general with increasing level. Lv1 ",
        "contains the raw expression analysis table output from the appropriate ",
        "R Bioconductor package (e.g. limma for microarray data). Lv2 holds a ",
        "feature list, paired with a \"weight\" that denotes relative expression ",
        "of each feature in that experiment. Lv3 holds the feature labels whose ",
        "test statistic, such as q-value or t statistic, exceeds a given ",
        "established threshold in the experiment. Although our current scope of ",
        "analysis is confined to gene expression, we intend to make our platform ",
        "modular, allowing access and analysis of expression profiles of other ",
        "features (e.g. microbial communities, metabolites)."
    ),
    paste0("The content of this panel of the accordion menu is very short, ",
    "but the panel is still very large. :("),
    "Here's some information on uploading a new signature."
)

# Make accordion menu
accordion_menu <- dq_accordion(
    "accordion_menu",
    accordion_titles,
    accordion_contents,
    # These options allow all of the panels to be collapsed at once
    options = list(active = FALSE, collapsible = TRUE),
    bg_color = "#f8f5f0"
)

# Help page structure
help_ui <- tabPanel("Help",
    fluidPage(title = "Help",
        dq_space(),
        accordion_menu))