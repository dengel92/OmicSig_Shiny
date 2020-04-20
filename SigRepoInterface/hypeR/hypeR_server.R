source("server_functions/Function_hypeR.R")
observe({
  updateSelectizeInput(
    session,
    "hypeR_signature",
    choices = c(
      "",
      get_signature_names()
    )
  )
})

output$hypeR_introduction <- renderText({
  c(
    "<p><h2>Over-representation/Enrichment Tests</h2></p>",
    "<h3>Suggested Example Uses :</h3>",
    "<br> *Sum149_CYP1B1 and C2_CP Canonical Pathways and/or Hallmark*
     <br> run hypeR::msigdb_info() to learn more about the available species and gene sets."
  )
})

hypeR_signature_df_variable <- reactive({
  df <- sql_generic(paste(
    "select feature_name, 
    weight, 
    direction 
    from feature_signature_view 
    where signature_name =",
    single_quoted(input$hypeR_signature), ";",
    sep = ""
  ))
  df <- df[order(df$weight, decreasing = T), ]
  colnames(df) <- c("signature_symbol", "signature_score", "signature_direction")
  return(df)
})

output$hypeR_signature_df <- renderTable({
  hypeR_signature_df_variable()
})

output$hypeR_download_signature <- downloadHandler(
  filename = paste(as.character(input$hypeR_signature), "_signature.tsv", sep = ""),
  content = function(file) {
    write.table(hypeR_signature_df_variable(), file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

hypeR_overrep_result_variable <- eventReactive(input$hypeR_overrep_analysis, {
  sig <- hypeR_signature_df_variable()
  gset_names <- data.frame(
    species = character(0),
    category = character(0),
    subcategory = character(0)
  )
  for (i in c(input$hypeR_gsets)) {
    gset_names <- rbind(gset_names, c(input$hypeR_species, unlist(strsplit(i, "_"))[1:2]), stringsAsFactors = FALSE)
  }
  colnames(gset_names) <- c("species", "category", "subcategory")
  result <- hypeR_overrep_enrich_server_function(sig, gset_names, test = "overrep")
  result$sig_name <- input$hypeR_signature
  return(result)
})

hypeR_enrich_result_variable <- eventReactive(input$hypeR_enrich_analysis, {
  # retrieve lv1 signature and add direction:
  sig <- retrieve_omicsig(input$hypeR_signature)$difexp[, c("symbol", "score")]
  direction <- rep("+", nrow(sig))
  direction[which(sig$score < 0)] <- "-"
  sig <- cbind(sig, direction)
  colnames(sig) <- c("signature_symbol", "signature_score", "signature_direction")
  
  # load genesets:
  gset_names <- data.frame(
    species = character(0),
    category = character(0),
    subcategory = character(0)
  )
  for (i in c(input$hypeR_gsets)) {
    gset_names <- rbind(gset_names, c(input$hypeR_species, unlist(strsplit(i, "_"))[1:2]), stringsAsFactors = FALSE)
  }
  colnames(gset_names) <- c("species", "category", "subcategory")
  
  # perform hypeR analysis:
  result <- hypeR_overrep_enrich_server_function(sig, gset_names, test = "enrich")
  result$sig_name <- input$hypeR_signature
  return(result)
})

output$hypeR_overrep_success <- renderText({
  c(
    "<h2>Sample: ",
    hypeR_overrep_result_variable()$sig_name,
    "</h2><h3>Species: ",
    input$hypeR_species,
    "</h3><h3>Gene Sets: ",
    input$hypeR_gsets, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> at this moment, the p-value cutoff of the analysis is very non-stringent, so it's probably outputing all the genesets that are found to have any overlap with the given signature list. - at Apr 2020. </p>"
  )
})

output$hypeR_overrep_description <- renderText({
  c(
    "<p>The meaning of each column:",
    "<br> direction: the direction of the feature (genes) in the given signature, if available.
        <br> category: category of the gene set (e.g. C2). Source: http://software.broadinstitute.org/gsea/msigdb.
        <br> subcategory: subcategory of the gene set (e.g. KEGG). Source: http://software.broadinstitute.org/gsea/msigdb.
        <br> label: pathway or geneset names.
        <br> pval: p-value of the over-representation of that pathway or geneset.
        <br> fdr: p-value after fdr correction.
        <br> geneset: total amount of genes in that pathway or geneset.
        <br> overlap: the number of significant overlapping genes between the signature and the geneset.
        <br> hits: symbol of the significant overlapping genes.",
    "</p>"
  )
})

output$hypeR_enrich_success <- renderText({
  c(
    "<h2>Sample: ",
    hypeR_enrich_result_variable()$sig_name,
    "</h2><h3>Species: ",
    input$hypeR_species,
    "</h3><h3>Gene Sets: ",
    input$hypeR_gsets, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> at this moment, the p-value cutoff of the analysis is very non-stringent, so it's probably outputing all the genesets that are found to have any overlap with the given signature list. - at Apr 2020. </p>"
  )
})

output$hypeR_enrich_description <- renderText({
  c(
    "<p>The meaning of each column:",
    "<br> direction: the direction of the feature (genes) in the given signature, if available.
        <br> category: category of the gene set (e.g. C2). Source: http://software.broadinstitute.org/gsea/msigdb.
        <br> subcategory: subcategory of the gene set (e.g. KEGG). Source: http://software.broadinstitute.org/gsea/msigdb.
        <br> label: pathway or geneset names.
        <br> pval: p-value of the over-representation of that pathway or geneset.
        <br> fdr: p-value after fdr correction.
        <br> geneset: total amount of genes in that pathway or geneset.
        <br> overlap: the number of significant overlapping genes between the signature and the geneset.
        <br> score: enrichment score.",
    "</p>"
  )
})

observeEvent(input$hypeR_overrep_analysis, {
  updateTabsetPanel(session, "hypeR_tabs",
    selected = "overrep_tab"
  )
})

observeEvent(input$hypeR_enrich_analysis, {
  updateTabsetPanel(session, "hypeR_tabs",
    selected = "enrichment_tab"
  )
})

output$hypeR_overrep_result <- DT::renderDataTable({
  if (!is.null(hypeR_overrep_result_variable()$result)) {
    hypeR_overrep_result_variable()$result
  }
})

output$hypeR_enrich_result <- DT::renderDataTable({
  if (!is.null(hypeR_enrich_result_variable()$result)) {
    hypeR_enrich_result_variable()$result
  }
})

output$hypeR_overrep_download <- downloadHandler(
  filename = paste(hypeR_overrep_result_variable()$sig_name, "_overrep.tsv", sep = ""),
  content = function(file) {
    write.table(hypeR_overrep_result_variable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
output$hypeR_enrich_download <- downloadHandler(
  filename = paste(hypeR_enrich_result_variable()$sig_name, "_enrich.tsv", sep = ""),
  content = function(file) {
    write.table(hypeR_enrich_result_variable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
