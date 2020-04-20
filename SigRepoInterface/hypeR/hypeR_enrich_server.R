
observe({
  updateSelectizeInput(
    session,
    "hypeR_enrich_signature",
    choices = c(
      "",
      get_signature_names()
    )
  )
})

output$hypeR_enrich_introduction <- renderText({
  c(
    "<p><h2>Enrichment Tests</h2></p>",
    "<h3>Suggested Example Uses :</h3>",
    "<br> *Sum149_CYP1B1 and C2_CP Canonical Pathways and/or Hallmark*
     <br> run hypeR::msigdb_info() to learn more about the available species and gene sets."
  )
})

hypeR_all_features_df_variable <- reactive({
  sig <- retrieve_omicsig(input$hypeR_enrich_signature)$difexp[, c("symbol", "score")]
  direction <- rep("+", nrow(sig))
  direction[which(sig$score < 0)] <- "-"
  sig <- cbind(sig, direction)
  colnames(sig) <- c("signature_symbol", "signature_score", "signature_direction")
  return(sig)
})

output$hypeR_all_features_df <- DT::renderDataTable({
  hypeR_all_features_df_variable()
})

output$hypeR_download_all_features <- downloadHandler(
  filename = paste(as.character(input$hypeR_enrich_signature), "_all_features.tsv", sep = ""),
  content = function(file) {
    write.table(hypeR_all_features_df_variable(), file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

hypeR_enrich_result_variable <- eventReactive(input$hypeR_enrich_analysis, {

  # load genesets:
  gset_names <- data.frame(
    species = character(0),
    category = character(0),
    subcategory = character(0)
  )
  for (i in c(input$hypeR_enrich_gsets)) {
    gset_names <- rbind(gset_names, c(input$hypeR_enrich_species, unlist(strsplit(i, "_"))[1:2]), stringsAsFactors = FALSE)
  }
  colnames(gset_names) <- c("species", "category", "subcategory")

  # perform hypeR analysis:
  result <- hypeR_overrep_enrich_server_function(hypeR_all_features_df_variable(), gset_names, test = "enrich")
  result$sig_name <- input$hypeR_enrich_signature
  return(result)
})

output$hypeR_enrich_success <- renderText({
  c(
    "<h2>Sample: ",
    hypeR_enrich_result_variable()$sig_name,
    "</h2><h3>Species: ",
    input$hypeR_enrich_species,
    "</h3><h3>Gene Sets: ",
    input$hypeR_enrich_gsets, "</h3>",
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

output$hypeR_enrich_result <- DT::renderDataTable({
  if (!is.null(hypeR_enrich_result_variable()$result)) {
    hypeR_enrich_result_variable()$result
  }
})

output$hypeR_enrich_download <- downloadHandler(
  filename = paste(hypeR_enrich_result_variable()$sig_name, "_enrich.tsv", sep = ""),
  content = function(file) {
    write.table(hypeR_enrich_result_variable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
