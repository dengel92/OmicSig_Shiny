#----intro & description----
output$hypeR_enrich_introduction <- renderText({
  c(
    "<p><h2>Enrichment Tests</h2></p>",
    "<h3>Suggested Example Uses :</h3>",
    "<br> *Sum149_CYP1B1 and C2_CP Canonical Pathways and/or Hallmark*
        <br> run hypeR::msigdb_info() to learn more about the available species and gene sets."
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

#----signature table----
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

#----msigDB geneset----
hypeR_enrich_result_variable <- eventReactive(input$hypeR_enrich_analysis, {
  sig <- hypeR_all_features_df_variable()
  gset_names <- NULL
  gset_names <- data.frame(
    species = character(0),
    category = character(0),
    subcategory = character(0)
  )
  for (i in c(input$hypeR_enrich_gsets)) {
    gset_names <- rbind(gset_names, c(input$hypeR_enrich_species, unlist(strsplit(i, "_"))[1:2]), stringsAsFactors = FALSE)
  }
  colnames(gset_names) <- c("species", "category", "subcategory")
  result <- hypeR_overrep_enrich_server_function(sig, gset_names = gset_names, gset_list = NULL, test = "enrich")
  result$sig_name <- input$hypeR_enrich_signature
  return(result)
})

output$hypeR_enrich_success <- renderText({
  c(
    "<h2>Signature: ",
    hypeR_enrich_result_variable()$sig_name,
    "</h2><h3>Species: ",
    input$hypeR_enrich_species,
    "</h3><h3>Gene Sets: ",
    input$hypeR_enrich_gsets, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> Result is shown with fdr cutoff 0.05. It's possible to have no data available if no significantly enriched gene set is found. - at Apr 2020. </p>"
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

#----customized gset----
hypeR_enrich_cust_result_variable <- eventReactive(input$hypeR_enrich_cust_analysis, {
  sig <- hypeR_all_features_df_variable()
  gset_list <- read_gset_list(input$hypeR_enrich_cust_gset$datapath)
  result <- hypeR_overrep_enrich_server_function(sig, gset_names = NULL, gset_list = gset_list, gset_list_category = as.character(input$hypeR_enrich_cust_gset_name), gset_list_subcategory = "/", test = "enrich")
  result$sig_name <- input$hypeR_enrich_signature
  return(result)
})

output$hypeR_enrich_cust_success <- renderText({
  c(
    "<h2>Signature: ",
    hypeR_enrich_cust_result_variable()$sig_name,
    "</h2><h3>Gene Sets: ",
    input$hypeR_enrich_cust_gset_name, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> Result is shown with fdr cutoff 0.05. It's possible to have no data available if no significantly enriched gene set is found. - at Apr 2020. </p>"
  )
})

output$hypeR_enrich_cust_result <- DT::renderDataTable({
  if (!is.null(hypeR_enrich_cust_result_variable()$result)) {
    hypeR_enrich_cust_result_variable()$result
  }
})

output$hypeR_enrich_download <- downloadHandler(
  filename = paste(hypeR_enrich_cust_result_variable()$sig_name, input$hypeR_enrich_cust_gset_name, "_enrich.tsv", sep = ""),
  content = function(file) {
    write.table(hypeR_enrich_cust_result_variable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
