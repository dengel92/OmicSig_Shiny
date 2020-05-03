#----intro & description----
output$hypeREnrichIntro <- renderText({
  c(
    "<p><h2>Enrichment Tests</h2></p>",
    "<h3>Suggested Example Uses :</h3>",
    "<br> *Sum149_CYP1B1 and C2_CP Canonical Pathways and/or Hallmark*
        <br> run hypeR::msigdb_info() to learn more about the available species and gene sets."
  )
})

output$hypeREnrichDescription <- renderText({
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
  signature_list <- sql_finding_query("signatures", "signature_name")[["signature_name"]]
  updateSelectizeInput(
    session,
    "hypeREnrichSignature",
    choices = c(
      "",
      signature_list
    )
  )
})

hypeREnrichAllFeaturesDFVariable <- reactive({
  sig <- retrieveOmicSigObj(signatureName = input$hypeREnrichSignature)$difexp[, c("symbol", "score")]
  direction <- rep("+", nrow(sig))
  direction[which(sig$score < 0)] <- "-"
  sig <- cbind(sig, direction)
  colnames(sig) <- c("signature_symbol", "signature_score", "signature_direction")
  return(sig)
})

output$hypeREnrichAllFeaturesDF <- DT::renderDataTable({
  hypeREnrichAllFeaturesDFVariable()
})

output$hypeREnrichDownloadAllFeatures <- downloadHandler(
  filename = paste(as.character(input$hypeREnrichSignature), "_all_features.tsv", sep = ""),
  content = function(file) {
    write.table(hypeREnrichAllFeaturesDFVariable(), file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

#----msigDB geneset----
hypeREnrichResultVariable <- eventReactive(input$hypeREnrichAnalysis, {
  sig <- hypeREnrichAllFeaturesDFVariable()
  gsetNames <- NULL
  gsetNames <- data.frame(
    species = character(0),
    category = character(0),
    subcategory = character(0)
  )
  for (i in c(input$hypeREnrichGsets)) {
    gsetNames <- rbind(gsetNames, c(input$hypeREnrichSpecies, unlist(strsplit(i, "_"))[1:2]), stringsAsFactors = FALSE)
  }
  colnames(gsetNames) <- c("species", "category", "subcategory")
  result <- hypeROverrepEnrichServerFunction(sig, gsetNames = gsetNames, gsetList = NULL, test = "enrich")
  result$sigName <- input$hypeREnrichSignature
  return(result)
})

output$hypeREnrichSuccess <- renderText({
  c(
    "<h2>Signature: ",
    hypeREnrichResultVariable()$sigName,
    "</h2><h3>Species: ",
    input$hypeREnrichSpecies,
    "</h3><h3>Gene Sets: ",
    input$hypeREnrichGsets, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> Result is shown with fdr cutoff 0.05. It's possible to have no data available if no significantly enriched gene set is found. - at Apr 2020. </p>"
  )
})

output$hypeREnrichResult <- DT::renderDataTable({
  if (!is.null(hypeREnrichResultVariable()$result)) {
    hypeREnrichResultVariable()$result
  }
})

output$hypeREnrichResultDownload <- downloadHandler(
  filename = paste(hypeREnrichResultVariable()$sigName, "_enrich.tsv", sep = ""),
  content = function(file) {
    write.table(hypeREnrichResultVariable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

#----customized gset----
hypeREnrichCustomGsetResultVariable <- eventReactive(input$hypeREnrichCustomGsetAnalysis, {
  sig <- hypeREnrichAllFeaturesDFVariable()
  gsetList <- readGsetList(input$hypeREnrichCustomGset$datapath)
  result <- hypeROverrepEnrichServerFunction(sig, gsetNames = NULL, gsetList = gsetList, gsetListCategory = as.character(input$hypeREnrichCustomGsetName), gsetListSubCategory = "/", test = "enrich")
  result$sigName <- input$hypeREnrichSignature
  return(result)
})

output$hypeREnrichCustomGsetSuccess <- renderText({
  c(
    "<h2>Signature: ",
    hypeREnrichCustomGsetResultVariable()$sigName,
    "</h2><h3>Gene Sets: ",
    input$hypeREnrichCustomGsetName, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> Result is shown with fdr cutoff 0.05. It's possible to have no data available if no significantly enriched gene set is found. - at Apr 2020. </p>"
  )
})

output$hypeREnrichCustomGsetResult <- DT::renderDataTable({
  if (!is.null(hypeREnrichCustomGsetResultVariable()$result)) {
    hypeREnrichCustomGsetResultVariable()$result
  }
})

output$hypeREnrichResultDownload <- downloadHandler(
  filename = paste(hypeREnrichCustomGsetResultVariable()$sigName, input$hypeREnrichCustomGsetName, "_enrich.tsv", sep = ""),
  content = function(file) {
    write.table(hypeREnrichCustomGsetResultVariable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
