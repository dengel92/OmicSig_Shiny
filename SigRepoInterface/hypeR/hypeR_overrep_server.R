#----intro & description----
output$hypeROverrepIntro <- renderText({
  c(
    "<p><h2>Over-representation Tests</h2></p>",
    "<h3>Suggested Example Uses :</h3>",
    "<br> *Sum149_CYP1B1 and C2_CP Canonical Pathways and/or Hallmark*
     <br> run hypeR::msigdb_info() to learn more about the available species and gene sets."
  )
})

output$hypeROverrepDescription <- renderText({
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

#----signature table----
observe({
  signature_list <- sql_finding_query("signatures", "signature_name")[["signature_name"]]
  updateSelectizeInput(
    session,
    "hypeROverrepSignature",
    choices = c(
      "",
      signature_list
    )
  )
})

hypeROverrepSignatureDFVariable <- reactive({
  df <- sql_generic(paste(
    "select feature_name, 
    weight, 
    direction 
    from feature_signature_view 
    where signature_name =",
    single_quoted(input$hypeROverrepSignature), ";",
    sep = ""
  ))
  df <- df[order(df$weight, decreasing = T), ]
  colnames(df) <- c("signature_symbol", "signature_score", "signature_direction")
  return(df)
})

output$hypeROverrepSignatureDF <- DT::renderDataTable({
  hypeROverrepSignatureDFVariable()
})

output$hypeROverrepDownloadSignature <- downloadHandler(
  filename = paste(as.character(input$hypeROverrepSignature), "_signature.tsv", sep = ""),
  content = function(file) {
    write.table(hypeROverrepSignatureDFVariable(), file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

#----msigDB geneset----
hypeROverrepResultVariable <- eventReactive(input$hypeROverrepAnalysis, {
  sig <- hypeROverrepSignatureDFVariable()
  gsetNames <- NULL
  gsetNames <- data.frame(
    species = character(0),
    category = character(0),
    subcategory = character(0)
  )
  for (i in c(input$hypeROverrepGsets)) {
    gsetNames <- rbind(gsetNames, c(input$hypeROverrepSpecies, unlist(strsplit(i, "_"))[1:2]), stringsAsFactors = FALSE)
  }
  colnames(gsetNames) <- c("species", "category", "subcategory")
  result <- hypeROverrepEnrichServerFunction(sig, gsetNames = gsetNames, gsetList = NULL, test = "overrep")
  result$sigName <- input$hypeROverrepSignature
  return(result)
})

output$hypeROverrepSuccess <- renderText({
  c(
    "<h2>Signature: ",
    hypeROverrepResultVariable()$gsetNames,
    "</h2><h3>Species: ",
    input$hypeROverrepSpecies,
    "</h3><h3>Gene Sets: ",
    input$hypeROverrepGsets, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> Result is shown with fdr cutoff 0.05. It's possible to have no data available if no significantly enriched gene set is found. - at Apr 2020. </p>"
  )
})

output$hypeROverrepResult <- DT::renderDataTable({
  if (!is.null(hypeROverrepResultVariable()$result)) {
    hypeROverrepResultVariable()$result
  }
})

output$hypeROverrepResultDownload <- downloadHandler(
  filename = paste(hypeROverrepResultVariable()$sigName, "_overrep.tsv", sep = ""),
  content = function(file) {
    write.table(hypeROverrepResultVariable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

#----customized upload geneset----
hypeROverrepCustomGsetResultVariable <- eventReactive(input$hypeROverrepCustomGsetAnalysis, {
  sig <- hypeROverrepSignatureDFVariable()
  gsetList <- readGsetList(input$hypeROverrepCustomGset$datapath)
  result <- hypeROverrepEnrichServerFunction(sig, gsetNames = NULL, gsetList = gsetList, gsetListCategory = as.character(input$hypeROverrepCustomGsetName), gsetListSubCategory = "/", test = "overrep")
  result$sigName <- input$hypeROverrepSignature
  return(result)
})

output$hypeROverrepCustomGsetSuccess <- renderText({
  c(
    "<h2>Signature: ",
    hypeROverrepCustomGsetResultVariable()$sigName,
    "</h2><h3>Gene Sets: ",
    input$hypeROverrepCustomGsetName, "</h3>",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#BF4422\"><b>NOTICE</b></font> Result is shown with fdr cutoff 0.05. It's possible to have no data available if no significantly enriched gene set is found. - at Apr 2020. </p>"
  )
})

output$hypeROverrepCustomGsetResult <- DT::renderDataTable({
  if (!is.null(hypeROverrepCustomGsetResultVariable()$result)) {
    hypeROverrepCustomGsetResultVariable()$result
  }
})

output$hypeROverrepCustomGsetResultDownload <- downloadHandler(
  filename = paste(hypeROverrepCustomGsetResultVariable()$sigName, input$hypeROverrepCustomGsetName, "_overrep.tsv", sep = ""),
  content = function(file) {
    write.table(hypeROverrepCustomGsetResultVariable()$result, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
