# 03/05: minor update required:
# modify columns better: signature number, background, put direction earlier
# idea: put up and down in seperate tables?
# idea: add link to the result of the genesets?

# future: add GSEA test (probably in a seperate tab) to perform enrichment analysis using lv1

source("server_functions/Function_hypeR.R")

observe({
  updateSelectizeInput(
    session,
    "overrep_signature",
    choices = c(
      "",
      get_signature_names()
    )
  )
})

output$overrep_introduction <- renderText({
  c(
    "<p><h4>*Introduction of the test*</h4></p>",
    "<br> *suggest use Cal27_CB113 to test.* "
  )
})

overrep_signature_df_variable <- reactive({
  df <- sql_generic(paste(
    "select feature_name, weight, direction from feature_signature_view where signature_name =",
    single_quoted(input$overrep_signature), ";",
    sep = ""
  ))
  df <- df[order(df$weight, decreasing = T),]
  colnames(df) <- c("signature_symbol", "signature_score", "signature_direction")
  return(df)
})

output$overrep_signature_df <- renderTable({
  overrep_signature_df_variable()
})

output$overrep_download_signature <- downloadHandler(
  filename = paste(as.character(input$overrep_signature), "_signature.tsv", sep = ""),
  content = function(file) {
    write.table(overrep_signature_df_variable(), file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

overrep_result_variable <- eventReactive(input$overrep_analysis, {
  sig <- overrep_signature_df_variable()

  ## species??
  ## also how is species named? need to update overrep_hypeR() function in Function_hypeR.R
  # sig_species <- sql_generic(
  #  paste(
  #    "select feature_name, weight from feature_signature_view where signature_name =",
  #    single_quoted(input$overrep_signature), ";",
  #    sep = ""
  #  ))
  ##
  result <- hypeR_overrep_function(sig, species = "Homo sapiens")
  result$sig_name <- input$overrep_signature
  return(result)
})

output$overrep_success <- renderText({
  c(
    "<p><i><font color=\"#008F00\"><b>Success!</b></font>",
    "Finished the Over-representation analysis result of", overrep_result_variable()$sig_name, ".",
    "You can see and download the result shown below.</i></p>",
    "<p><font color=\"#008F00\"><b>NOTICE</b></font> at this moment, the p-value cutoff of the analysis is very non-stringent, so it's probably outputing all the genesets that are found to have any overlap with the given signature list. - at Mar2020. </p>",
    "<p>The meaning of each column:",
      "<br> direction: the direction of the feature (genes) in the given signature, if available.",
      "<br> label: pathway or geneset names.",
      "<br> pval: p-value of the over-representation of that pathway or geneset.",
      "<br> fdr: p-value after fdr correction.",
      "<br> geneset: total amount of genes in that pathway or geneset.",
      "<br> overlap: the number of significant overlapping genes between the signature and the geneset.", 
      "<br> hits: symbol of the significant overlapping genes.",
    "</p>"
  )
})


output$overrep_result <- renderTable({
  if (!is.null(overrep_result_variable()$overrep)) {
    overrep_result_variable()$overrep
  }
})

output$overrep_show_features <- renderText({
  c(
    "<p><i>All features in the signature selected:</i> <br><font color=\"#995500\">",
    if (length(overrep_result_variable()$signature) > 0) {
      paste(overrep_result_variable()$signature)
    } else {
      paste("*Signature is empty.*")
    },
    "</font></p>"
  )
})

output$overrep_download <- downloadHandler(
  filename = paste(overrep_result_variable()$sig_name, "_overrep.tsv", sep = ""),
  content = function(file) {
    write.table(overrep_result_variable()$overrep, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
