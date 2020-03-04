source("server_functions/Function_hypeR.R")

observe({
  updateSelectizeInput(
    session,
    "gsea_signature",
    choices = c(
      "",
      get_signature_names()
    )
  )
})

output$gsea_introduction <- renderText({
  c(
    "<p><h4>*Introduction of the test*</h4></p>",
    "<br> *suggest use Cal27_CB113 to test.* "
  )
})

gsea_signature_df_variable <- reactive(sql_generic(
  paste(
    "select feature_name, weight, direction from feature_signature_view where signature_name =",
    single_quoted(input$gsea_signature), ";",
    sep = ""
  )
))

output$gsea_signature_df <- renderTable({
  gsea_signature_df_variable()
})

output$gesa_download_signature <- downloadHandler(
  filename = paste(as.character(input$gsea_signature), "_signature.tsv", sep = ""),
  content = function(file) {
    write.table(gsea_signature_df_variable(), file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)

gsea_result_variable <- eventReactive(input$gsea_analysis, {
  sig <- sql_generic(
    paste(
      "select feature_name, weight, direction from feature_signature_view where signature_name =",
      single_quoted(input$gsea_signature), ";",
      sep = ""
    )
  )
  colnames(sig) <- c("signature_symbol", "signature_score", "signature_direction")

  ## species??
  ## also how is species named? need to update gsea_hypeR() function in Function_hypeR.R
  # sig_species <- sql_generic(
  #  paste(
  #    "select feature_name, weight from feature_signature_view where signature_name =",
  #    single_quoted(input$gsea_signature), ";",
  #    sep = ""
  #  ))
  ##
  result <- gsea_hypeR(sig, species = "Homo sapiens")
  result$sig_name <- input$gsea_signature
  return(result)
})

output$gsea_success <- renderText({
  c(
    "<p><i><font color=\"#008F00\"><b>Success!</b></font>",
    "Finished the GSEA analysis of", gsea_result_variable()$sig_name, ".",
    "You can see and download the result shown below.</i></p>"
  )
})


output$gsea_result <- renderTable({
  if (!is.null(gsea_result_variable()$gsea)) {
    gsea_result_variable()$gsea
  }
})

output$gsea_show_features <- renderText({
  c(
    "<p><i>All features in the signature selected:</i> <br><font color=\"#995500\">",
    if (length(gsea_result_variable()$signature) > 0) {
      paste(gsea_result_variable()$signature)
    } else {
      paste("*Signature is empty.*")
    },
    "</font></p>"
  )
})

output$gesa_download <- downloadHandler(
  filename = paste(gsea_result_variable()$sig_name, "_GSEA.tsv", sep = ""),
  content = function(file) {
    write.table(gsea_result_variable()$gsea, file, row.names = F, quote = F, col.names = T, sep = "\t")
  }
)
