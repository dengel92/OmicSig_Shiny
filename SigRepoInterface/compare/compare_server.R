# Update signature name dropdown with list from database
# if you guys know a more generic/one shot way of updating all selectize inputs at once,
# please let me know. Although, later on, we'll need to update the selectize inputs downstream
# to not include the name(s) of the other selectize inputs

observe({
  signature_list <- sql_finding_query("signatures", "signature_name")[["signature_name"]]
  updateSelectizeInput(
    session,
    "compare_1",
    choices = c(
      "",
      signature_list
    )
  )
  updateSelectizeInput(
    session,
    "compare_2",
    choices = c(
      "",
      signature_list
    )
  )
})

# function used to check if any input signatures / outputs is empty
# only applicable for this tab
anotb_length_check <- function(c_vector_name, empty_message = "*Nothing found*") {
  result_list <- empty_message
  if (length(compare_result_variable()[[c_vector_name]]) > 0) {
    result_list <- paste(compare_result_variable()[[c_vector_name]], sep = ", ")
  }
  return(result_list)
}



# results of compare signatures:
# Tuturu~ Meow!
compare_result_variable <- eventReactive(input$compare_signatures, {
    return(compare_signatures(input$compare_1, input$compare_2, input$compare_background_number))
})

# output Venn diagram:
output$compare_result_Venn <- renderPlot(compare_result_variable()$Venn)

# output the unique, common features, hyper-geometric test resules:
output$compare_result <- renderText({
  c(
    "<h3 class='result_header'>The signatures compared</h3>",
    "<h3 id='first_sig_compare_header'>",
    compare_result_variable()$sig1_name,
    "(",
    length(compare_result_variable()$sig1_symbol),
    " features found )",
    "<h3 id='second_sig_compare_header'>",
    compare_result_variable()$sig2_name,
    "(",
    length(compare_result_variable()$sig2_symbol),
    " features found )",
    "</h3>",
    "<p>",
    "<h5><i>Unique features in ",
    compare_result_variable()$sig1_name,
    ":</i></h5>",
    anotb_length_check("only_sig1"),
    "<br/><br/><h5><i>Unique features in ",
    compare_result_variable()$sig2_name,
    ":</i></h5>",
    anotb_length_check("only_sig2"),
    "<br/><br/><h5><i>Shared features:</i>",
    "<br><br>",
    anotb_length_check("sig_both"),
    "</h5><br/><br/>",
    "<p> Please see the Venn diagram below for a better visualization! </p><br/>",
    "<h4>Hyper Geometric test: Parameters and P-value</h4>",
    "Background: ", input$compare_background_number, " features",
    "<br>P-value: <b>",
    compare_result_variable()$hyper_p.value,
    "</b>.<br/>",
    "<h4>Wilcox rank test for signature with score, if applicable:</h4>",
    "P-value: <b>",
    compare_result_variable()$rank_p.value,
    "</b>.<br><br>",
    "<p id='ty_bb'><b><i> Thank you for using! </font></b></i></p>",
    "</font>"
  )
})



# print the actual features in the two signatures for reference:
output$compare_show_signatures <- renderText({
  c(
    "<p><i>All features in",
    compare_result_variable()$sig1_name,
    ":</i> <br><font color=\"#228822\">",
    anotb_length_check("sig1_symbol", empty_message = "*Nothing Found.* Signature is empty."),
    "</font></p>",
    "<p><i>All features in",
    compare_result_variable()$sig2_name,
    ":</i> <br><font color=\"#881199\">",
    anotb_length_check("sig2_symbol", empty_message = "*Nothing Found.* Signature is empty."),
    "</font></p>"
  )
})



# print the actual features in the two signatures for reference:
output$compare_show_signatures <- renderText({
  c(
    "<p><i>All features in",
    compare_result_variable()$sig1_name,
    ":</i> <br><font color=\"#228822\">",
    anotb_length_check("sig1_symbol", empty_message = "*Nothing Found.* Signature is empty."),
    "</font></p>",
    "<p><i>All features in",
    compare_result_variable()$sig2_name,
    ":</i> <br><font color=\"#881199\">",
    anotb_length_check("sig2_symbol", empty_message = "*Nothing Found.* Signature is empty."),
    "</font></p>"
  )
})
