# Update signature name dropdown with list from database
# if you guys know a more generic/one shot way of updating all selectize inputs at once,
# please let me know. Although, later on, we'll need to update the selectize inputs downstream
# to not include the name(s) of the other selectize inputs
source("server_functions/Function_sigCompare.R")

observe({
  updateSelectizeInput(
    session,
    "compare_1",
    choices = c(
      "",
      get_signature_names()
    )
  )
  updateSelectizeInput(
    session,
    "compare_2",
    choices = c(
      "",
      get_signature_names()
    )
  )
})

anotb_length_check <- function(c_vector_name, empty_message="*Nothing found*"){
    result_list = empty_message
    if (length(compare_result_variable()[[c_vector_name]]) > 0) {
        result_list = paste(compare_result_variable()[[c_vector_name]],sep=", ")
    }
    return(result_list)
}


# results of compare signatures:
# Tuturu~
# Meow!
compare_result_variable <- eventReactive(input$compare_signatures, {
  sig1 <- sql_generic(
    paste(
      "select feature_name from feature_signature_view where signature_name =",
      single_quoted(input$compare_1), ";",
      sep = ""
    )
  )$feature_name
  sig2 <- sql_generic(
    paste(
      "select feature_name from feature_signature_view where signature_name =",
      single_quoted(input$compare_2), ";",
      sep = ""
    )
  )$feature_name
  return(sigCompare_two(sig1, sig2, sig1_name = input$compare_1, sig2_name = input$compare_2, is.lv2 = FALSE, background_number = input$compare_background_number))
  # names: c("Venn", "only_sig1", "only_sig2", "sig_both", "hyper_p.value")
})
output$compare_result_Venn <- renderPlot(compare_result_variable()$Venn)


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
    "<br>",
    anotb_length_check("sig_both"),
    "</h5><br/><br/>",
    "<p> Please see the Venn diagram below for a better visualization! </p><br/>",
    "<h4>Hyper Geometric test: Parameters and P-value</h4>",
    "Background: ",input$compare_background_number," features",
    "<br>P-value: <b>",
    compare_result_variable()$hyper_p.value,
    "</b>.<br/>",
    "<p id='ty_bb'><b><i> Thank you for using! </font></b></i></p>",
    "</font>"
  )
})
# show_signatures_variable <- eventReactive(input$compare_show_signatures,{compare_result_variable()$})

output$compare_show_signatures <- renderText({
  c(
    "<p><i>All signatures in",
    compare_result_variable()$sig1_name,
    ":</i> <br><font color=\"#228822\">",
    anotb_length_check("sig1_symbol",empty_message="*Nothing Found.* Signature is empty."),
    "</font></p>",
    "<p><i>All signatures in",
    compare_result_variable()$sig2_name,
    ":</i> <br><font color=\"#881199\">",
    anotb_length_check("sig2_symbol",empty_message="*Nothing Found.* Signature is empty."),
    "</font></p>"
  )
})
