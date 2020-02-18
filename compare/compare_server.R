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
  return(sigCompare_two(sig1, sig2, is.lv2 = FALSE, background_number = input$compare_background_number))
  # names: c("Venn", "only_sig1", "only_sig2", "sig_both", "hyper_p.value")
})
output$compare_result_Venn <- renderPlot(compare_result_variable()$Venn)

output$compare_result <- renderText({
  c(
    "<font face = \"PT Sans\",font size=3>",
    "<p>The signatures you are comparing:",
    "<br><font color=\"#228822\">",
    input$compare_1,
    "(",
    length(compare_result_variable()$sig1),
    " signatures found )",
    " </font> ",
    "and <font color=\"#881199\">",
    input$compare_2,
    "(",
    length(compare_result_variable()$sig2),
    " signatures found )",
    "</font></p>",
    "<p>",
    "<p><i>The unique signatures in ",
    input$compare_1,
    " are:</i>",
    "<br>",
    {
      if (length(compare_result_variable()$only_sig1) == 0) {
        paste("*Nothing found*")
      } else {
        paste(compare_result_variable()$only_sig1)
      }
    },
    "<br><i>The unique signatures in ",
    input$compare_2,
    " are:</i>",
    "<br>",
    {
      if (length(compare_result_variable()$only_sig2) == 0) {
        paste("*Nothing found*")
      } else {
        paste(compare_result_variable()$only_sig2)
      }
    },
    "<br><i>The signatures appeared in both lists are:</i>",
    "<br>",
    {
      if (length(compare_result_variable()$sig_both) == 0) {
        paste("*Nothing found*")
      } else {
        paste(compare_result_variable()$sig_both)
      }
    },
    "</p>",
    "<p> Please see the Venn diagram below for a better visualization! </p>",
    "<p>After the Hyper Geometric test, with the background set as ",
    input$compare_background_number,
    " genes / proteins: ",
    "<br>the p-value we got is <b>",
    compare_result_variable()$hyper_p.value,
    "</b>.</p>",
    "<p><font color=\"#AB6611\"><b><i> Thank you for using! </font></b></i></p>",
    "</font>"
  )
})
# show_signatures_variable <- eventReactive(input$compare_show_signatures,{compare_result_variable()$})

output$compare_show_signatures <- renderText({
  c(
    "<p><i>All signatures in",
    input$compare_1,
    ":</i> <br><font color=\"#228822\">",
    {
      if (length(compare_result_variable()$sig1) == 0) {
        paste("*Nothing Found.* Signature is empty.")
      } else {
        paste(compare_result_variable()$sig1)
      }
    },
    "</font></p>",
    "<p><i>All signatures in",
    input$compare_2,
    ":</i> <br><font color=\"#881199\">",
    {
      if (length(compare_result_variable()$sig2) == 0) {
        paste("*Nothing Found. Signature is empty.*")
      } else {
        paste(compare_result_variable()$sig2)
      }
    },
    "</font></p>"
  )
})
