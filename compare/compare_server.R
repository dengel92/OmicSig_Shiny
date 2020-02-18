# Update signature name dropdown with list from database
#if you guys know a more generic/one shot way of updating all selectize inputs at once, 
#please let me know. Although, later on, we'll need to update the selectize inputs downstream
#to not include the name(s) of the other selectize inputs
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

#results of compare signatures.
#this part is where we need to call the compare function
#and likely change the output nature to "plot" instead of "text" here
#Tuturu~
observeEvent(
    input$compare_signatures,{
        #you'll want check functions here to make sure there aren't any empty signature name entries
        output$compare_result <- renderText(
            sql_generic(
                paste(
                    "select feature_name from feature_signature_view where signature_name =",
                    single_quoted(input$compare_1),";",
                    sep=""
                )
            )$feature_name
    )})