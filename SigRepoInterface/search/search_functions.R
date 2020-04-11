# General functions used by search page

#' General function for generating html output showing selected search terms
#'
#' @param search_id the dropdown menu id whose selected values should be displayed
#' @param display_name the word or phrase to display in the html output representing
#'   the dropdown menu field
selected_html <- function(search_id, display_name) {
    # Display "no <display_name> selected" if there is no input to this dropdown
    if (length(input[[search_id]]) < 1) {
        paste0("no ", display_name, " selected</br>")
    } else {
        # Otherwise display "<display_name>: <value1>, <value2>, ..."
        paste0(display_name,
            ": <b>",
            paste(input[[search_id]], collapse = ", "),
            "</b></br>")
    }
}

#' General function for updating a dropdown menu
#'
#' @param field the name of the search field whose dropdown menu should be updated
#' @param ins list of all of the possible in clauses for sql_finding_query()
#' @param betweens list of all of the possible between clauses for
#'   sql_finding_query()
update_dropdown = function(field, ins, betweens) {
    # Query the database to find values of field that match selected values of
    #   other fields
    sql_obj <-
        sql_finding_query(
            fields = field,
            target_table = "platform_signature_view",
            ins = ins[names(ins) != field],
            betweens = betweens[names(betweens) != field]
        )
    # Determine the ID for the dropdown menu that should be updated
    search_id <- paste0("search_", field)
    # Update dropdown menu
    updateSelectizeInput(session,
        search_id,
        choices = c(sql_obj[[field]], input[[search_id]]),
        selected = input[[search_id]])
}

#' General function for clearing selections of a dropdown menu
#'
#' @param field the name of the search field whose dropdown menu should be cleared
clear_dropdown = function(field) {
    # Query the database to find all values of field
    sql_obj <-
        sql_finding_query(fields = field,
            target_table = "platform_signature_view")
    # Determine the ID for the dropdown menu that should be updated
    search_id <- paste0("search_", field)
    # Update dropdown menu
    updateSelectizeInput(session,
        search_id,
        choices = c(sql_obj[[field]]),
        selected = NULL)
}