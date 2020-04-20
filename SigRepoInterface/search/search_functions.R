# General functions used by search page

#' General function for generating html output showing selected search terms
#'
#' @param search_id the dropdown menu id whose selected values should be
#'   displayed
#' @param display_name the word or phrase to display in the html output
#'   representing the dropdown menu field
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

#' General function for getting the values of a field using sql_finding_query()
#' 
#' @param field the name of the search field whose dropdown menu should be
#'   updated
#' @param db_table the database table to query
#' @param ins list of all of the possible in clauses for sql_finding_query()
#' @param betweens list of all of the possible between clauses for
#'   sql_finding_query()
get_field_values <- function(field, db_table, ins, betweens) {
    # Query the database
    sql_obj <-
        sql_finding_query(
            fields = field,
            target_table = db_table,
            ins = ins[names(ins) != field],
            betweens = betweens[names(betweens) != field]
        )
    # Return the list of possible values for the given field
    return(sql_obj[[field]])
}

#' General function for updating a dropdown menu
#'
#' @param field the name of the search field whose dropdown menu should be
#'   updated
#' @param db_table the database table to query
#' @param ins list of all of the possible in clauses for sql_finding_query()
#' @param betweens list of all of the possible between clauses for
#'   sql_finding_query()
update_dropdown <- function(field, db_table, ins, betweens) {
    # Query the database to find values of field that match selected values of
    #   other fields
    field_values <- get_field_values(field, db_table, ins, betweens)
    # Determine the ID for the dropdown menu that should be updated
    search_id <- paste0("search_", field)
    # Update dropdown menu
    updateSelectizeInput(session,
        search_id,
        choices = sort(c(field_values, input[[search_id]])),
        selected = input[[search_id]])
}

#' General function for clearing selections of a dropdown menu
#'
#' @param field the name of the search field whose dropdown menu should be
#'   cleared
#' @param db_table the database table to query
clear_dropdown <- function(field, db_table) {
    # Query the database to find all values of field
    field_values = get_field_values(field, db_table, NULL, NULL)
    # Determine the ID for the dropdown menu that should be updated
    search_id <- paste0("search_", field)
    # Update dropdown menu
    updateSelectizeInput(session,
        search_id,
        choices = c(field_values),
        selected = NULL)
}

#' General function for updating the values of a field in a list of in clauses
#'   based on the intersection of two lists of possible values for that field
#'   
#' @param field the field to update in the list of in clauses
#' @param db_table the database table to query to get a list of possible values
#'   for field
#' @param query_ins the list of in clauses to use in the sql query to get a list
#'   of possible values for field
#' @param ins_list the list of in clauses containing the original list of
#'   possible values for field
get_intersection <- function(field, db_table, query_ins, ins_list) {
    # Get the list of field values corresponding to query_ins
    corresponding_values <-
        get_field_values(
            field,
            db_table,
            ins = query_ins,
            betweens = NULL
        )
    # Get the list of selected field values
    selected_values <- ins_list[[field]]
    # Get the intersection of the two lists of field values
    intersect_values <-
        intersect(corresponding_values, selected_values)
    if (length(intersect_values) < 1 & length(selected_values) >= 1) {
        # If the intersection is empty and field values are selected
        #   then set the field value to an empty string for sql_finding_query()
        ins_list[[field]] = ""
    } else if (length(intersect_values) < 1 & length(selected_values) < 1) {
        # If the intersection is empty and no field values are selected
        #   then set the field value to the values corresponding to the
        #   selected ins_query for sql_finding_query()
        ins_list[[field]] = corresponding_values
    } else {
        # Otherwise set field values to the intersection for sql_finding_query()
        ins_list[[field]] = intersect_values
    }
    return(ins_list)
}