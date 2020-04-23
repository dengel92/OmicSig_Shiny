library(RMySQL)
library(pool)
library(dplyr)
library(stringr)
library(plyr)

# Database related functions

#' Add single quotation marks around a string
#'
#' @param my_string the string to add single quotation marks to
single_quoted <- function(my_string) {
    return(paste0("'", my_string, "'"))
}

#' Create a link to a signature file based on the signature name
#'
#' @param signature_name the name of the signature to link to
create_link <-
    function(signature_name) {
        paste0(
            '<a href="http://sigrepo.bu.edu:3838/challenge_project/',
            'miscellanea/signatures/',
            signature_name,
            '_obj.json',
            '"',
            'target="_blank">',
            signature_name,
            '</a>'
        )
    }

## retrieve_omicsig()
##
#' @title retrieve OmicSig obj from VM file system; hard coded
#'
#' @param signature_name 
#' @return OmicSignature object
#'
#' @example
#' retrieve_omicsig(signature_name = "Cal27_BaP")
#'
retrieve_omicsig <-
    function(signature_name) {
        return(read_json(paste0(
            'http://sigrepo.bu.edu:3838/challenge_project/',
            'miscellanea/signatures/',
            signature_name,
            '_obj.json'
        )))
    }

#' Create a connection to the database
new_conn_handle <- function() {
    dbConnect(
        drv = RMySQL::MySQL(),
        dbname = "sigrepo",
        host = "sigrepo.bu.edu",
        port = 4253,
        username = "guest",
        password = "guest"
    )
}

#' Submit a query to the database
#'
#' @param query the query to submit
sql_generic <- function(query) {
    # Connect to the database
    conn <- new_conn_handle()
    # Disconnect from database when exiting sql_generic()
    on.exit(dbDisconnect(conn), add = TRUE)
    # Query database and return results
    this_query <- dbGetQuery(conn, statement = query)
    return(this_query)
}

#' Assemble part of a where clause of the form "<field> IN (<field_values>)"
#'
#' @param mylist named list where the names are fields and the values are values
#'   associated with those fields
#' @param list_key the field name to access from mylist
construct_in_clause <- function(mylist, list_key) {
    # Construct clause
    in_clause <- paste0(list_key,
        " IN (",
        paste(single_quoted(mylist[[list_key]]), collapse = ","),
        ")")
    # If the values for this field include 'NA', then allow null values in query
    #   "(<field> IN (<field_values>) OR <field> is null)"
    if ('NA' %in% mylist[[list_key]]) {
        in_clause <- paste0('(', in_clause, ' OR ', list_key, ' is null)')
    }
    return(in_clause)
}

#' Assemble part of a where clause of the form
#'   "<field> BETWEEN <value1> AND <value2>"
#'
#' @param mylist named list where the names are fields and the values are values
#'   associated with those fields
#' @param list_key the field name to access from mylist
#'
#' Note that mylist[[list_key]] must have exactly two values
construct_between_clause <- function(mylist, list_key) {
    # Make sure mylist[[list_key]] has exactly two values
    if (length(mylist[[list_key]]) != 2) {
        stop(paste0(
            "You tried to construct a between clause with ",
            length(mylist[[list_key]]),
            " element(s)!"
        ))
    }
    # Construct clause
    between_clause <- paste0(list_key,
        " BETWEEN ",
        paste0(single_quoted(mylist[[list_key]][1:2]), collapse = " AND "))
    return(between_clause)
}

#' Constructs sql query based on where clause, if one is needed,
#'   and executes final query as output
#'
#' @param fields character vector, the fields to select from the target table
#' @param target_table string; the table to select from
#' @param ins named list where the names are the fields to narrow search by
#'   and the values are vectors of the values to look for in those fields
#' @param betweens named list where the names are the fields to narrow search by
#'   and the values are vectors of length 2 containing the endpoints of the
#'   range to consider in those fields
#' @param order_fields a vector of fields to order the query results by
#' @param distinct a boolean indicating whether to return unique results
#'
#' @example
#' 
#' sql_finding_query("platform_signature_view", fields = c("*"),
#'     ins = list("species" = c("Homo sapiens"),
#'         "signature_name" = c("Cal27_BaP", "Cal27_PYO")),
#'     betweens = list("upload_date" = c("2020-01-01", "2020-03-01")))
#'     order_fields = c("field1",..."fieldn")
sql_finding_query <-
    function(target_table,
        fields = c("*"),
        ins = NULL,
        betweens = NULL,
        order_fields = NULL,
        distinct = TRUE) {
        # Query construction
        order_sub = ""
        if(!is.null(order_fields)){
            order_sub = paste("ORDER BY", paste(order_fields, sep=","))
        }
        sql <- paste("SELECT ",
            ifelse(distinct, "DISTINCT ", ""),
            paste(fields, collapse = ","),
            " FROM ",
            target_table,
            sep = '')
        # There's a very subtle but important point to be made when dealing with
        # multiple possible values you want to query the DB with.
        # Here, I could pass a vector of values, but the query constructed would
        # asking for everything in one go.
        # If you lapply instead, using the list of where values, you'll get
        # separate queries/executions.
        # lapply approach is advised if you're doing granular checking of values
        # in a submitted list.
        # bulk approach technically works as well, but you won't know which
        # values resulted in zero records from the DB.
        where_clauses = ''
        # Removes elements where the value is null
        ins <- compact(ins)
        betweens <- compact(betweens)
        if (!is.null(ins) && length(ins) > 0) {
            # Assemble the part of each in clause of the form
            #   "<field> IN (<field_values>)"
            ins <-
                lapply(names(ins), construct_in_clause, mylist = ins)
        }
        if (!is.null(betweens) && length(betweens) > 0) {
            # Assemble the part of each between clause of the form
            #   "<field> BETWEEN <value1> AND <value2>"
            betweens <-
                lapply(names(betweens), construct_between_clause,
                    mylist = betweens)
        }
        if (!is.null(c(ins, betweens)) &&
                length(c(ins, betweens)) > 0) {
            # Add "WHERE" to the beginning of the clauses and separate
            #   each clause by " AND "
            where_clauses <-
                paste("WHERE", paste(c(ins, betweens), collapse = " AND "))
        }
        # Add where clauses to query
        sql <- paste(sql, where_clauses, order_sub, ";", sep = " ")
        #Debugging block
        if (FALSE) {
            print(sql)
        }
        # Execute
        return(sql_generic(sql))
    }
