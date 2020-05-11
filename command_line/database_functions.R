library(RMySQL)
library(pool)
library(dplyr)
library(stringr)
library(plyr)

## Database related functions

#' Add single quotation marks around a string
#'
#' @param myString the string to add single quotation marks to
singleQuote <- function(myString) {
    return(paste0("'", myString, "'"))
}

#' Create a link to a signature file based on the signature name
#'
#' @param signatureName the name of the signature to link to
createLink <-
    function(signatureName) {
        paste0(
            '<a href="http://sigrepo.bu.edu:3838/challenge_project/',
            'miscellanea/signatures/',
            signatureName,
            '_obj.json',
            '"',
            'target="_blank">',
            signatureName,
            '</a>'
        )
    }

#' retrieveOmicSigObj()
#'
#' @title retrieve OmicSig obj from VM file system; hard coded
#'
#' @param signatureName 
#' @return OmicSignature object
#'
#' @example
#' retrieveOmicSigObj(signatureName="Cal27_BaP")
#'
retrieveOmicSigObj <-
    function(signatureName) {
        return(readJson(
            paste0(
                'http://sigrepo.bu.edu:3838/challenge_project/',
                'miscellanea/signatures/',
                signatureName,
                '_obj.json'
            )
        ))
    }

#' Create a connection to the database
newConnHandle <- function() {
    dbConnect(
        drv=RMySQL::MySQL(),
        dbname="sigrepo",
        host="sigrepo.bu.edu",
        port=4253,
        username="guest",
        password="guest"
    )
}

#' Submit a query to the database
#'
#' @param query the query to submit
sqlGeneric <- function(query) {
    ## Connect to the database
    conn <- newConnHandle()
    ## Disconnect from database when exiting sqlGeneric()
    on.exit(dbDisconnect(conn), add=TRUE)
    ## Query database and return results
    queryResult <- dbGetQuery(conn, statement=query)
    return(queryResult)
}

#' Assemble part of a where clause of the form "<field> IN (<field_values>)"
#'
#' @param myList named list where the names are fields and the values are values
#'   associated with those fields
#' @param listKey the field name to access from myList
constructInClause <- function(myList, listKey) {
    ## Construct clause
    inClause <- paste0(listKey,
        " IN (",
        paste(singleQuote(myList[[listKey]]), collapse=","),
        ")")
    
    ## If the values for this field include 'NA', then allow null values in query
    ##   "(<field> IN (<field_values>) OR <field> is null)"
    if ('NA' %in% myList[[listKey]]) {
        inClause <- paste0('(', inClause, ' OR ', listKey, ' is null)')
    }
    return(inClause)
}

#' Assemble part of a where clause of the form
#'   "<field> BETWEEN <value1> AND <value2>"
#'
#' @param myList named list where the names are fields and the values are values
#'   associated with those fields
#' @param listKey the field name to access from myList
#'
#' Note that myList[[listKey]] must have exactly two values
constructBetweenClause <- function(myList, listKey) {
    ## Make sure myList[[listKey]] has exactly two values
    if (length(myList[[listKey]]) != 2) {
        stop(paste0(
            "You tried to construct a between clause with ",
            length(myList[[listKey]]),
            " element(s)!"
        ))
    }
    ## Construct clause
    betweenClause <- paste0(listKey,
        " BETWEEN ",
        paste0(singleQuote(myList[[listKey]][1:2]), collapse=" AND "))
    return(betweenClause)
}

#' Constructs sql query based on where clause, if one is needed,
#'   and executes final query as output
#'
#' @param fields character vector, the fields to select from the target table
#' @param dbTable string; the table to select from
#' @param ins named list where the names are the fields to narrow search by
#'   and the values are vectors of the values to look for in those fields
#' @param betweens named list where the names are the fields to narrow search by
#'   and the values are vectors of length 2 containing the endpoints of the
#'   range to consider in those fields
#' @param orderFields a vector of fields to order the query results by
#' @param distinct a boolean indicating whether to return unique results
#'
#' @example
#' 
#' sqlFindingQuery("platform_signature_view", fields=c("*"),
#'     ins=list("species"=c("Homo sapiens"),
#'         "signature_name"=c("Cal27_BaP", "Cal27_PYO")),
#'     betweens=list("upload_date"=c("2020-01-01", "2020-03-01")))
#'     orderFields=c("field1",..."fieldn")
sqlFindingQuery <-
    function(dbTable,
        fields=c("*"),
        ins=NULL,
        betweens=NULL,
        orderFields=NULL,
        distinct=TRUE) {
        ## Query construction
        orderSub <- ""
        if (!is.null(orderFields)) {
            orderSub <- paste("ORDER BY", paste(orderFields, sep=","))
        }
        sql <- paste0(
            "SELECT ",
            ifelse(distinct, "DISTINCT ", ""),
            paste(fields, collapse=","),
            " FROM ",
            dbTable
        )
        ## There's a very subtle but important point to be made when dealing with
        ## multiple possible values you want to query the DB with.
        ## Here, I could pass a vector of values, but the query constructed would
        ## asking for everything in one go.
        ## If you lapply instead, using the list of where values, you'll get
        ## separate queries/executions.
        ## lapply approach is advised if you're doing granular checking of values
        ## in a submitted list.
        ## bulk approach technically works as well, but you won't know which
        ## values resulted in zero records from the DB.
        whereClauses <- ''
        ## Removes elements where the value is null
        ins <- compact(ins)
        betweens <- compact(betweens)
        if (!is.null(ins) && length(ins) > 0) {
            ## Assemble the part of each in clause of the form
            ##   "<field> IN (<field_values>)"
            ins <-
                lapply(names(ins), constructInClause, myList=ins)
        }
        if (!is.null(betweens) && length(betweens) > 0) {
            ## Assemble the part of each between clause of the form
            ##   "<field> BETWEEN <value1> AND <value2>"
            betweens <-
                lapply(names(betweens), constructBetweenClause,
                    myList=betweens)
        }
        if (!is.null(c(ins, betweens)) &&
                length(c(ins, betweens)) > 0) {
            ## Add "WHERE" to the beginning of the clauses and separate
            ##   each clause by " AND "
            whereClauses <-
                paste("WHERE", paste(c(ins, betweens), collapse=" AND "))
        }
        ## Add where clauses to query
        sql <- paste(sql, whereClauses, orderSub, ";", sep=" ")
        ## Debugging block
        if (FALSE) {
            print(sql)
        }
        ## Execute
        return(sqlGeneric(sql))
    }
