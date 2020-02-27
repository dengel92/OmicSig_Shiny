library(plyr)

# Database related functions

# Add single quotation marks around a string
single_quoted <- function(my_string) {
    return(paste0("'", my_string, "'"))
}

# Create a link to a signature file based on the signature name
# Currently actually just does a Google image search for pandas...
create_link <-
    function(signature_name) {
        paste0('<a href="https://www.google.com/search?q=pandas&rlz=',
        '1C1CHBF_enUS807US807&sxsrf=ALeKk01pqP3J6pN-SRsmXkw_zBWDMpke_g:',
        '1582736800703&source=lnms&tbm=isch&sa=X&ved=2ahUKEwjU_pyG2u_',
        'nAhUvgnIEHU34AfMQ_AUoAXoECBEQAw&biw=1280&bih=610"',
        'target="_blank">', signature_name, '</a>')
    }

# Create a connection to the database
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

# Generic sql function
sql_generic <- function(query) {
    conn <- new_conn_handle()
    # Disconnect from database when exiting sql_generic()
    on.exit(dbDisconnect(conn), add = TRUE)
    # Query database and return results
    this_query <- dbGetQuery(conn, statement = query)
    return(this_query)
}

# Assemble part of a where clause of the form "<field> IN (<field_values>)"
# Inputs:
#   mylist: named list where the names are fields and the values are values
#       associated with those fields
#   list_key: the field name to access from mylist
in_paste <- function(mylist, list_key) {
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

# Constructs sql query based on where clause, if one is needed,
#   and executes final query as output
# Inputs:
#   fields: character vector, the fields to select from the target table
#   target_table: string; the table to select from
#   wheres: named list where the names are the fields to narrow search by and the
#       values are vectors of the values to look for in those fields
sql_finding_query <-
    function(target_table,
        fields = c("*"),
        wheres = NULL) {
        # Query construction
        sql <- paste("SELECT ",
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
        wheres <- compact(wheres)
        if (!is.null(wheres) && length(wheres) > 0) {
            # Assemble the part of each where clause of the form
            #   "<field> IN (<field_values>)"
            ins <- lapply(names(wheres), in_paste, mylist = wheres)
            # Add "WHERE" to the beginning of the where clauses and separate
            #   each "<field> IN (<field_values>)" clause by " AND "
            where_clauses <-
                paste("WHERE", paste(ins, collapse = " AND "))
        }
        # Add where clauses to query
        sql <- paste(sql, where_clauses, ";", sep = " ")
        #Debugging block
        if (FALSE) {
            print(sql)
        }
        # Execute
        return(sql_generic(sql))
    }

# Get choices for species
get_species <- reactive ({
    # Query database
    species_obj <- sql_generic("
    select
        species
    from species;")
    # Return results of query
    return(species_obj$species)
})

# Get choices for signature names
get_signature_names <- reactive ({
    # Query database
    signature_name_obj <- sql_generic("
        select
            signature_name
        from signatures;
        ")
    # Return results of query
    return(signature_name_obj$signature_name)
})


# Get choices for platforms
get_platforms <- reactive ({
    # Query database
    platform_obj <- sql_generic("
        select
            platform_name
        from assay_platforms;
        ")
    # Return results of query
    return(platform_obj$platform_name)
})