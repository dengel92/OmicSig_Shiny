# Database related functions

# Add single quotation marks around a string
single_quoted <- function(my_string) {
    return(paste0("'", my_string, "'"))
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

pasty_boi <- function(mylist,list_key){
    paste(list_key, " IN (",paste(single_quoted(mylist[[list_key]]),collapse=","),")",sep="")
}

# Constructs sql query based on where clause, if one is needed,
#   and executes final query as output
# Inputs:
#   fields: character vector, the fields to select from the target table
#   target_table: string; the table to select from
#   wheres: named list where the name is the field to narrow search by and the
#       value is a vector of the values to look for in that field
sql_finding_query <- function(fields = c("*"), target_table, wheres = NULL) {
        # Query construction
        sql <- paste("SELECT ", paste(fields, collapse = ","),
            " FROM ", target_table, sep = '')
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
        if (!is.null(wheres) && length(wheres) > 0) {
            hm = lapply(names(wheres), pasty_boi, mylist=wheres) 
            where_clauses = paste("WHERE",paste(hm,collapse=" AND "))
        }
        # Add where clauses to query
        sql <- paste(sql, where_clauses,";", sep = " ")
        #Debugging block
        if(TRUE){
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

# Get signatures
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


# Get choices for platform
get_platforms <- reactive ({
    # Query database
    platform_obj <- sql_generic("
        select
            platform_name
        from platform_signature_view;
        ")
    # Return results of query
    return(platform_obj$platform_name)
})