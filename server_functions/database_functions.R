# Database related functions

# Add single quotation marks around a string
single_quoted <- function(my_string) {
    return(paste("'", my_string, "'", sep = ""))
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
    conn = new_conn_handle()
    # Disconnect from database when exiting sql_generic()
    on.exit(dbDisconnect(conn), add = TRUE)
    this_query =
        dbGetQuery(conn,
            statement = query)
    return(this_query)
}

# Constructs sql query based on where clause, if one is needed,
#   and executes final query as output
# Inputs:
# fields: character vector, the fields to select from the target table
# target_table: string; the table to select from
# field_where: string; field you want to narrow search by
#   NOTE: multi-where not implemented here yet
# field_where_value: whatever type is in field_where of target_table,
#   usually you will lapply some list of values with this function,
#   where the list is field_where_value
sql_finding_query <-
    function(fields = c("*"),
        target_table,
        field_where = NULL,
        field_where_value = NULL) {
        # Query construction
        sql = paste("SELECT ",
            paste(fields, collapse = ","),
            " FROM ",
            target_table,
            sep = '')
        # If there's a where clause, add where clause to main query
        sql_where = ""
        if (!is.null(field_where) && !is.null(field_where_value)) {
            sql_where = paste(" WHERE ",
                field_where,
                "=",
                switch(
                    typeof(field_where_value),
                    # If you're querying based on string value,
                    #   you must put the values in single quotes
                    #   so as not to mess up the final string
                    "character" = paste(single_quoted(field_where_value),
                        sep = ""),
                    field_where_value
                ), sep = "")
        }
        # Final construction of query
        sql = paste(sql, sql_where, ";", sep = "")
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

# Get choices for platform
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

# 'select * from platform_signature_view'