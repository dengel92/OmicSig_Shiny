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
sql_finding_query <-function(fields = c("*"),
        target_table,
        field_where = NULL,
        field_where_value = NULL,
        one_more_thing = NULL) {
        # Query construction
        sql = paste("SELECT ",
            paste(fields, collapse = ","),
            " FROM ",
            target_table,
            sep = '')
        # If there's a where clause, add where clause to main query
        sql_where = ""
        #type of values in argument
        value_type = typeof(field_where_value)
        #length of argument
        value_length = length(field_where_value)
        #think of the below like a dictionary, or a switch. 
        #I would pass the type of the argument like a key into this object to determine how
        #this argument is to be played with
        type_tovalue = list("character"=single_quoted(field_where_value),"double"=field_where_value)
        #There's a very subtle but important point to be made when dealing with 
        #multiple possible values you want to query the DB with.
        #Here, I could pass a vector of values, but the query constructed would 
        #asking for everything in one go. 
        #If you lapply instead, using the list of where values, you'll get separate queries/executions.
        #lapply approach is advised if you're doing granular checking of values in a submitted list.
        #bulk approach technically works as well, but you won't know which values resulted in
        #zero records from the DB.
        value_where = ifelse(
          value_length>1,
          paste("IN (", paste(type_tovalue[[value_type]],collapse=","),")",sep=""),
          paste("=",type_tovalue[[value_type]],sep="")
        )
        if (!is.null(field_where) && !is.null(field_where_value)) {
            sql_where = paste(
              "WHERE",
              field_where,
              value_where,
              sep=" ")
        }
        extra_wheres=""
        if(!is.null(one_more_thing)){
          extra_wheres=paste(names(one_more_thing),single_quoted(one_more_thing),sep="=",collapse=" AND ")
        }
        # Final construction of query
        sql = paste(sql, sql_where, "AND", extra_wheres,";", sep = " ")
        #Debugging block
        if(FALSE){
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