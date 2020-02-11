# Database related functions

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
sql_generic <- function(query){
    conn = new_conn_handle()
    # Disconnect from database when exiting sql_generic()
    on.exit(dbDisconnect(conn), add = TRUE)
    this_query =
        dbGetQuery(conn,
            statement = query)
    return(this_query)
}

# Get choices for species
get_species <- reactive ({
    # Query database
    species_obj <- sql_generic("
    select
        concat(species, '[', taxonomic_id, ']')
        as species
    from species;")
    # Return results of query
    return(species_obj$species)
})

sql_finding_query <- function(fields="*", target_table, field_where=NULL, field_where_value=NULL){
  sql = paste("SELECT " , paste(fields,collapse=","), " FROM ", target_table, sep='')
  sql_where = ""
  if(!is.null(field_where) && !is.null(field_where_value)){
    sql_where = paste(
      " WHERE ", 
      field_where, 
      "=",
      switch(typeof(field_where_value),
             "character"=paste( single_quoted(field_where_value),sep=""),
             field_where_value
      ),sep=""
    )
  }
  sql = paste(sql, sql_where, ";", sep="")
  return(sql_generic(sql))
}

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