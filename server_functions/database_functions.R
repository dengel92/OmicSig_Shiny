# Database related functions

# Add single quotation marks around a string
single_quoted <- function(my_string){
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