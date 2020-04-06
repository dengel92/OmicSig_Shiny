#' Get choices for signature names
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


#' Get choices for platforms
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

#' Get choices for species
get_species <- reactive ({
    # Query database
    species_obj <- sql_generic("
    select
        species
    from species;")
    # Return results of query
    return(species_obj$species)
})