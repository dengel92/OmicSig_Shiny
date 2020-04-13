library(RMySQL)
library(pool)
library(dplyr)
library(stringr)

in_studio<-FALSE
#finds out where this file is living when running on cli
#so you can execute this file where in your file system.
#granted, you need to have database_functions in the same directory as 
#this file
if(!in_studio){
    initial.options <- commandArgs(trailingOnly = FALSE)
    file.arg.name <- "--file="
    script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
    script.dirname <- dirname(script.name)
    setwd(script.dirname)
}
source("database_functions.R")
source("../SigRepoInterface/server_functions/Function_sigCompare.R")
#' Really nice way to construct name-value pairs when calling 
#' Rscript to execute this file(cli) to pass easily to downstream functions.
#' parameters with multiple values declared will have those values
#' concatenated together in a vector
#' 
#' @param arglist character vector, the trailing arguments to CLI call
#' @param name_tag string, what you want to tag your parameter names with
name_to_val <- function(arglist, name_tag="--"){
    #getting where parameter names are in list
    hm = lapply(
            arglist, 
            function(a){
                grep(name_tag, a)
            }
        )
    arg_names_pos = which(hm==1)
    #getting these parameter names, along with getting 
    #lengths for downstream operations
    arg_names = str_replace(arglist[arg_names_pos],name_tag,"")
    arglist[arg_names_pos] = arg_names
    num_names = length(arg_names_pos) 
    num_args = length(arglist) 
    # check for empty values for a typed in parameter
    # Basically, if you type in "....... --param1 --param2"
    # The function will tell you that nothing corresponding to param1
    # was declared in the system call
    if(any(diff(arg_names_pos)==1) || is.na(arglist[arg_names_pos[num_names]+1])){
        stop("missing value(s) for some parameter(s)")
    }
    # otherwise, keep going, and group values by names
    arg_name_groups = list()
    name_list = lapply(
        1:num_names, function(i){
            # If I'm not at the last parameter name yet... get the values
            if(arg_names[i]!=arg_names[num_names]){
                arg_name_groups[[ arg_names[i] ]] = arglist[(arg_names_pos[i] + 1) : (arg_names_pos[i+1]-1)]
            }
            # Same thing for being the last parameter name, except this block accounts for
            # 
            else{
                arg_name_groups[[arg_names[i]]] = arglist[(arg_names_pos[i]+1) : length(arglist)]
            }
        }
    )
    names(name_list) = arg_names
    return(name_list)
}

if(length(commandArgs(trailingOnly = TRUE))>0){
    user_inputs <- commandArgs(trailingOnly=TRUE)
    input_groups <- name_to_val(user_inputs)
    #expects certain names for parameters
    switch(
        input_groups$f,
        "find" = {
            sql_finding_query(
                target_table=input_groups$table,
                fields=input_groups$fields
            )
        },
        "show_fields" = {
            sql_generic(paste("SHOW COLUMNS FROM", input_groups$table, ";"))
        },
        "show_tables" = {
            sql_generic(paste("SHOW TABLES;"))
        },
        "compare" = {
            compare_signatures(input_groups$signatures[1], input_groups$signatures[2])
        }
    )
}