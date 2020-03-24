# Server logic for upload page

upload_root_path <- "/srv/shiny-server/signatures/"
source('../OmicSignature/check_functions/Function_json.R')
source('../OmicSignature/check_functions/Function_objCheck.R')
source('../OmicSignature/check_functions/Function_write_sig.R')
source('../OmicSignature/OmicSignature.R')

# QC of rds file to ensure it's not corrupted and in the right format(later)
checkRDS <- function(rds_file,input_name){
    # Observe+trycatch meant to capture error and display in UI,
    # Rather than crashing the app when an error happens.
    observe({
        tryCatch(
            readRDS(rds_file),
            error = function(e) {
                shinyalert("improper file. Make sure your file is RDS and not corrupted.")
            }
        )
    })
}

#input : file input object, target directory 
#output : copied file
copy_file <- function(input_file, destination){
    #check if input is empty or not. if empty, do nothing.
    if(input_file!=""){
        file.copy(input_file,destination)
    }
}


##=============================================================##
##------------------Adding Signatures--------------------------##
##=============================================================##


# writing file to directory based on level
# If directory nonexistent, makes one
# inputs:
# file_object: input object of type 'file'(app)
# sig_name: string denoting name of signature. file names don't always line up with 
#   their corresponding signature name/user may want it to be named something else but either
#   forgot to or didn't feel like changing the file name accordingly
write_signature_file <- function(file_object, sig_name){
    sig_dir = paste( upload_root_path, sig_name, sep="")
    print(sig_dir)
    if(!dir.exists(sig_dir)){
        dir.create(sig_dir)
    }
    copy_file(file_object[['datapath']], sig_dir)
}


# finds features from OmicSig object that are not in the DB
# to be passed to a synonym checker function
# Input: OmicObj formatted object. consult OmicObj documentation
odd_ones_out <- function(your_obj){
    #fetching feature ids corresponding to symbols
    lv2_feature_ids = sql_finding_query(
        "features", 
        c("feature_name","feature_id"), 
        wheres=list("feature_name" = c(your_obj$signatures$signature_symbol))
    )
    fids = bind_rows(lv2_feature_ids)$feature_id
    fnames = bind_rows(lv2_feature_ids)$feature_name
    return(
        setdiff(
            your_obj$signatures$signature_symbol, 
            fnames
        )
    )
}

syno_fetch <- function(symbol_list, organism="Homo sapiens", genome=""){
    return("")
}

# Adds signature information to signatures table in the DB
# Inputs:
#   sig_name: name of signature
#   species_name: name of *species*
#   platform_name: name of assay platform
#   cell_line: name of cell line
# all fields are usually obtained from input values from application
add_signature <- function(sig_name, species_name, platform_name, cell_line) {
    #tracking upload date for DB insert
    now = Sys.time()
    signature_name = single_quoted(sig_name)
    my_species = (species_name)
    species_id_insert = sql_finding_query("species", c("species_id"), wheres=list("species"=my_species))$species_id[1]
    platform_id = as.integer(sql_finding_query("assay_platforms", c("platform_id"), wheres=list("platform_name"=platform_name))$platform_id[1])
    cell_line = cell_line
    insert_query = paste("insert into signatures(
                         signature_name,
                         upload_date,
                         species_id,
                         cell_line,
                         platform_id) values(", paste(signature_name,"now()",species_id_insert,cell_line,platform_id,sep=","),");")
    if(FALSE){
        print(insert_query)
    }
    insert_signature_conn = new_conn_handle()
    dbSendQuery(insert_signature_conn, insert_query)
    dbDisconnect(insert_signature_conn)
}



#function will insert lvl2/3 data into DB
#inputs: file path to lvl2/3 data file, signature id, signature_name
add_lv2 <- function(lv2_file, sid, sig_name){
    #reading file into a table
    #also accounts for uploading from an omicsig object
    #that object would contain a dataframe(list) representation
    #of lv2
    if( typeof(lv2_file) == "list" ){
        lv2_table = lv2_file
    }
    else {
        lv2_table = read.table(lv2_file, header=T)
    }
    if(FALSE){
        print(lv2_table$signature_symbol)
    }
    
    #
    
    #
    
    #converting direction to + or -
    lv2_table$signature_direction = as.character(lv2_table$signature_direction)
    lv2_table$signature_direction[which(tolower(lv2_table$signature_direction)=="up")]="+"
    lv2_table$signature_direction[which(tolower(lv2_table$signature_direction)=="dn")]="-"
    #fetching feature ids corresponding to symbols
    lv2_feature_ids = sql_finding_query(
                            "features", 
                            c("feature_name","feature_id"), 
                            wheres=list("feature_name" = c(lv2_table$signature_symbol))
                        )
    fids = bind_rows(lv2_feature_ids)$feature_id
    fnames = bind_rows(lv2_feature_ids)$feature_name
    #all coming from same signature, hence 'rep'
    sid_col = rep(sid, length(fids))
    
    #debugging block. set to TRUE for printing variables to console
    if(FALSE){
        print(lv2_feature_ids)
        print(fids)
        print(lv2_table$score)
        print(lv2_table$direction)
        print(length(fids))
        print(length(sid_col))
        print(length(lv2_table$signature_score))
        print(length(lv2_table$direction))
    }
    

    # dataframe for inserting into db
    # need to integrate synonym checker function here instead of just
    # merging what works(temporary solution)
    lv2_insert.df = merge(lv2_table, lv2_feature_ids, by.x="signature_symbol", by.y="feature_name")
    #making insert query for all records at once. if transaction fails, 
    #sql will rollback, which is what we want.
    insert_records = paste(
        "(",
        sid_col, ",", 
        lv2_insert.df$feature_id, ",", 
        lv2_insert.df$signature_score, ",", 
        single_quoted(lv2_insert.df$direction),")",sep="",collapse=","
    )
    insert_lv2_query = paste(
        "INSERT INTO feature_signature(signature_id,feature_id,weight,direction) VALUES ",
        insert_records,
        sep="")
    #executes insert
    sql_generic(insert_lv2_query)
    write.csv(lv2_table, paste(upload_root_path,sig_name,"/level_2/",sig_name,".csv",sep=""))
}


#function for adding keyword-signature pairs
#inputs: 
#   character vector containing keywords
#   signature id associated with keywords
add_signature_keywords <- function(keyword_v, sid){
  keywords = unique(as.character(keyword_v))
  #what keywords already are in the DB?
  #isolate new keywords and insert into core table
  keyword_sql = bind_rows(
    lapply(
      keywords,
      sql_finding_query,
      fields=c("keyword_id","keyword"),
      target_table="keywords",
      field_where="keyword"
      )
    )
  #new keywords
  new_keywords = setdiff(keywords, keyword_sql$keyword)
  #if there are any, add them to db
  if(length(new_keywords)!=0){
    insert_newkeywords_query = paste("INSERT INTO keywords(keyword) VALUES ",paste("(",new_keywords,")",sep="",collapse=","),";")
    insert_newkeywords_sql = sql_generic(insert_newkeywords_query)
    new_keyword_ids = bind_rows(
      lapply(
        new_keywords,
        sql_finding_query,
        fields=c("keyword_id"),
        target_table="keywords",
        field_where="keyword"
      )
    )$keyword_id
  }
  #make dataframe of insert values
  #if there's a function that mass inserts a dataframe into the db
  #i'd like to do that instead of all these pastes
  keyword_signature.df<-data.frame(
    signature_id = rep(sid, length(keyword_v)),
    keyword_id = bind_rows(keyword_sql$keyword_id,new_keyword_ids)
  )
  keyword_signature_insert_query = paste(
      "INSERT INTO keyword_signature(signature_id,keyword_id) VALUES ",
      paste("(",
            keyword_signature.df$signature_id,",",
            keyword_signature.df$keyword_id,
            ")",sep="",collapse=","),
            ";"
      )
  sql_generic(keyword_signature_insert_query)
}


##=============================================================##
##------------------Observe Blocks-----------------------------##
##=============================================================##

#When you click the 'submit' button...
observeEvent(input$add_signature, {
    #where will these rds files live?
    #can expand on this function as we become more confident in
    #backend(DB) setup(i.e. hierarchical file system)
    write_signature_file(input$rds_file_1,input$signature_name)
    # observe...tryCatch blocks are used to capture errors
    # without having the app instance crash on your browser/app viewer
    observe(
        tryCatch(
            add_signature(input$signature_name, 
                          input$species_id, 
                          input$platform_name, 
                          single_quoted(input$cell_line)),
            error = function(e){
                #outputting the actual error message would be nice; however,
                #would not want to output any sql in the error message(which could happen),
                #could lead to exploits/people knowing the DB structure
                shinyalert("Hm something went wrong while uploading this signature...")
            }
        )
    )
    #since signature is inserted now, we can get its signature id
    #and feed it into the lvl2/3 upload function
    last_sid = as.integer(sql_generic(paste("select signature_id from signatures where signature_name=",(signature_name),";",sep=""))$signature_id[1])
    add_lv2(input$rds_file_2$datapath,last_sid)
    if(length(input$keywords)!=0){
      add_signature_keywords(input$keywords,last_sid, signature_name)
    }
})

## Checks upon upload if any symbols in signature are missing in our db.
## outputs message warning user if so.
## input is OmicSig object
observeEvent(input$omicobj_upload, {
    this = read_json(input$omicobj_upload$datapath)
    this.misfits = odd_ones_out(this)
    if(length(this.misfits)>0){
        output$errday=renderText(paste("The following features don't seem to exist
in our DB: ", paste(this.misfits,collapse=", ")))
        shinyjs::disable("upload_object")
    }
    else{
        output$errday=renderText("signature has the right feature symbols. go for it")
        shinyjs::enable("upload_object")
    }
})

##

#When you click the 'submit' button...
#Yeah I click the 'submit' button
observeEvent(input$upload_object, {
    signature_object = read_json(input$omicobj_upload$datapath)
    signature_name = (input$signature_object_name)
    write_signature_file(input$omicobj_upload, input$omicobj_upload$file$name)
    if( length(signature_object[['difexp']]) > 0){
        write.table(signature_object[['difexp']], paste(upload_root_path, signature_name, "level_1", input$omicobj_upload$name, sep="/"))
    }
    fdr_cutoff = 0.05
    logfc_cutoff = 1
    if(signature_object$metadata$fdr_cutoff!=""){
        fdr_cutoff = signature_object$metadata$fdr_cutoff 
    }
    if(is.null(signature_object$signatures)){
        signature_object$signatures=signature_object$extract.signature(
            paste("abs(logFC) > ",logfc_cutoff, "; fdr < ", fdr_cutoff,sep="")
        )
    }
    sig_meta = signature_object$metadata
    
    if(FALSE){    
        observe(
            tryCatch(
                {
                    add_signature(input$signature_object_name, 
                              sig_meta$organism, 
                              sig_meta$platform, 
                              single_quoted(sig_meta$cell_lines))
                    print("cool")
                    #since signature is inserted now, we can get its signature id
                    #and feed it into the lvl2/3 upload function
                    last_sid = as.integer(sql_finding_query("signatures",c("signature_id"),wheres=list("signature_name"=input$signature_object_name))$signature_id[1])
                    add_lv2(signature_object$signatures,last_sid, signature_name)
                    # if(length(signature_object$metadata$keywords)!=0){
                    #     add_signature_keywords(signature_object$metadata$keywords, last_sid, signature_name)
                    # }
                    shinyalert("Insert Successful")
                },
                error = function(e){
                    shinyalert("Hm something went wrong while uploading this signature to the DB...")
                    print(e)
                }
            )
        )
    }
})

# Update options in species dropdown menu with list of species from database
observe({
    updateSelectizeInput(session,
                         "species_id",
                         choices = c("", get_species()))
})

# Update options in platform dropdown menu with list of platforms from database
observe({
    updateSelectizeInput(session,
                         "platform_name",
                         choices = c("", get_platforms()))
})