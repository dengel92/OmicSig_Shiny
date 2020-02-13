# Server logic for upload page

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





#function will insert lvl2/3 data into DB
#inputs: file path to lvl2/3 data file, signature id
add_lv2 <- function(lv2_file, sid){
  #reading file into a table
  lv2_table = read.table(lv2_file, header=T)
  #converting direction to + or -
  lv2_table$direction = as.character(lv2_table$direction)
  lv2_table$direction[which(tolower(lv2_table$direction)=="up")]="+"
  lv2_table$direction[which(tolower(lv2_table$direction)=="dn")]="-"
  #fetching feature ids corresponding to symbols
  lv2_feature_ids = (lapply(as.character(lv2_table$symbol), 
         sql_finding_query,
         fields="feature_id", 
         target_table="features", 
         field_where="feature_name"))
  fids = bind_rows(lv2_feature_ids)$feature_id
  #all coming from same signature, hence 'rep'
  sid_col = rep(sid, length(fids))
  
  if(TRUE){
    print(lv2_feature_ids)
    print(fids)
    print(lv2_table$score)
    print(lv2_table$direction)
    print(length(fids))
    print(length(sid_col))
    print(length(lv2_table$score))
    print(length(lv2_table$direction))
  }
  
  #dataframe for inserting into db
  lv2_insert.df = data.frame(
    signature_id = sid_col,
    feature_id = fids,
    weight = lv2_table$score,
    direction = single_quoted(lv2_table$direction)
  )
  #making insert query for all records at once. if transaction fails, 
  #sql will rollback, which is what we want.
  insert_records = paste("(",lv2_insert.df$signature_id, ",", lv2_insert.df$feature_id, ",", lv2_insert.df$weight, ",", lv2_insert.df$direction,")",sep="",collapse=",")
  insert_lv2_query = paste("INSERT INTO feature_signature(signature_id,feature_id,weight,direction) VALUES ",insert_records,sep="")
  #executes insert
  sql_generic(insert_lv2_query)
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
  keyword_signature_insert_query = paste("INSERT INTO keyword_signature(signature_id,keyword_id) VALUES ",paste("(",keyword_signature.df$signature_id,",",keyword_signature.df$keyword_id,")",sep="",collapse=","),";")
  sql_generic(keyword_signature_insert_query)
}


#When you click the 'submit' button...
observeEvent(input$add_signature, {
    #where will these rds files live?
    #can expand on this function as we become more confident in
    #backend(DB) setup(i.e. hierarchical file system)
    rds_dir<-"/data_files/rds/"
    #alerting user about where the file's going.
    #shinyalert(paste("writing file to ",rds_dir,input$rds_file_1$name,sep=''))
    #copy_file(input$rds_file_1$datapath,paste(rds_dir,input$rds_file_1$name,sep=''))
    #need to add a 'hashkey' ability. That is, instead of just copying the file as '/path/to/<filename>', we would either
    # 1) write as '/path/to/<random hash name of file>' and reflect in DB
    # 2) write the file with the original name, but have soft links named <hash name> that point to the original file, with the hash key
    #     also reflected in the DB
    #also need to add alerts confirming to the user that the operation is/was successful
    now = Sys.time()
    signature_name = single_quoted(input$signature_name)
    my_species = single_quoted(input$species_id)
    species_tax_id = as.integer(sql_generic(paste("select taxonomic_id from species where species=",(my_species),";",sep=""))$taxonomic_id[1])
    species_id_insert = sql_finding_query(c("species_id"),"species","taxonomic_id",species_tax_id)$species_id[1]
    platform_id = as.integer(sql_finding_query(c("platform_id"),"assay_platforms","platform_name",input$platform_name)$platform_id[1])
    cell_line = input$cell_line
    insert_query = paste("insert into signatures(
                              signature_name,
                              upload_date,
                              species_id,
                              platform_id) values(", paste(signature_name,"now()",species_id_insert,platform_id,sep=","),");")
    insert_signature_conn = new_conn_handle()
    dbSendQuery(insert_signature_conn, insert_query)
    dbDisconnect(insert_signature_conn)
    #since signature is inserted now, we can get its signature id
    #and feed it into the lvl2/3 upload function
    last_sid = as.integer(sql_generic(paste("select signature_id from signatures where signature_name=",(signature_name),";",sep=""))$signature_id[1])
    add_lv2(input$rds_file_2$datapath,last_sid)
    if(length(input$keywords)!=0){
      add_signature_keywords(input$keywords,last_sid)
    }
})