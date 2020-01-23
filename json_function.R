write_Obj_to_json=function(omic.obj,file){
  write_lv2lv3=list("Up_Regulated_symbol"=omic.obj$signatures$Up_Regulated_symbol,
                    "Dn_Regulated_symbol"=omic.obj$signatures$Dn_Regulated_symbol,
                    "Up_Regulated_score"=omic.obj$signatures$Up_Regulated_score,
                    "Dn_Regulated_score"=omic.obj$signatures$Dn_Regulated_score
                 )
  metadata_length=length(omic.obj$metadata)
  write_json_obj=rjson::toJSON(c(omic.obj$metadata,
                                 "metadata_length"=metadata_length,
                                 write_lv2lv3,
                                 omic.obj$difexp))
  write(write_json_obj,file)
  return ("finished")
}

read_json_to_Obj=function(filename){
  read_json=rjson::fromJSON(file=filename)
  
  read_lv1=as.data.frame(dplyr::bind_rows(read_json[c("probe_ID","symbol","logFC","AveExpr","t","P.Value","fdr","B")]))
  read_lv2lv3=list("Up_Regulated_symbol"=read_json$Up_Regulated_symbol,
                   "Dn_Regulated_symbol"=read_json$Dn_Regulated_symbol,
                   "Up_Regulated_score"=read_json$Up_Regulated_score,
                   "Dn_Regulated_score"=read_json$Dn_Regulated_score)
  read_meta=read_json[c(1:read_json$metadata_length)]
  
  read_sig.obj <- OmicSignature$new(read_meta, read_lv2lv3, read_lv1)
  return(read_sig.obj)
}
