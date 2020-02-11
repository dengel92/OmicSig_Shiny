write_obj <- function(omic.obj, file) {
  write_lv2lv3 <- list(
    "Up_Regulated_Symbol" = omic.obj$signatures$Up_Regulated_Symbol,
    "Dn_Regulated_Symbol" = omic.obj$signatures$Dn_Regulated_Symbol,
    "Up_Regulated_Score" = omic.obj$signatures$Up_Regulated_Score,
    "Dn_Regulated_Score" = omic.obj$signatures$Dn_Regulated_Score
  )
  metadata_length <- length(omic.obj$metadata)
  write_json_obj <- rjson::toJSON(c(omic.obj$metadata,
    "metadata_length" = metadata_length,
    write_lv2lv3,
    list("lv1_colnames" = colnames(omic.obj$difexp)),
    omic.obj$difexp
  ))
  write(write_json_obj, file)
  return("finished")
}

read_json <- function(filename) {
  read_json <- rjson::fromJSON(file = filename)
  lv1_colnames <- read_json$lv1_colnames
  read_lv1 <- as.data.frame(dplyr::bind_rows(read_json[c(lv1_colnames)]))
  read_lv2lv3 <- list(
    "Up_Regulated_Symbol" = read_json$Up_Regulated_Symbol,
    "Dn_Regulated_Symbol" = read_json$Dn_Regulated_Symbol,
    "Up_Regulated_Score" = read_json$Up_Regulated_Score,
    "Dn_Regulated_Score" = read_json$Dn_Regulated_Score
  )
  read_meta <- read_json[c(1:read_json$metadata_length)]

  read_sig.obj <- OmicCollection$new(read_meta, read_lv2lv3, read_lv1)
  return(read_sig.obj)
}
