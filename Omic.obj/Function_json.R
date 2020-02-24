write_obj <- function(omic.obj, file) {
  signature_direction <- summary(omic.obj$signatures$signature_direction)
  # drop the signature_direction column:
  omic.obj$signatures <- omic.obj$signatures[, !(colnames(signatures) %in% "signture_direction")]
  metadata_length <- length(omic.obj$metadata)
  write_json_obj <- rjson::toJSON(c(
    omic.obj$metadata,
    "metadata_length" = metadata_length,
    omic.obj$signatures,
    signature_direction,
    list("lv1_colnames" = colnames(omic.obj$difexp)),
    omic.obj$difexp
  ))
  write(write_json_obj, file)
  return("finished")
}

read_json <- function(filename) {
  read_json <- rjson::fromJSON(file = filename)
  read_meta <- read_json[c(1:read_json$metadata_length)]
  lv1_colnames <- read_json$lv1_colnames
  print(lv1_colnames)
  read_lv1 <- as.data.frame(dplyr::bind_rows(read_json[c(lv1_colnames)]))
  read_lv2 <- as.data.frame(dplyr::bind_rows(read_json[c("signature_symbol", "signature_score")]))
  read_direction <- read_json$signature_direction
  signature_direction <- rep(names(signature_direction), signature_direction)
  read_lv2 <- cbind(read_lv2, signature_direction)
  read_sig.obj <- OmicSignature$new(read_meta, read_lv2, read_lv1)
  return(read_sig.obj)
}
