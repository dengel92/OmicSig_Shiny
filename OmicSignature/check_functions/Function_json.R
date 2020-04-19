#######################################################################
## write_json()
##
#' @title write OmicSignature object into json txt format
#' updated 01/2020
#'
#' @param omic.obj A OmicSignature object
#' @param file export file name
#' @return a "finished" message
#'
#' @example
#' write_json(Omic.obj, file = "Omic.json")
#'

write_json <- function(omic.obj, file) {
  signature_direction <- summary(omic.obj$signatures$signature_direction)
  # drop the signature_direction column:
  write_signature <- omic.obj$signatures
  write_signature$signature_direction <- NULL
  metadata_length <- length(omic.obj$metadata)
  write_json_obj <- jsonlite::toJSON(c(
    omic.obj$metadata,
    "metadata_length" = metadata_length,
    list("signature_direction_names" = names(signature_direction)),
    signature_direction,
    write_signature,
    list("lv1_colnames" = colnames(omic.obj$difexp)),
    omic.obj$difexp
  ), na = NULL, pretty = T)
  write(write_json_obj, file)
  return("finished")
}

#######################################################################
## read_json()
##
#' @title read an OmicSignature object from json txt file created by write_json()
#' updated 01/2020
#'
#' @param filename json file name to read in
#' @return OmicSignature object
#'
#' @example
#' read_json(filename = "Omic.json")
#'
read_json <- function(filename) {
  read_json <- jsonlite::fromJSON(txt = filename)
  read_meta <- read_json[c(1:read_json$metadata_length)]
  lv1_colnames <- read_json$lv1_colnames
  read_lv1 <- data.frame(dplyr::bind_rows(read_json[c(lv1_colnames)]))
  read_lv2 <- data.frame(dplyr::bind_rows(read_json[c("signature_symbol", "signature_score")]))
  signature_direction <-
    rep(read_json$signature_direction_names, unlist(read_json[read_json$signature_direction_names]))
  read_lv2 <- cbind(read_lv2, signature_direction)
  read_sig.obj <- OmicSignature$new(read_meta, read_lv2, read_lv1)
  return(read_sig.obj)
}
