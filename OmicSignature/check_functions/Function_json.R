#######################################################################
## writeJson()
##
#' @title write OmicSignature object into json txt format
#' updated 01/2020
#'
#' @param OmicObj A OmicSignature object
#' @param file export file name
#' @return a "finished" message
#'
#' @example
#' writeJson(OmicObj, file = "Omic.json")
#'

writeJson <- function(OmicObj, file) {
  signature_direction <- summary(OmicObj$signatures$signature_direction)
  # drop the signature_direction column:
  writeSignature <- OmicObj$signatures
  writeSignature$signature_direction <- NULL
  metadata_length <- length(OmicObj$metadata)
  writeJsonObj <- jsonlite::toJSON(c(
    OmicObj$metadata,
    "metadata_length" = metadata_length,
    list("signature_direction_names" = names(signature_direction)),
    signature_direction,
    writeSignature,
    list("lv1_colnames" = colnames(OmicObj$difexp)),
    OmicObj$difexp
  ), na = NULL, pretty = T)
  write(writeJsonObj, file)
  return("finished")
}

#######################################################################
## readJson()
##
#' @title read an OmicSignature object from json txt file created by writeJson()
#' updated 01/2020
#'
#' @param filename json file name to read in
#' @return OmicSignature object
#'
#' @example
#' readJson(filename = "Omic.json")
#'
readJson <- function(filename) {
  readJson <- jsonlite::fromJSON(txt = filename)
  readMetadata <- readJson[c(1:readJson$metadata_length)]
  lv1_colnames <- readJson$lv1_colnames
  readLv1 <- data.frame(dplyr::bind_rows(readJson[c(lv1_colnames)]))
  readLv2 <- data.frame(dplyr::bind_rows(readJson[c("signature_symbol", "signature_score")]))
  signature_direction <-
    rep(readJson$signature_direction_names, unlist(readJson[readJson$signature_direction_names]))
  readLv2 <- cbind(readLv2, signature_direction)
  readSigObj <- OmicSignature$new(readMetadata, readLv2, readLv1)
  return(readSigObj)
}
