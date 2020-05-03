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
  signatureDirection <- summary(OmicObj$signatures$signatureDirection)
  # drop the signatureDirection column:
  writeSignature <- OmicObj$signatures
  writeSignature$signatureDirection <- NULL
  metadataLength <- length(OmicObj$metadata)
  writeJson_obj <- jsonlite::toJSON(c(
    OmicObj$metadata,
    "metadataLength" = metadataLength,
    list("signatureDirectionNames" = names(signatureDirection)),
    signatureDirection,
    writeSignature,
    list("lv1Colnames" = colnames(OmicObj$difexp)),
    OmicObj$difexp
  ), na = NULL, pretty = T)
  write(writeJson_obj, file)
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
  readMetadata <- readJson[c(1:readJson$metadataLength)]
  lv1Colnames <- readJson$lv1Colnames
  readLv1 <- data.frame(dplyr::bind_rows(readJson[c(lv1Colnames)]))
  readLv2 <- data.frame(dplyr::bind_rows(readJson[c("signature_symbol", "signature_score")]))
  signatureDirection <-
    rep(readJson$signatureDirectionNames, unlist(readJson[readJson$signatureDirectionNames]))
  readLv2 <- cbind(readLv2, signatureDirection)
  readSigObj <- OmicSignature$new(readMetadata, readLv2, readLv1)
  return(readSigObj)
}
