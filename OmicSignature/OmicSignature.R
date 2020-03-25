library(R6)
library(dplyr)

{
    ## OmicSignature define
    ##
    OmicSignature <-
        R6Class(
        "OmicSignature",
        list(metadata = NULL,
             signatures = NULL,
             difexp = NULL,
             initialize = function(metadata, signatures, difexp = NULL) {
                 self$metadata <- metadata
                 self$signatures <- signatures
                 self$difexp <- difexp
             },
             print = function(...) {
                 cat("Signature Object: \n")
                 cat("  Metadata: \n")
                 sh <- mapply(function(k, v) {
                     cat("   ", k, "=", v, "\n")
                 }, names(self$metadata), self$metadata)
                 cat("  Signatures: \n")
                 sh <- mapply(function(k, v) {
                     cat("    ", k, " (", v, ")", "\n", sep = "")
                 }, names(summary(self$signatures$signature_direction)),
                 summary(self$signatures$signature_direction))
                 ## old code for signature as a list:
                 ## sh <- mapply(function(k, v) {
                 ##    cat("    ", k, " (", length(v), ")", "\n", sep = "")
                 ## }, names(self$signatures), self$signatures)
                 cat("  Differential Expression Data: \n")
                 cat("    ", nrow(self$difexp), " x ", ncol(self$difexp), "\n", sep = "")
                 invisible(self)
             },
             extract.signature = function(conditions) {
                 v <- rlang::parse_exprs(conditions)
                 res <- self$difexp %>%
                     dplyr::filter(!!!v) %>%
                     dplyr::select(symbol,score) %>%
                     dplyr::mutate(direction=ifelse(score<0,"Dn","Up")) %>%
                     dplyr::arrange(direction) %>%
                     dplyr::rename(symbol=signature_symbol,score=signature_score,direction=signature_direction)
                     ## Callen Comment
                     ## Can we instead assign '+' and '-' as direction values?
                     ## dplyr::mutate(res,direction=ifelse(score<0,"-","+"))
                 return(res)
             }
             )
    )
}
