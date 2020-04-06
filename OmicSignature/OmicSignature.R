library(R6)
library(dplyr)

{
  source("OmicSignature/check_functions/Function_objCheck.R")
  ## OmicSignature define
  ##
  OmicSignature <-
    R6Class(
      "OmicSignature",
      list(
        metadata = NULL,
        signatures = NULL,
        difexp = NULL,
        initialize = function(metadata, signatures, difexp = NULL, print_message = FALSE) {
          metadata <- check_metadata(metadata, v = print_message)
          if (!is.null(difexp)) {
            difexp <- check_difexp(difexp, v = print_message)
          }
          signatures <- check_signature(signatures, signature_type = metadata$type, v = print_message)
          self$metadata <- metadata
          self$signatures <- signatures
          self$difexp <- difexp
          cat("  [Success] OmicSignature object created.\n")
        },
        print = function(...) {
          cat("Signature Object: \n")
          cat("  Metadata: \n")
          sh <- mapply(function(k, v) {
            cat("   ", k, "=", v, "\n")
          }, names(self$metadata), self$metadata)
          cat("  Signatures: \n")
          sh <- mapply(
            function(k, v) {
              cat("    ", k, " (", v, ")", "\n", sep = "")
            }, names(summary(self$signatures$signature_direction)),
            summary(self$signatures$signature_direction)
          )
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
            dplyr::select(symbol, score) %>%
            dplyr::mutate(direction = ifelse(score < 0, "-", "+")) %>%
            dplyr::arrange(direction) %>%
            dplyr::rename(symbol = signature_symbol, score = signature_score, direction = signature_direction)
          return(res)
        }
      )
    )
}
