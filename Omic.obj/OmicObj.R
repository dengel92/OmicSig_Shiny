library(R6)
library(dplyr)

{ # OmicCollection define
  OmicCollection <- R6Class("OmicCollection", list(
    metadata = NULL,
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
        cat("    ", k, " (", length(v), ")", "\n", sep = "")
      }, names(self$signatures), self$signatures)
      cat("  Differential Expression Data: \n")
      cat("    ", nrow(self$difexp), " x ", ncol(self$difexp), "\n", sep = "")
      invisible(self)
    },
    extract.signature = function(conditions) {
      v <- rlang::parse_exprs(conditions)
      self$difexp %>%
        dplyr::filter(!!!v) %>%
        dplyr::pull(symbol)
    }
  ))
}
