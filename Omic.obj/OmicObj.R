library(R6)
library(dplyr)

{ # OmicSignature define
  OmicSignature <- R6Class("OmicSignature", list(
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
        cat("    ", k, " (", v, ")", "\n", sep = "")
      }, names(summary(self$signatures$signature_direction)), summary(self$signatures$signature_direction))
      # old code for signature as a list:
      # sh <- mapply(function(k, v) {
      #    cat("    ", k, " (", length(v), ")", "\n", sep = "")
      # }, names(self$signatures), self$signatures)
      cat("  Differential Expression Data: \n")
      cat("    ", nrow(self$difexp), " x ", ncol(self$difexp), "\n", sep = "")
      invisible(self)
    },
    extract.signature = function(conditions) {
      v <- rlang::parse_exprs(conditions)
      res <- self$difexp %>%
        dplyr::filter(!!!v) #%>%
        #dplyr::pull(symbol)
      res <- res[,c("symbol", "Score")]
      direction<-character()
      direction[which(res$Score<0)]<-"Dn"
      direction[which(res$Score>0)]<-"Up"
      res <- cbind(res, direction)
      res <- res[order(res$direction, decreasing = FALSE),]
      # don't know why, but after order() them by "direction", in each direaction, 
      # it automatically sorted the score from absolute value the highest to the lowest,
      # which is useful
      colnames(res) <- c("signature_symbol", "signature_score", "signature_direction")
      return(res)
    }
  ))
}
