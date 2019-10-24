library(R6)
library(dplyr)

check_metadata <- function(metadata) {
}
check_signatures <- function(signatures) {
}
check_difexp <- function(difexp) {
}

sig <- R6Class("sig", list(
    metadata   = NULL,
    signatures = NULL,
    difexp     = NULL,
    initialize = function(metadata, signatures, difexp=NULL) {
        self$metadata   <- metadata
        self$signatures <- signatures
        self$difexp     <- difexp
    },
    print = function(...) {
        cat("Signature Object: \n")
        cat("  Metadata: \n")
        sh <- mapply(function(k, v) cat("   ", k, "=", v, "\n"), names(self$metadata), self$metadata) 
        cat("  Signatures: \n")
        sh <- mapply(function(k, v) cat("    ", k, " (", length(v), ")", "\n", sep=""), names(self$signatures), self$signatures)
        cat("  Differential Expression Data: \n")
        cat("    ", nrow(self$difexp), " x ", ncol(self$difexp), "\n", sep="")
        invisible(self)
    },
    extract.signature = function(conditions) {
        v <- rlang::parse_exprs(conditions)
        self$difexp %>%
        dplyr::filter(!!!v) %>%
        dplyr::pull(symbol)
    }
))

# Testing data
metadata <- list("organism"  = "human",
                 "tissue"    = "cell",
                 "phenotype" = "carginogenic",
                 "type"      = "bidirectional",
                 "keywords"  = c("carcinogenome", "cancer", "chemicals", "pertubations"))

difexp <- read.table("data/difexp.txt", header=TRUE, stringsAsFactors=FALSE)

signatures <- list("Up Regulated" = filter(difexp, t > 0 & fdr < 0.001) %>% pull(symbol),
                   "Dn Regulated" = filter(difexp, t < 0 & fdr < 0.001) %>% pull(symbol))

# Object examples
sig.obj <- sig$new(metadata, signatures, difexp)
print(sig.obj)
sig.obj$extract.signature('logfc > 0.5')
sig.obj$extract.signature('logfc < -0.5; fdr < 0.001')
sig.obj$extract.signature('abs(logfc) > 0.5; fdr < 0.001')
