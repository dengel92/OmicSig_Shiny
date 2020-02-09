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


{ # Testing data example
  metadata <- list(
    "organism" = "human",
    "tissue" = "cell",
    "phenotype" = "carginogenic",
    "type" = "bi-directional",
    "platform" = "illumina",
    "keywords" = c("carcinogenome", "cancer", "chemicals", "pertubations")
  )


  difexp <- read.table("Omic.obj/example_diffmatrix_MDA_AhR.txt", header = TRUE, stringsAsFactors = FALSE)
  # change the column names to the standard names as required in OmicSig Obj lv1:
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "t"), "Score")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "logfc"), "logFC")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "gene_symbol"), "symbol")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "adj.P.Val"), "fdr")
  difexp$Probe_ID <- rownames(difexp)

  signatures <- list( # a bi-directional example
    "Up_Regulated_Symbol" = filter(difexp, Score > 0 & fdr < 0.001) %>% pull(symbol),
    "Dn_Regulated_Symbol" = filter(difexp, Score < 0 & fdr < 0.001) %>% pull(symbol),
    "Up_Regulated_Score" = filter(difexp, Score > 0 & fdr < 0.001) %>% pull(Score),
    "Dn_Regulated_Score" = filter(difexp, Score < 0 & fdr < 0.001) %>% pull(Score)
  )

  # Object:
  Omic.obj <- OmicCollection$new(metadata, signatures, difexp)
  print(Omic.obj)
}

# extract signature function:
Omic.obj$extract.signature("logFC > 0.5")
Omic.obj$extract.signature("logFC < -0.5; fdr < 0.001")
Omic.obj$extract.signature("abs(logFC) > 0.5; fdr < 0.001")
Omic.obj$signatures

# test check-functions:
source("Omic.obj/Function_objCheck.R")
check_metadata(Omic.obj$metadata)
check_difexp(Omic.obj$difexp)
check_signatures(Omic.obj)
check_signatures(Omic.obj$signatures, signature_type = "bi-directional")
