library(R6)
library(dplyr)

check_metadata <- function(metadata) {
  # should be a list
  if (class(metadata)=="list") { 
    return (paste("checked. metadata is a list."))
  } else { return (paste("Error: metadata should be a list.", sep="")) }
}

check_signatures <- function(signatures) {
  # should be list, length of 2, each are factor/character. Up Regulated and Dn Regulated
  if (class(signatures)=="list") { 
    if (length(signatures)==2){
      print(paste("checked. signature is a list of length 2. "))
    } else { return (paste("Error: signature should be a list of length 2. ", sep="")) }
  }
  if (names(signatures[1])=="Up Regulated" & names(signatures[2])=="Dn Regulated"){
    print(paste("checked. signature contains *Up Regulated* and *Dn Regulated* elements. "))
  } else {
    print(paste("notice: signature is not named as *Up Regulated* and *Dn Regulated*, the names are changed accordingly. "))
    names(signatures)=c("Up Regulated","Dn Regulated")
  }
  if (class(signatures[[1]])=="factor"){signatures[[1]]=as.character(signatures[[1]])}
  if (class(signatures[[2]])=="factor"){signatures[[2]]=as.character(signatures[[2]])}
  if (class(signatures[[1]])!="character" | class(signatures[[2]])!="character"){
    return(paste("Error: signature should be gene symbols. "))
  } else {
    print(paste("checked. signature is represented by gene symbols. "))
  }
}

check_difexp <- function(difexp) {
  # should be data frame, contain columns: "logFC", "AveExpr", "t", "P.Value", "fdr", "B", "symbol"
  
  # check if it's data.frame:
  if (class(difexp)=="matrix") { difexp=as.data.frame(difexp) }
  if (class(difexp)=="data.frame") {
    print(paste("checked. input Differential Express is a data frame"))
  } else {
    return (paste("Error: Input Differential Express is not a data.frame or matrix"))
    # use "return" instead of "print" here, so if this criteria is not met, the following steps will not run
  }
  
  # check column names:
  difexp_colname_missing=setdiff(c("logFC", "AveExpr","t","P.Value","fdr","B","symbol"),colnames(difexp))
  if (length(difexp_colname_missing)==0){
    print(paste("checked. input Differential Express contain all the essential columns."))
  } else { print(paste("Warning: Input Differential Express does not contain column(s): ", paste(difexp_colname_missing, collapse=", "), ". This can cause problem when retriving data.", sep=""))}
  
  # check column type:
  # "logFC","AveExpr","t","P.Value","fdr","B" should be numerical
  for (numeric_colname in c("logFC","AveExpr","t","P.Value","fdr","B")){
    if (numeric_colname %in% colnames(difexp)) {
      if (class(difexp[,numeric_colname])=="numeric") { 
        print(paste("checked. ", numeric_colname, " is numeric.", sep="")) 
      } else { print(paste("Warning: ", numeric_colname, " is not numeric.")) }
    }
  }
  
  # "symbol" should be character. if it's factor, then change it into character
  if ("symbol" %in% colnames(difexp)){
    if (class(difexp$symbol)=="factor") { difexp$symbol=as.character(difexp$symbol) }
    if (class(difexp$symbol)=="character") { 
      print(paste("checked. symbol is character.", sep=""))
    } else { print(paste("Warning: symbol is not character."))}
  }
}


OmicSignature <- R6Class("OmicSignature", list(
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
        sh <- mapply(function(k, v)
            cat("   ", k, "=", v, "\n"), names(self$metadata), self$metadata)
        cat("  Signatures: \n")
        sh <- mapply(function(k, v)
            cat("    ", k, " (", length(v), ")", "\n", sep=""), names(self$signatures), self$signatures)
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

OmicCollection <- R6Class("OmicCollection", list(
    data = NULL,
    initialize = function(data) {
        stopifnot(lapply(data, is) == "OmicSignature")
        self$data <- data
    }
))
if ( FALSE )
{
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
sig.obj <- OmicSignature$new(metadata, signatures, difexp)
print(sig.obj)
sig.obj$extract.signature('logfc > 0.5')
sig.obj$extract.signature('logfc < -0.5; fdr < 0.001')
sig.obj$extract.signature('abs(logfc) > 0.5; fdr < 0.001')
}
