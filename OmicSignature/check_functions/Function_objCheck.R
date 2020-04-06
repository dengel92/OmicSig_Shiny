# use verbose() function so the message is optionally printed. default is False
verbose <- function(v, ...) {
  if (v) cat(...)
}

## check_metadata() -------------------------------------
##
check_metadata <- function(metadata, v = FALSE) {
  ## metadata should be a list with required attributes
  if (class(metadata)[1] == "OmicSignature") {
    metadata <- metadata$metadata
  }
  if (class(metadata) == "list") {
    verbose(v, "  Metadata: Checked; is a list. \n")
  }
  else {
    stop("Metadata not found or metadata is not a list. ")
  }
  metadata_required <- c("organism", "platform", "type", "phenotype")
  metadata_missing <- setdiff(metadata_required, names(metadata))
  verbose(v, paste("  --Required attributes for metadata: ", paste(metadata_required, collapse = ", "), " --\n", sep = ""))

  if (length(metadata_missing) == 0) {
    verbose(v, paste("  Metadata: Checked; contains all the essential attributes. \n"))
  } else {
    stop("Metadata does not contain attribute(s): ",
      paste(metadata_missing, collapse = ", "),
      ". This can cause problem when retriving data.",
      sep = ""
    )
  }
  verbose(v, "  [Success] Metadata is saved. \n")
  return(metadata)
}
## check_signature()---------------------------------------------
## we need the information of "signature_type" in the metadata
## so, the input need to either be OmicSignature Obj, or need to specify signature type
check_signature <- function(omic.obj, signature_type = NULL, num_categories = 0, v = FALSE) {
  ## num_categories is used for multi-directional signature to specify
  ## read the signature, and check if it is a dataframe:
  if (class(omic.obj)[1] == "OmicSignature") {
    signature <- omic.obj$signature
    signature_type <- omic.obj$metadata$type
    if (signature_type == "multi-directional") {
      if (!is.null(omic.obj$metadata$num_categories)) {
        num_categories <- omic.obj$metadata$num_categories
      } else {
        stop("Signature is specified as multi-directional, but sample number not found.")
      }
    }
  }
  else if (class(omic.obj)[1] == "data.frame") {
    signature <- omic.obj
    remove(omic.obj)
    ## change possible column names to standard column names:
    colnames(signature) <- tolower(colnames(signature))
    colnames(signature) <- replace(colnames(signature), which(colnames(signature) == "signature"), "signature_symbol")
    colnames(signature) <- replace(colnames(signature), which(colnames(signature) == "symbol"), "signature_symbol")
    colnames(signature) <- replace(colnames(signature), which(colnames(signature) == "name"), "signature_symbol")
    colnames(signature) <- replace(colnames(signature), which(colnames(signature) == "score"), "signature_score")
    colnames(signature) <- replace(colnames(signature), which(colnames(signature) == "direction"), "signature_direction")
  } else {
    stop("Signature not found in OmicSignature object, or signature is not a dataframe.")
  }

  if (nrow(signature) == 0) {
    stop("Signature is empty.")
  }
  ## check if signature_symbol and signature_score (lv2 or lv3) exists:
  if (!c("signature_symbol") %in% colnames(signature)) {
    stop("Signature dataframe does not contain \"signature_symbol\" column.")
  }
  if (c("signature_score") %in% colnames(signature)) {
    verbose(v, "  Signature: contains lv2 data. \n")
  } else {
    cat("  Warning: Feature score not found, please make sure column \"signature_score\" presents if you have score for the features. \n")
  }

  ## check if the direction match with signature type:
  if (is.null(signature_type)) {
    stop("Signature type not specified. It needs to be uni-, bi- or multi-directional.")
  }
  ## each signature type need to be "else if". because if none of the
  ## type meet the criteria required, we need an "else" to output error.

  ## bi-directional signature:
  if (signature_type == "bi-directional") {
    if (!"signature_direction" %in% colnames(signature)) {
      stop("Signature is specified as bi-directional but \"signature_direction\" information not found.")
    }
    ## change direction symbol to + and - :
    signature$signature_direction <- tolower(signature$signature_direction)
    signature$signature_direction <- replace(signature$signature_direction, which(signature$signature_direction == "up"), "+")
    signature$signature_direction <- replace(signature$signature_direction, which(signature$signature_direction == "dn"), "-")
    signature$signature_direction <- replace(signature$signature_direction, which(signature$signature_direction == "down"), "-")
    signature$signature_direction <- as.factor(signature$signature_direction)

    ## check direction:
    summary_direction <- summary(signature$signature_direction)
    if (isTRUE(all.equal(c("-", "+"), names(summary(signature$signature_direction))))) {
      verbose(v, "  Signature: Checked, signature is bi-directional with - (Dn) and + (Up) directions. \n")
    } else if (names(summary_direction) == c("-") | names(summary_direction) == c("+")) {
      verbose(v, "  Signature: Checked, signature is bi-directional, but only one direction is found. \n")
    } else {
      stop("Direction info in bi-directional signature is not valid. Direction should be marked with \"-\" and \"+\".")
    }
  }

  ## uni-directional signature:
  else if (signature_type == "uni-directional") {
    if (!"signature_direction" %in% colnames(signature)) {
      verbose(v, "  Checked. signature is uni-directional. \n")
    } else {
      verbose(v, "  Warning: signature is specified as uni-directional but additional direction information found. \n")
    }
  }

  ## multi-directional signature:
  else if (signature_type == "multi-directional") {
    summary_direction <- summary(signature$signature_direction)
    if (length(summary_direction) == num_categories) {
      verbose(v, paste("  Checked. signature is multi-directional with", num_categories, "samples. \n", sep = " "))
    } else if (length(summary_direction) < num_categories) {
      verbose(v, paste("  Warning: signature is multi-directional with", num_categories, "samples, but some samples do not have signature. \n"))
    } else {
      stop("Signature is specified as multi-directional, but sample number is invalid.")
    }
  }

  ## if none of the signature type is met:
  else {
    stop("Error: Signature information invalid.")
  }
  verbose(v, "  [Success] Signature is valid. \n")
  return(signature)
}

## check_difexp()-----------------------------------------
## check_difexp() function supports input Obj or dataframe
check_difexp <- function(omic.obj, v = FALSE) {
  ## should be data frame, contain columns: c("probe_id", "symbol", "logfc", "score", "p.value", "fdr")
  if (class(omic.obj)[1] == "OmicSignature") {
    difexp <- omic.obj$difexp
  }
  else if (class(omic.obj)[1] == "data.frame") {
    difexp <- omic.obj
    remove(omic.obj)
  }
  else {
    stop("Input is not a OmicSignature object or dataframe. ")
  }
  ## check if it's empty:
  if (nrow(difexp) == 0) {
    stop("Differential Express Matrix (lv1 data) is empty. ")
  } else {
    verbose(v, "  difexp: Checked. Differential Express Matrix (lv1 data) is a data frame. \n")
  }

  ## check column names:
  ## note: if there are additional columns besides the required columns, there will NOT be any warning messages
  colnames(difexp) <- tolower(colnames(difexp))
  difexp_colname_required <- c("probe_id", "symbol", "logfc", "score", "p.value", "fdr")
  difexp_colname_missing <- setdiff(difexp_colname_required, colnames(difexp))
  difexp_colname_additional <- setdiff(colnames(difexp), difexp_colname_required)
  verbose(v, paste("  --Required columns for Differential Express Matrix (lv1 data): ", paste(difexp_colname_required, collapse = ", "), " --\n", sep = ""))

  if (length(difexp_colname_missing) == 0) {
    verbose(v, "  difexp: Checked. Differential Express Matrix (lv1 data) contain all the essential columns. \n")
  } else {
    stop("Differential Express Matrix (lv1 data) does not contain required column(s): ", paste(difexp_colname_missing, collapse = ", "), ".", sep = "")
  }
  if (length(difexp_colname_additional) > 0) {
    verbose(v, paste("  difexp: additional columns found: ", paste(difexp_colname_additional, collapse = ", "), ". \n", sep = ""))
  }
  ## check column type:
  ## "logfc","score","p.value","fdr" should be numerical
  for (numeric_colname in c("logfc", "score", "p.value", "fdr")) {
    if (numeric_colname %in% colnames(difexp)) {
      if (class(difexp[, numeric_colname]) == "numeric") {
        verbose(v, paste("  difexp: Checked. ", numeric_colname, " is numeric. \n", sep = ""))
      } else {
        stop(paste("difexp: ", numeric_colname, " is not numeric."))
      }
    }
  }
  ## "symbol" should be character
  if ("symbol" %in% colnames(difexp)) {
    if (class(difexp$symbol) == "character" | class(difexp$symbol) == "factor") {
      difexp$symbol <- as.character(difexp$symbol)
      verbose(v, "  difexp: Checked. symbol is character. \n")
    } else {
      stop("difexp: signature symbol is not character.")
    }
  }
  verbose(v, "  [Success] difexp matrix is valid. \n")
  return(difexp)
}
