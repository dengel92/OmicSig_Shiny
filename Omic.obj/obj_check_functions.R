check_metadata <- function(omic.obj) {
  metadata <- omic.obj$metadata
  # should be a list
  if (class(metadata) == "list") {
    print(paste("Checked. metadata is a list."))
  } else {
    return(paste("Error: metadata should be a list.", sep = ""))
  }
  metadata_missing <- setdiff(c("organism", "platform", "type"), names(metadata))
  if (length(metadata_missing) == 0) {
    print(paste("Checked. Metadata contains all the essential attributes."))
    return(TRUE)
  } else {
    return(paste("Warning: Metadata does not contain attribute(s): ", paste(metadata_missing, collapse = ", "), ". This can cause problem when retriving data.", sep = ""))
  }
}


check_signatures <- function(omic.obj) {
  signatures <- omic.obj$signatures
  signature_type <- omic.obj$metadata$type
  if (signature_type == "multi-directional") {
    sample_number <- omic.obj$metadata$sample_number
  }

  # should be a list of proper length:
  if (class(signatures) == "list") {
    if (signature_type == "bi-directional" && length(signatures) == 4) {
      print(paste("Checked. signature is bi-directional with length of 4. "))
      if ("Up_Regulated_Symbol" %in% names(signatures) &&
        "Dn_Regulated_Symbol" %in% names(signatures) &&
        "Up_Regulated_Score" %in% names(signatures) &&
        "Dn_Regulated_Symbol" %in% names(signatures)) {
        print(paste("Checked. signature contains *Up Regulated* and *Dn Regulated* elements. "))
        signatures$Up_Regulated_Symbol <- as.character(signatures$Up_Regulated_Symbol)
        signatures$Dn_Regulated_Symbol <- as.character(signatures$Dn_Regulated_Symbol)
        signatures$Up_Regulated_Score <- as.numeric(signatures$Up_Regulated_Score)
        signatures$Dn_Regulated_Score <- as.numeric(signatures$Dn_Regulated_Score)
        if (length(signatures$Up_Regulated_Symbol) * length(signatures$Up_Regulated_Symbol) == 0) {
          return(paste("Warning: No signatures saved in the Omic Object."))
        }
      } else {
        print(paste("Warning: Signature need to be named as Up_Regulated_Symbol, Dn_Regulated_Symbol, Up_Regulated_Score, Dn_Regulated_Score. This can cause problem when you trying to save the OmicSig object."))
      }
    } else if (signature_type == "uni-directional" && length(signatures) == 2) {
      print(paste("Checked. Signature is uni-directional with length of 2. "))
      if ("Signature_Symbol" %in% names(signatures) && "Signature_Score" %in% names(signatures)) {
        print(paste("Checked. Signature contains *Signature_Symbol* and *Signature_Score* elements. "))
      } else {
        print(paste("Warning: Signature need to be named as Signature_Symbol, Signature_Score. This can cause problem when you trying to save the OmicSig object."))
      }
    } else if (signature_type == "multi-directional" && length(signatures) == sample_number) {
      print(paste("Checked. Signature is multi-directional with", sample_number, "samples.", sep = " "))
    } else {
      return(paste(
        "Error: Signature is not a list with proper length. Or the sample_number is not specified in multi-directional signature."
      ))
    }
  } else {
    return(paste("Error: Signature is not a list or signature not found. "))
  }
  return(TRUE)
}


check_difexp <- function(omic.obj) {
  difexp <- omic.obj$difexp
  # should be data frame, contain columns: "Probe_ID","symbol","logFC","AveExpr","Score","P.Value","fdr"

  # check if it's data.frame:
  if (class(difexp) == "matrix") {
    difexp <- as.data.frame(difexp)
  }
  if (class(difexp) == "data.frame") {
    # check if it is NULL:
    if (nrow(difexp) == 0) {
      return(paste("Warning: There is no Differential Express Matrix (lv1 data) available. "))
    } else {
      print(paste("Checked. Input Differential Express Matrix (lv1 data) is a data frame. "))
    }
  } else {
    return(paste("Error: Input Differential Express Matrix (lv1 data) is not a data.frame or matrix. "))
    # use "return" instead of "print" here, so if this criteria is not met, the following steps will not run
  }

  # check column names:
  # note: if there are additional columns besides the required columns, there will not be any warning messages. However, when writing the obj into json txt file, those additional columns will be lost.
  difexp_colname_required <- c("Probe_ID", "symbol", "logFC", "AveExpr", "Score", "P.Value", "fdr")
  difexp_colname_missing <- setdiff(difexp_colname_required, colnames(difexp))
  print(paste("Input Differential Express Matrix (lv1 data) have to contain columns: ", paste(difexp_colname_required, collapse = ", "), " .", sep = ""))
  if (length(difexp_colname_missing) == 0) {
    print(paste("Checked. Input Differential Express Matrix (lv1 data) contain all the essential columns. "))
  } else {
    print(paste("Warning: Input Differential Express Matrix (lv1 data) does not contain column(s): ", paste(difexp_colname_missing, collapse = ", "), ". This can cause problem when retriving data.", sep = ""))
  }

  # check column type:
  # "logFC","AveExpr","Score","P.Value","fdr" should be numerical
  for (numeric_colname in c("logFC", "AveExpr", "Score", "P.Value", "fdr")) {
    if (numeric_colname %in% colnames(difexp)) {
      if (class(difexp[, numeric_colname]) == "numeric") {
        print(paste("Checked. ", numeric_colname, " is numeric.", sep = ""))
      } else {
        print(paste("Warning: ", numeric_colname, " is not numeric."))
      }
    }
  }

  # "symbol" should be character. if it's factor, then change it into character
  if ("symbol" %in% colnames(difexp)) {
    if (class(difexp$symbol) == "factor") {
      difexp$symbol <- as.character(difexp$symbol)
    }
    if (class(difexp$symbol) == "character") {
      print(paste("Checked. Symbol is character.", sep = ""))
    } else {
      print(paste("Warning: Symbol is not character."))
    }
  }
  return(TRUE)
}
