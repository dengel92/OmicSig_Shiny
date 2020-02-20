check_metadata <- function(metadata) {
  # metadata should be a list with required attributes
  if (class(metadata)[1] == "OmicSignature") {
    metadata <- metadata$metadata
  }

  if (class(metadata) == "list") {
    print(paste("Checked. Metadata is a list."))
  }
  else {
    return(paste("Error: Metadata not found in OmicSignature object, or metadata is not a list. "))
  }

  metadata_required <- c("organism", "platform", "type")
  metadata_missing <- setdiff(metadata_required, names(metadata))
  print(paste("Input metadata have to contain columns: ", paste(metadata_required, collapse = ", "), " .", sep = ""))

  if (length(metadata_missing) == 0) {
    print(paste("Checked. Metadata contains all the essential attributes."))
    return(TRUE)
  } else {
    return(paste("Warning: Metadata does not contain attribute(s): ",
      paste(metadata_missing, collapse = ", "),
      ". This can cause problem when retriving data.",
      sep = ""
    ))
  }
}

#----------------------------------------------
# we need the information of "signature_type" in the metadata
# so, the input need to either be OmicSig Obj, or need to specify signature type
check_signatures <- function(omic.obj, signature_type = NULL) {

  # read the signature, and check if it is a list:
  if (class(omic.obj)[1] == "OmicSignature") {
    signatures <- omic.obj$signatures
    signature_type <- omic.obj$metadata$type
  }
  else if (class(omic.obj)[1] == "list") {
    signatures <- omic.obj
    remove(omic.obj)
  }
  else {
    return(paste("Error: Signature not found in OmicSignature object, or signature is not a list. ", sep = ""))
  }

  # signatures should be of proper length, according to the "type":
  if (signature_type == "multi-directional") {
    sample_number <- omic.obj$metadata$sample_number
  }

  # bi-directional signature:
  # note: each signature type need to be in "else if",
  # because if none of the type meet the criteria required, we need an "else" to output error.
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
      if ((length(signatures$Up_Regulated_Symbol) + length(signatures$Dn_Regulated_Symbol)) == 0) {
        return(paste("Warning: No signatures saved in the Omic Object."))
      } else if ((length(signatures$Up_Regulated_Symbol) * length(signatures$Dn_Regulated_Symbol)) == 0) {
        print(paste("Warning: Only one of the bi-directional signature is found."))
      }
    } else {
      print(paste("Warning: Signature need to be named as Up_Regulated_Symbol, Dn_Regulated_Symbol, Up_Regulated_Score, Dn_Regulated_Score. This can cause problem when you trying to save the OmicSig object."))
    }
  }

  # uni-directional signature:
  else if (signature_type == "uni-directional" && length(signatures) == 2) {
    print(paste("Checked. Signature is uni-directional with length of 2. "))
    if ("Signature_Symbol" %in% names(signatures) && "Signature_Score" %in% names(signatures)) {
      print(paste("Checked. Signature contains *Signature_Symbol* and *Signature_Score* elements. "))
    } else {
      print(paste("Warning: Signature need to be named as Signature_Symbol, Signature_Score. This can cause problem when you trying to save the OmicSig object."))
    }
  }

  # multi-directional signature:
  else if (signature_type == "multi-directional" && length(signatures) == sample_number) {
    print(paste("Checked. Signature is multi-directional with", sample_number, "samples.", sep = " "))
  }

  # if none of the signature type is meet:
  else {
    return(paste("Error: Signature is not a list with proper length. Or the sample_number is not specified in a multi-directional signature."))
  }

  return(TRUE)
  # note: except erroes, all the output in check_signature are "print" now
  #       have not "locked" the return yet in case in the future we need to check more things
}

#----------------------------------------------
# check_difexp() function supports input Obj or dataframe
check_difexp <- function(omic.obj) {
  # should be data frame, contain columns: "Probe_ID","symbol","logFC","AveExpr","Score","P.Value","fdr"

  if (class(omic.obj)[1] == "OmicSignature") {
    difexp <- omic.obj$difexp
  }
  else if (class(omic.obj)[1] == "data.frame") {
    difexp <- omic.obj
    remove(omic.obj)
  }
  else {
    return(paste("Error: Input is not a OmicSignature object or dataframe. "))
  }

  # check if it's empty:
  if (nrow(difexp) == 0) {
    return(paste("Error: There is no Differential Express Matrix (lv1 data) available. "))
  } else {
    print(paste("Checked. Input Differential Express Matrix (lv1 data) is a data frame. "))
  }

  # check column names:
  # note: if there are additional columns besides the required columns, there will Not be any warning messages
  # 2020/02/05: Vanessa currently working on develop read, write json function to preserve the optional columns
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
  # "symbol" should be character
  if ("symbol" %in% colnames(difexp)) {
    if (class(difexp$symbol) == "character" | class(difexp$symbol) == "factor") {
      print(paste("Checked. Symbol is character.", sep = ""))
    } else {
      print(paste("Warning: Symbol is not character."))
    }
  }

  return(TRUE)
  # note: except errors, all the output in check_difexp are "print" now
  #       have not "locked" the return yet in case in the future we need to check more things
}
