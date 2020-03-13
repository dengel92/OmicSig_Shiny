Check_metadata <- function(metadata) {
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

  metadata_required <- c("organism", "platform", "type", "phenotype")
  metadata_missing <- setdiff(metadata_required, names(metadata))
  print(paste("Input metadata have to contain columns: ", paste(metadata_required, collapse = ", "), " .", sep = ""))

  if (length(metadata_missing) == 0) {
    print(paste("Checked. Metadata contains all the essential attributes."))
    return("finished.")
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
# so, the input need to either be OmicSignature Obj, or need to specify signature type
check_signatures <- function(omic.obj, signature_type = NULL, sample_number = 0) {
  # read the signature, and check if it is a dataframe:
  if (class(omic.obj)[1] == "OmicSignature") {
    signatures <- omic.obj$signatures
    signature_type <- omic.obj$metadata$type
    if (signature_type == "multi-directional") {
      sample_number <- omic.obj$metadata$sample_number
    }
  }
  else if (class(omic.obj)[1] == "data.frame") {
    signatures <- omic.obj
    remove(omic.obj)
  } else {
    return(paste("Error: Signature not found in OmicSignature object, or signature is not a dataframe. ", sep = ""))
  }
  if (nrow(signatures) == 0) {
    return(paste("Error: Signature is empty."))
  }

  # check if signature_symbol and signature_score (lv2 or lv3) exists:
  if (!c("signature_symbol") %in% colnames(signatures)) {
    return(paste("Error: signature dataframe does not contain \"signature_symbol\" column."))
  }
  if (c("signature_score") %in% colnames(signatures)) {
    print(paste("This signature contains lv2 data."))
  } else {
    print(paste("This signature only contains lv3 data."))
  }

  # check if the direction match with signature type:
  if (is.null(signature_type)) {
    return(paste("Error: signature type not specified."))
  }
  # each signature type need to be "else if". because if none of the type meet the criteria required, we need an "else" to output error.
  # bi-directional signature:
  if (signature_type == "bi-directional") {
    summary_direction <- summary(signatures$signature_direction)
    if (isTRUE(all.equal(c("Dn", "Up"), names(summary(signatures$signature_direction))))) {
      print("Checked. signature is bi-directional with Dn and Up directions.")
    } else if (names(summary_direction) == c("Dn") | names(summary_direction) == c("Up")) {
      print("Checked. signature is bi-directional, but only one direction is found.")
    } else {
      return(paste("Error: bi-directional signature should be marked with \"Dn\" and \"Up\"."))
    }
  }

  # uni-directional signature:
  else if (signature_type == "uni-directional") {
    if (!"signature_direction" %in% colnames(signatures)) {
      print(paste("Checked. signature is uni-directional. "))
    } else {
      print(paste("Warning: signature is specified as uni-directional but additional direction information found."))
    }
  }

  # multi-directional signature:
  else if (signature_type == "multi-directional") {
    summary_direction <- summary(signatures$signature_direction)
    if (length(summary_direction) == sample_number) {
      print(paste("Checked. signature is multi-directional with", sample_number, "samples.", sep = " "))
    } else if (length(summary_direction) < sample_number) {
      print(paste("Warning: signature is multi-directional with", sample_number, "samples, but some samples do not have signatures."))
    } else {
      return(paste("Error: signature is specified as multi-directional, but sample number is invalid."))
    }
  }

  # if none of the signature type is meet:
  else {
    return(paste("Error: Signature information invalid."))
  }

  return("finished.")
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

  return("finished.")
  # note: except errors, all the output in check_difexp are "print" now
  #       have not "locked" the return yet in case in the future we need to check more things
}
