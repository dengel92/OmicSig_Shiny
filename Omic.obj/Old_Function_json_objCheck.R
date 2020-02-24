# old functions when signatures are stored as list in OmicSig Obj

write_obj <- function(omic.obj, file) {
  write_lv2lv3 <- list(
    "Up_Regulated_Symbol" = omic.obj$signatures$Up_Regulated_Symbol,
    "Dn_Regulated_Symbol" = omic.obj$signatures$Dn_Regulated_Symbol,
    "Up_Regulated_Score" = omic.obj$signatures$Up_Regulated_Score,
    "Dn_Regulated_Score" = omic.obj$signatures$Dn_Regulated_Score
  )
  metadata_length <- length(omic.obj$metadata)
  write_json_obj <- rjson::toJSON(c(omic.obj$metadata,
    "metadata_length" = metadata_length,
    write_lv2lv3,
    list("lv1_colnames" = colnames(omic.obj$difexp)),
    omic.obj$difexp
  ))
  write(write_json_obj, file)
  return("finished")
}

read_json <- function(filename) {
  read_json <- rjson::fromJSON(file = filename)
  lv1_colnames <- read_json$lv1_colnames
  read_lv1 <- as.data.frame(dplyr::bind_rows(read_json[c(lv1_colnames)]))
  read_lv2lv3 <- list(
    "Up_Regulated_Symbol" = read_json$Up_Regulated_Symbol,
    "Dn_Regulated_Symbol" = read_json$Dn_Regulated_Symbol,
    "Up_Regulated_Score" = read_json$Up_Regulated_Score,
    "Dn_Regulated_Score" = read_json$Dn_Regulated_Score
  )
  read_meta <- read_json[c(1:read_json$metadata_length)]

  read_sig.obj <- OmicSignature$new(read_meta, read_lv2lv3, read_lv1)
  return(read_sig.obj)
}

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
