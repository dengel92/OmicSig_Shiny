source("OmicSignature/OmicSignature.R")
source("OmicSignature/check_functions/Function_objCheck.R")
source("OmicSignature/check_functions/Function_json.R")

sample_name <- "Sum149_CYP1B1"
{
  metadata <- list(
    "organism" = "human",
    "tissue" = "cell",
    "cell_lines" = "Sum149",
    "phenotype" = "CYP1B1_KO",
    "type" = "bi-directional",
    "platform" = "illumina",
    "fdr_cutoff" = 0.005,
    "logFC_cutoff" = 1,
    "keywords" = c("cancer", "KO", "pertubations")
  )

  difexp <- read.table(paste("miscellanea/files_diffmatrix/diffmatrix_", sample_name, ".txt", sep = ""), header = TRUE, stringsAsFactors = FALSE)
  # change the column names to the standard names as required in OmicSig Obj lv1:
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "t"), "Score")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "logfc"), "logFC")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "gene_symbol"), "symbol")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "adj.P.Val"), "fdr")
  difexp$Probe_ID <- rownames(difexp)

  # old code for signature as a list:
  # signatures <- list( # a bi-directional example
  #  "Up_Regulated_Symbol" = filter(difexp, Score > 0 & fdr < metadata$fdr_cutoff) %>% pull(symbol),
  #  "Dn_Regulated_Symbol" = filter(difexp, Score < 0 & fdr < metadata$fdr_cutoff) %>% pull(symbol),
  #  "Up_Regulated_Score" = filter(difexp, Score > 0 & fdr < metadata$fdr_cutoff) %>% pull(Score),
  #  "Dn_Regulated_Score" = filter(difexp, Score < 0 & fdr < metadata$fdr_cutoff) %>% pull(Score)
  # )

  # Signatures: # a bi-directional example
  temp_upsig <- cbind(
    filter(difexp, Score > 0 & abs(logFC) > metadata$logFC_cutoff & fdr < metadata$fdr_cutoff) %>% pull(symbol),
    filter(difexp, Score > 0 & abs(logFC) > metadata$logFC_cutoff & fdr < metadata$fdr_cutoff) %>% pull(Score),
    "Up"
  )
  temp_upsig
  temp_dnsig <- cbind(
    filter(difexp, Score < 0 & abs(logFC) > metadata$logFC_cutoff & fdr < metadata$fdr_cutoff) %>% pull(symbol),
    filter(difexp, Score < 0 & abs(logFC) > metadata$logFC_cutoff & fdr < metadata$fdr_cutoff) %>% pull(Score),
    "Dn"
  )
  temp_dnsig

  signatures <- data.frame(rbind(temp_upsig, temp_dnsig))
  colnames(signatures) <- c("signature_symbol", "signature_score", "signature_direction")
  # signatures$signature_direction <- as.factor(signatures$signature_direction)
  signatures <- signatures[order(signatures$signature_direction, decreasing = F), ]
  # save the signatures according to the order of the direction, to make read and write convenient
  remove(temp_upsig, temp_dnsig)

  # Object:
  Omic.obj <- OmicSignature$new(metadata, signatures, difexp)
  print(Omic.obj)
  remove(metadata, signatures, difexp)
}

# check-functions:
check_metadata(Omic.obj)
check_difexp(Omic.obj$difexp)
check_signature(Omic.obj$signatures, signature_type = "bi-directional")
check_signature(Omic.obj$signatures, signature_type = "uni-directional")

# write signaure (lv2/lv3) txt file:
write.table(Omic.obj$signatures, file = paste("OmicSignature/signatures/", sample_name, "_lv2.txt", sep = ""), col.names = T, row.names = F, quote = F)
write.table(Omic.obj$signatures[, c("signature_symbol", "signature_direction")], file = paste("OmicSignature/signatures/", sample_name, "_lv3.txt", sep = ""), col.names = T, row.names = F, quote = F)

# write Omic.obj json files:
write_json(Omic.obj, file = paste("OmicSignature/signatures/", sample_name, "_obj.json", sep = ""))


# extract more signature function:
Omic.obj$extract.signature("logFC > 0.5")
Omic.obj$extract.signature("logFC < -0.5; fdr < 0.001")
Omic.obj$extract.signature("abs(logFC) > 0.5; fdr < 0.001")
Omic.obj$signatures
