source("OmicSignature/OmicSignature.R")
source("OmicSignature/check_functions/Function_objCheck.R")
source("OmicSignature/check_functions/Function_json.R")

{
  sample_name <- "cal27_e7386"
    
  metadata <- list(
    "signature_name" = sample_name,
    "organism" = "human",
    "tissue" = "cell",
    "cell_lines" = "Cal27",
    "phenotype" = "e7386",
    "type" = "bi-directional",
    "platform" = "Brainarray",
    "fdr_cutoff" = 0.005,
    "logfc_cutoff" = 1,
    "keywords" = c("cancer", "pertubations")
  )

  diffexp <- readRDS("miscellanea/files_diffmatrix/e7386_cal27_hnsc_diffanal.RDS")
  diffexp$logfc <- log2(abs(diffexp$fold.change)) * sign(diffexp$fold.change)
  colnames(diffexp) <- replace(colnames(diffexp), which(colnames(diffexp) == "Brainarray.probeset.ID"), "Probe_ID")
  colnames(diffexp) <- replace(colnames(diffexp), which(colnames(diffexp) == "t"), "score")
  colnames(diffexp) <- replace(colnames(diffexp), which(colnames(diffexp) == "FDR.q.filtered"), "fdr")
  colnames(diffexp) <- replace(colnames(diffexp), which(colnames(diffexp) == "p"), "p.value")
  colnames(diffexp) <- tolower(colnames(diffexp))

  temp_upsig <- cbind(
    filter(diffexp, score > 0 & abs(logfc) > metadata$logfc_cutoff & fdr < metadata$fdr_cutoff) %>% pull(symbol),
    filter(diffexp, score > 0 & abs(logfc) > metadata$logfc_cutoff & fdr < metadata$fdr_cutoff) %>% pull(score),
    "up"
  )
  temp_upsig
  temp_dnsig <- cbind(
    filter(diffexp, score < 0 & abs(logfc) > metadata$logfc_cutoff & fdr < metadata$fdr_cutoff) %>% pull(symbol),
    filter(diffexp, score < 0 & abs(logfc) > metadata$logfc_cutoff & fdr < metadata$fdr_cutoff) %>% pull(score),
    "dn"
  )
  temp_dnsig

  signatures <- data.frame(rbind(temp_upsig, temp_dnsig))
  colnames(signatures) <- c("signature_symbol", "signature_score", "signature_direction")
  signatures <- signatures[order(signatures$signature_direction, decreasing = F), ]

  Omic.obj.cal27 <- OmicSignature$new(metadata, signatures, diffexp)
  Omic.obj.cal27$extract.signature("logfc < -1")
}

write.table(Omic.obj.cal27$signatures, file = paste("miscellanea/signatures/", sample_name, "_lv2.txt", sep = ""), col.names = T, row.names = F, quote = F)
write_json(Omic.obj.cal27, file = paste("miscellanea/signatures/", sample_name, "_obj.json", sep = ""))

## ---------------------------
## read and write OmicSig Obj again to test if it works:
# Apr 7th 2020: checked after json function updated
# a=read_json(file = paste("miscellanea/signatures/", sample_name, "_obj.json", sep = ""))
# a$extract.signature("logfc < -1")
# all.equal(target=a,current=Omic.obj.hsc) # FALSE. rownames; number difference because of rounding;signature_symbol is character in a but previous factor
# write_json(a, file = paste("miscellanea/signatures/", sample_name, "_obj2.json", sep = ""))
# b=read_json(file = paste("miscellanea/signatures/", sample_name, "_obj2.json", sep = ""))
# b$extract.signature("logfc < -1")
# all.equal(target=b,current=a) # TRUE
