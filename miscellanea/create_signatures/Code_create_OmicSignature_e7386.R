source("OmicSignature/OmicSignature.R")
source("OmicSignature/check_functions/Function_objCheck.R")
source("OmicSignature/check_functions/Function_json.R")

{
metadata <- list(
    "organism" = "human",
    "tissue" = "cell",
    "cell_lines" = "hsc3",
    "phenotype" = "e7386",
    "type" = "bi-directional",
    "platform" = "Brainarray",
    "fdr_cutoff" = 0.005,
    "logfc_cutoff" = 1,
    "keywords" = c("cancer", "pertubations")
)
  
sample_name = "hsc3_e7386"
  
diffexp <- readRDS("miscellanea/files_diffmatrix/e7386_hsc3_hnsc_diffanal.RDS")
diffexp$logfc <- log2(abs(diffexp$fold.change))*sign(diffexp$fold.change)
colnames(diffexp) <- replace(colnames(diffexp), which(colnames(diffexp) == "Brainarray.probeset.ID"), "Probe_ID")
colnames(diffexp) <- replace(colnames(diffexp), which(colnames(diffexp) == "t"), "score")
colnames(diffexp) <- replace(colnames(diffexp), which(colnames(diffexp) == "FDR.q.filtered"), "fdr")
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

Omic.obj.hsc <- OmicSignature$new(metadata, signatures, diffexp)

}
check_difexp(Omic.obj)
check_difexp(Omic.obj)
write.table(Omic.obj.hsc$signatures, file = paste("miscellanea/signatures/", sample_name, "_lv2.txt", sep = ""), col.names = T, row.names = F, quote = F)
write_json(Omic.obj.hsc, file = paste("miscellanea/signatures/", sample_name, "_obj.json", sep = ""))

