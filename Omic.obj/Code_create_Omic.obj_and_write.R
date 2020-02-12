source("Omic.obj/OmicObj.R")
source("Omic.obj/Function_objCheck.R")
source("Omic.obj/Function_write_sig.R")
source("Omic.obj/Function_json.R")

sample_name <- "MDA_CYP1B1"
{
  metadata <- list(
    "organism" = "human",
    "tissue" = "cell",
    "cell_lines" = "MDA",
    "phenotype" = "CYP1B1_KO",
    "type" = "bi-directional",
    "platform" = "illumina",
    "fdr_cutoff" = 0.01,
    "keywords" = c("cancer", "KO", "pertubations")
  )

  difexp <- read.table(paste("Omic.obj/diff_exp_analysis/diffmatrix_", sample_name, ".txt", sep = ""), header = TRUE, stringsAsFactors = FALSE)
  # change the column names to the standard names as required in OmicSig Obj lv1:
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "t"), "Score")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "logfc"), "logFC")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "gene_symbol"), "symbol")
  colnames(difexp) <- replace(colnames(difexp), which(colnames(difexp) == "adj.P.Val"), "fdr")
  difexp$Probe_ID <- rownames(difexp)

  signatures <- list( # a bi-directional example
    "Up_Regulated_Symbol" = filter(difexp, Score > 0 & fdr < metadata$fdr_cutoff) %>% pull(symbol),
    "Dn_Regulated_Symbol" = filter(difexp, Score < 0 & fdr < metadata$fdr_cutoff) %>% pull(symbol),
    "Up_Regulated_Score" = filter(difexp, Score > 0 & fdr < metadata$fdr_cutoff) %>% pull(Score),
    "Dn_Regulated_Score" = filter(difexp, Score < 0 & fdr < metadata$fdr_cutoff) %>% pull(Score)
  )

  # Object:
  Omic.obj <- OmicCollection$new(metadata, signatures, difexp)
  print(Omic.obj)
  remove(metadata, signatures, difexp)
}

# check-functions:
check_metadata(Omic.obj)
check_difexp(Omic.obj$difexp)
check_signatures(Omic.obj)
check_signatures(Omic.obj$signatures, signature_type = "bi-directional")

# write lv2, lv3 txt files:
write_sig_bi(Omic.obj, name = sample_name)

# write Omic.obj json files:
write_obj(Omic.obj, file = paste("Omic.obj/signatures/", sample_name, "_obj.txt", sep = ""))


# extract more signature function:
Omic.obj$extract.signature("logFC > 0.5")
Omic.obj$extract.signature("logFC < -0.5; fdr < 0.001")
Omic.obj$extract.signature("abs(logFC) > 0.5; fdr < 0.001")
Omic.obj$signatures
