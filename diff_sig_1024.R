library(Biobase)
library(DESeq2)
library(dplyr)
library(limma)

dat=readRDS("ahr_brca.rds")
dim(dat)
View(head(fData(dat)))
View(pData(dat))
expr_MDA=exprs(dat)
gene_symbol=fData(dat)[rownames(expr_MDA),"hgnc_symbol"]
# 2 cell type: MDA, Sum149. 
# 3 treatment: ctrl, KOAhR, KOCYP1. 
# so, for each cell type, do ctrl vs KOAhR and ctrl vs KOCYP1
# take the t-statistic of all the rows, rank the genes according to t-statistic

# design_mat_MDA_AhR=model.matrix(~experiment, data = pData(dat)[c(1:6),] )
# design matrix sets the "ctrl" as the treatment, so higher value means it's higher in "ctrl"
# manuly create the design matrix instead

#### MDA_AhR ####
design_mat_MDA_AhR=data.frame(cbind(rep(1,6),c(0,0,0,1,1,1)))
colnames(design_mat_MDA_AhR)=c("ctrl","AhR")
rownames(design_mat_MDA_AhR)=rownames(pData(dat))[c(1:6)]

fit_MDA_AhR <- lmFit(expr_MDA[,1:6],design_mat_MDA_AhR)
head(fit_MDA_AhR$coefficients)

fit_MDA_AhR=eBayes(fit_MDA_AhR)
toptable_MDA_AhR=topTable(fit_MDA_AhR,coef="AhR",adjust="BH",sort.by="t",number=nrow(expr_MDA)) 
# "BH" stands for FDR correction of p-val
# ranked by t-score
head(toptable_MDA_AhR)

# the top 10 sig genes:
siggenes_MDA_AhR=fData(dat)[rownames(toptable_MDA_AhR)[1:10],"hgnc_symbol"]
siggenes_MDA_AhR

#### MDA_CYP ####
design_mat_MDA_CYP=data.frame(cbind(rep(1,6),c(0,0,0,1,1,1)))
colnames(design_mat_MDA_CYP)=c("ctrl","CYP")
rownames(design_mat_MDA_CYP)=rownames(pData(dat))[c(1:3,7:9)]

fit_MDA_CYP <- lmFit(expr_MDA[,c(1:3,7:9)],design_mat_MDA_CYP)
head(fit_MDA_CYP$coefficients)

fit_MDA_CYP=eBayes(fit_MDA_CYP)
toptable_MDA_CYP=topTable(fit_MDA_CYP,coef="CYP",adjust="BH",sort.by="t",number=nrow(expr_MDA))
  # if don't specify number, will only give top 10
head(toptable_MDA_CYP)

# the top 10 sig genes:
siggenes_MDA_CYP=fData(dat)[rownames(toptable_MDA_CYP)[1:10],"hgnc_symbol"]
siggenes_MDA_CYP

#### ####
#dat_taz=readRDS("taz_yap_dpagt1_hnsc.RDS")
#View(pData(dat_taz)) # ctrl vs Taz, ctrl vs Yap

#### sig Obj ####
# following is copied from SigObj.R #
sig <- R6Class("sig", list(
  metadata   = NULL,
  signatures = NULL,
  mine_difexp     = NULL,
  initialize = function(metadata, signatures, mine_difexp=NULL) {
    # Methods can access the methods and fields of the current object via self$
    self$metadata   <- metadata
    self$signatures <- signatures
    self$mine_difexp     <- mine_difexp
  },
  print = function(...) {
    cat("Signature Object: \n")
    cat("  Metadata: \n")
    sh <- mapply(function(k, v) cat("   ", k, "=", v, "\n"), names(self$metadata), self$metadata) 
    cat("  Signatures: \n")
    sh <- mapply(function(k, v) cat("    ", k, " (", length(v), ")", "\n", sep=""), names(self$signatures), self$signatures)
    cat("  Differential Expression Data: \n")
    cat("    ", nrow(self$mine_difexp), " x ", ncol(self$mine_difexp), "\n", sep="")
    invisible(self)
  },
  extract.signature = function(conditions) {
    v <- rlang::parse_exprs(conditions)
    self$mine_difexp %>%
      dplyr::filter(!!!v) %>%
      dplyr::pull(symbol) 
  }
))

# copy from SigObj.R end #

# specify some metadata to describe the experiment:
mine_metadata <- list("organism"  = "human",
                 "tissue"    = "cell",
                 "phenotype" = "my_phynotype",
                 "type"      = "my_type",
                 "keywords"  = c("fruit", "apple", "pineapple"))

# the diff matrix:
  # use MDA cell AhR_KO as an example here
mine_difexp=toptable_MDA_AhR

# add gene symbols to the matrix (dataframe):
mine_difexp=cbind(mine_difexp, gene_symbol)

# change column name to default names for sig object:
colnames(mine_difexp)[which(colnames(mine_difexp)=="gene_symbol")]="symbol"
colnames(mine_difexp)[which(colnames(mine_difexp)=="adj.P.Val")]="fdr"

# signatures, i.e. lv3 data:
mine_signatures <- list("Up Regulated" = filter(mine_difexp, t > 0 & fdr < 0.001) %>% pull(symbol),
                        "Dn Regulated" = filter(mine_difexp, t < 0 & fdr < 0.001) %>% pull(symbol))

# store the data into sig obj:
mine_sigobj <- sig$new(mine_metadata, mine_signatures, mine_difexp)
print(mine_sigobj)

# extract signatures with certain criteria from sig obj:
  # returns factor string
mine_sigobj$extract.signature('logFC > 1') 
mine_sigobj$extract.signature('logFC < -1; fdr < 0.0001')

mine_sigobj$extract.signature('abs(logFC) > 1; fdr < 0.0001')
mine_sigobj$extract.signature('abs(logFC) > 1; fdr < 0.0001; t>10')

