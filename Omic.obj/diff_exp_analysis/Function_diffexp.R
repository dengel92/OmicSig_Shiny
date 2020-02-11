# function for lm diff exp analysis with Six samples
# designed based on ahr_brca.rds data

diff_sixLm <- function(dat, ctrl_rows, trt_rows, fData_symbol = "hgnc_symbol", name = "newSample") {
  design_mat <- data.frame(cbind(rep(1, 6), c(0, 0, 0, 1, 1, 1)))
  colnames(design_mat) <- c("ctrl", "trt")
  rownames(design_mat) <- rownames(pData(dat))[c(ctrl_rows, trt_rows)]
  print(design_mat)

  fit_lm <- lmFit(exprs(dat)[, c(ctrl_rows, trt_rows)], design_mat)
  fit_lm <- eBayes(fit_lm)
  toptable_lm <- topTable(fit_lm, coef = "trt", adjust = "BH", sort.by = "t", number = nrow(exprs(dat)))
  toptable_lm <- cbind(toptable_lm,
    "gene_symbol" = fData(dat)[rownames(toptable_lm), fData_symbol],
    "Probe_ID" = rownames(toptable_lm)
  )
  print(head(toptable_lm))
  write.table(toptable_lm, paste("diffmatrix_", name, ".txt", sep = ""), col.names = T, quote = F, row.names = T)
}

# output:
dat <- readRDS("ahr_brca.rds")
pData(dat)

diff_sixLm(dat, ctrl_rows = c(1:3), trt_rows = c(4:6), name = "MDA_AhR")
diff_sixLm(dat, ctrl_rows = c(1:3), trt_rows = c(7:9), name = "MDA_CYP1B1")
diff_sixLm(dat, ctrl_rows = c(10:12), trt_rows = c(13:15), name = "Sum149_AhR")
diff_sixLm(dat, ctrl_rows = c(10:12), trt_rows = c(16:18), name = "Sum149_CYP1B1")

diff_sixLm(dat, ctrl_rows = c(19:21), trt_rows = c(22:24), name = "Cal27_CB113")
diff_sixLm(dat, ctrl_rows = c(19:21), trt_rows = c(25:27), name = "Cal27_BaP")
diff_sixLm(dat, ctrl_rows = c(19:21), trt_rows = c(28:30), name = "Cal27_PYO")
diff_sixLm(dat, ctrl_rows = c(19:21), trt_rows = c(31:33), name = "Cal27_CH")

