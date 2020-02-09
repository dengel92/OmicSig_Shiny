sigCompare_two <- function(sig1, sig2, is.lv2 = TRUE, background_number = 22000) {
  # check: should be dataframe with symbol, (score), direction
  if (class(sig1) == "data.frame" & class(sig2) == "data.frame") {
    if (is.lv2 == TRUE) {
      colnames_required <- c("symbol", "score")
    } else {
      colnames_required <- c("symbol")
    }
    if (length(setdiff(colnames_required, colnames(sig1))) != 0 | length(setdiff(colnames_required, colnames(sig2))) != 0) {
      return(paste("Error: input signature(s) does not contain required columns."))
    }
  } else {
    return(paste("Error: input signature(s) is not a dataframe."))
  }

  output <- list()

  # comparison 1: compare name + Venn diagram
  sig_both <- intersect(sig1$symbol, sig2$symbol)
  Venn <- VennDiagram::draw.pairwise.venn(
    area1 = length(sig1$symbol), area2 = length(sig2$symbol), cross.area = length(sig_both),
    cex = rep(3, 3), cat.cex = rep(3, 2), fontfamily = rep("Palatino", 3), cat.fontfamily = rep("Palatino", 2),
    category = c("sig1", "sig2"), fill = c("darkseagreen1", "plum1"),
    scaled = T, cat.pos = rep(180, 2), cat.dist = c(0.05, 0.05)
  )

  # hypergeometric test:
  # background: N = m+n, total number of balls in the urn (e.g. 22k gene)
  # m: (total number of marked balls) number of signature 1
  # n: (total number of unmarked balls) number of (background - signature 1)
  # k: (a random selection of balls) number of signature 2
  # x: (number of marked balls within the selection) number in both signatures
  q <- length(sig_both)
  m <- length(sig1$symbol)
  n <- background_number - length(sig1$symbol)
  k <- length(sig2$symbol)
  hyper_pval <- phyper(q = q, m = m, n = n, k = k, lower.tail = FALSE)

  output[[1]] <- Venn
  output[[2]] <- setdiff(sig1$symbol, sig2$symbol)
  output[[3]] <- setdiff(sig2$symbol, sig1$symbol)
  output[[4]] <- hyper_pval
  names(output) <- c("Venn", "only_sig1", "only_sig2", "hyper_p.value")
  return(output)
}


# test the function:
sig1 <- read.table("server_functions/example_sigCompare_1.txt", header = T)
sig2 <- read.table("server_functions/example_sigCompare_2.txt", header = T)
sigCompare_two(sig1, sig2, is.lv2 = T)
