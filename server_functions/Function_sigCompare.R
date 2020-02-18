sigCompare_two <- function(sig1, sig2, is.lv2 = TRUE, background_number = 22000) {
  # check: should be dataframe with symbol, (score), direction
  if (class(sig1) == "data.frame" & class(sig2) == "data.frame") {
    if (is.lv2 == TRUE) {
      colnames_required <- c("symbol", "score")
    } else {
      colnames_required <- c("symbol")
    }
    if (length(setdiff(colnames_required, colnames(sig1))) != 0 | length(setdiff(colnames_required, colnames(sig2))) != 0) {
      return(paste("Error: input signatures are dataframes but do not contain required columns."))
    }
    sig1_score <- sig1$score
    sig2_score <- sig2$score
    sig1_symbol <- sig1$symbol
    sig2_symbol <- sig2$symbol
    remove(sig1, sig2)
  } else if (class(sig1) == "character" & class(sig2) == "character") {
    if (is.lv2 == TRUE) {
      return(paste("Error: for lv2 data, input signatures should be dataframe."))
    }
    sig1_symbol <- sig1
    sig2_symbol <- sig2
    remove(sig1, sig2)
  }
  else {
    return(paste("Error: input signatures are not valid."))
  }

  output <- list()

  # comparison 1: compare name + Venn diagram
  sig_both <- intersect(sig1_symbol, sig2_symbol)
  Venn <- VennDiagram::draw.pairwise.venn(
    area1 = length(sig1_symbol), area2 = length(sig2_symbol), cross.area = length(sig_both),
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
  m <- length(sig1_symbol)
  n <- background_number - length(sig1_symbol)
  k <- length(sig2_symbol)
  hyper_pval <- phyper(q = q, m = m, n = n, k = k, lower.tail = FALSE)

  output[[1]] <- Venn
  output[[2]] <- setdiff(sig1_symbol, sig2_symbol)
  output[[3]] <- setdiff(sig2_symbol, sig1_symbol)
  output[[4]] <- sig_both
  output[[5]] <- hyper_pval
  output[[6]] <- sig1_symbol
  output[[7]] <- sig2_symbol
  names(output) <- c("Venn", "only_sig1", "only_sig2", "sig_both", "hyper_p.value", "sig1_symbol", "sig2_symbol")
  return(output)
}

# test the function:
# sig1 <- read.table("server_functions/example_sigCompare_1.txt", header = T)
# sig2 <- read.table("server_functions/example_sigCompare_2.txt", header = T)
# sigCompare_two(sig1, sig2, is.lv2 = T)
