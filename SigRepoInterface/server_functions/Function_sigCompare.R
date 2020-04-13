#' @title Compare two lists of signatures
#' @description Compare two lists of signatures and return Venn diagram and hypergeometric test p-value for intersect.
#' updated 03/2020
#'
#' @param sig1 a dataframe or character for signature 1. In dataframe, column "symbol" is required with feature symbol inside. If it's level 2 data (symbol with score), columns "symbol" and "score" are required.
#' @param sig2 a dataframe or character for signature 2. Requirements are the same as sig1 above.
#' @param sig1_name string. the name of signature 1. used for display in Venn diagram.
#' @param sig2_name string. the name of signature 2. used for display in Venn diagram.
#' @param is.lv2 either TRUE or FALSE. If TRUE, the input sig1 and sig2 should be dataframe with columns "symbol" and "score" available. If FALSE, input sig1 and sig2 can be character, or dataframe with column "symbol" available.
#' @param background_number a numeric value indicating the total number of features. For example, in microarray, there are usually 22000 genes on a chip.
#' @return a list of the comparison result of the two signature. including Venn diagram, hypergeometric test p-value.
#'
#' @example
#' 
#' sig1 <- c("TNMD","DPM1","CFH","FUCA2","HECW1","LASP1","MAD1L1","KLHL13","FUCA2")
#' sig2 <- c("TNMD","FUCA2","C1orf112","FGR","MAD1L1","NFYA","STPG1","CFTR","LAS1L","ENPP4","SEMA3F","ANKIB1")
#' compare_result <- sigCompare_two(sig1, sig2, sig1_name = "sig1", sig2_name = "sig2", is.lv2 = FALSE, background_number = 22000)
#' compare_result$Venn
#' compare_result$sig_both
#'  
#' 

sigCompare_two <- function(sig1, sig2, sig1_name = "sig1", sig2_name = "sig2", is.lv2 = FALSE, background_number = 22000) {
  # check: should be dataframe with symbol, (score), direction; if it's lv3, can be character:
  if (class(sig1) == "data.frame" && class(sig2) == "data.frame") {
    # if they are dataframes, check if they have the required columns:
    if (is.lv2 == TRUE) {
      colnames_required <- c("symbol", "score")
    } else {
      colnames_required <- c("symbol")
    }
    if (length(setdiff(colnames_required, colnames(sig1))) != 0 | length(setdiff(colnames_required, colnames(sig2))) != 0) {
      return(paste("Error: input signatures are dataframes but do not contain required columns."))
    }

    # if the above checks are valid, save the score as a numeric variable. save symbol as the name of score:
    if (is.lv2 == TRUE) {
      sig1_score <- sig1$score
      sig2_score <- sig2$score
    } else {
      sig1_score <- numeric(length = nrow(sig1))
      sig2_score <- numeric(length = nrow(sig2))
    }
    names(sig1_score) <- sig1$symbol
    names(sig2_score) <- sig2$symbol
    remove(sig1, sig2)
  } else if (class(sig1) == "character" && class(sig2) == "character") {
    # else if they are characters:
    if (is.lv2 == TRUE) {
      return(paste("Error: for lv2 data, input signatures should be dataframe."))
    # use return() to stop the function from running. could have better ways to do this
    }
    # if is.lv2 = False, it is lv3:
    sig1_score <- numeric(length = length(sig1))
    sig2_score <- numeric(length = length(sig2))
    names(sig1_score) <- sig1
    names(sig2_score) <- sig2
    remove(sig1, sig2)
  }
  else {
      # else if they are neither df or character, it's not valid; 
      # currently assume that "one is character and the other is df" is not valid
    return(paste("Error: input signatures are not valid."))
  }
    
  # after the process above, score saved in sig1/2_score; if it's lv3, score simply be 0.
  # symbols are saved as the name no matter inputs are lv2 or lv3.

  # comparison 1: compare symbol (feature) + output Venn diagram
  sig_both <- intersect(names(sig1_score), names(sig2_score))
  Venn <- VennDiagram::draw.pairwise.venn(
    area1 = length(names(sig1_score)), area2 = length(names(sig2_score)), cross.area = length(sig_both),
    cex = rep(3, 3), cat.cex = rep(3, 2), fontfamily = rep("Palatino", 3), cat.fontfamily = rep("Palatino", 2),
    category = c(sig1_name, sig2_name), fill = c("darkseagreen1", "plum1"),
    scaled = T, cat.pos = rep(180, 2), cat.dist = c(0.05, 0.05)
  )
  # could be a better Venn using package in montilab github

  # comparison 2: hypergeometric test of the intercept
  # note: if p-val is very small, it means the the overlap of the two signatures is significantly "not random",
  #       which means the two signatures are similar
  # background: N = m+n (total number of balls in the urn) 22k gene
  # m: (total number of marked balls) number of signature 1
  # n: (total number of unmarked balls) number of (background - signature 1)
  # k: (a random selection of balls) number of signature 2
  # x: (number of marked balls within the selection) number in both signatures
  q <- length(sig_both)
  m <- length(names(sig1_score))
  n <- background_number - length(names(sig1_score))
  k <- length(names(sig2_score))
  hyper_pval <- phyper(q = q, m = m, n = n, k = k, lower.tail = FALSE)

  # comparison 3: rank test for lv2 (ranked signature)
  # as for Feb 2020: this test seems not very reliable. whether to keep it or not remain questionable.
  # note: if p-val is large, it means the score of the two signatures of the overlap are from "the same distribution",
  #       which means the two signatures are similar
  if (is.lv2 == TRUE) {
    rank_pval <- wilcox.test(sig1_score[sig_both], sig2_score[sig_both], paired = TRUE)$p.value
  } else {
    rank_pval <- NA
  }

  # output:
  output <- list()
  output[[1]] <- sig1_name # signature name, e.g. MDA_AhR_KO
  output[[2]] <- sig2_name
  output[[3]] <- names(sig1_score) # all symbols (features) in sig1
  output[[4]] <- names(sig2_score)
  output[[5]] <- setdiff(names(sig1_score), names(sig2_score))
  output[[6]] <- setdiff(names(sig2_score), names(sig1_score))
  output[[7]] <- sig_both
  output[[8]] <- Venn
  output[[9]] <- hyper_pval
  output[[10]] <- rank_pval
  names(output) <- c(
    "sig1_name", "sig2_name", "sig1_symbol", "sig2_symbol",
    "only_sig1", "only_sig2", "sig_both", "Venn",
    "hyper_p.value", "rank_p.value" ) 
  # note: the sequence of the output things do not matter, so long as the names specified are corresponded.
  #       because in Shiny, the results are called using the names, e.g. result$only_sig1, not index, e.g. result[[5]].
  
  return(output)
}

# test the function: normally the test codes need to be commented out so R Shiny won't run them
# sig1 <- read.table("SigRepoInterface/server_functions/example_sigCompare_1.txt", header = T)
# sig2 <- read.table("SigRepoInterface/server_functions/example_sigCompare_2.txt", header = T)
# sigCompare_two(sig1, sig2, is.lv2 = T)


## implementation by getting feature sets from db
## compare signature function
compare_signatures <- function(sig1_name,sig2_name,background=22000){
    sig1 = sql_generic(
        paste(
            "select feature_name, weight from feature_signature_view where signature_name =",
            single_quoted(sig1_name), ";",
            sep = ""
        )
    )
    colnames(sig1)=c("symbol","score")
    sig2=sql_generic(
        paste(
            "select feature_name, weight from feature_signature_view where signature_name =",
            single_quoted(sig2_name), ";",
            sep = ""
        )
    )
    colnames(sig2)=c("symbol","score")
    return(sigCompare_two(sig1, sig2, sig1_name = sig1_name, sig2_name = sig2_name, is.lv2 = TRUE, background_number = background))
    # available names: c("Venn", "only_sig1", "only_sig2", "sig_both", "hyper_p.value", "sig1_name", "sig2_name", "sig1_symbol", "sig2_symbol")
}



