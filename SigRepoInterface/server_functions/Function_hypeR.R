library(hypeR)
library(dplyr)
library(magrittr)

# readGsetList()----------
#' @title readGsetList()
#' @description read geneset from a txt file,
#' e.g. a file downloaded from Enrichr https://amp.pharm.mssm.edu/Enrichr/#stats
#' last updated 04/21/2020 by vmli
#'
#' @param file A txt file with geneset inside. each line should be a gene set.
#'  first value be the gene set name.
#'  genes should be tab seperated.
#' @return A list
readGsetList <- function(file = NULL) {
  x <- scan(file, what = "list", sep = "\n")
  y <- strsplit(x, "\t")
  names(y) <- lapply(y, function(x) x[[1]])
  y <- lapply(y, function(x) {
    x <- x[c(2:length(x))]
    x <- x[x != ""]
  })
  return(y)
}

# hypeROverrepEnrichFunction----------
#' @title hypeROverrepEnrichFunction()
#' @description Perform over-representative analysis using hypeR package
#' hypeROverrepEnrichFunction() allow input BOTH msigdb gsets (a df with names) as gsetNames
#' and a customized gset list (a list) as gsetList,
#' and output result in a df.
#' last updated 04/21/2020 by vmli
#'
#' @param sigSymbol A character contains signature symbols.
#' @param gsetNames A dataframe or matrix to specify which geneset(s) to use. required columns including "species", "category", "subcategory". See hypeR::msigdb_info() to see available genesets.
#' @param gsetList A list of genesets. the name of each entry should be the geneset name. e.g. valid output of readGsetList() function.
#' @param gsetListCategory A single character string; need this to rbind result from msigDB gset and customize gset.
#' @param gsetListSubCategory A single character string; need this to rbind result from msigDB gset and customize gset.
#' @param test required by hypeROverrepEnrichFunction()


#' @param test "overrep" or "enrich" for over-representation analysis and rank-based enrichment analysis
#' @return A dataframe of over-representation analysis result.
#'
#' @example
#'
#' testSigSymbol <- c("TNMD", "FUCA2", "C1orf112", "FGR", "MAD1L1", "NFYA",
#' "STPG1", "CFTR", "LAS1L", "ENPP4", "SEMA3F", "ANKIB1", "KRIT1", "RAD52",
#' "MYH16", "BAD", "LAP3", "CD99", "HS3ST1", "AOC1", "WNT16", "SNX11",
#' "TMEM176A", "M6PR")
#' gsetNames <- data.frame(species = "Homo sapiens",
#' category = c("C2", "C4"),
#' subcategory = c("CP:KEGG", "CGN"))
#' hypeROverrepEnrichFunction(testSigSymbol, gsetNames, test = "overrep")
#'
#'

hypeROverrepEnrichFunction <- function(sigSymbol, gsetNames = NULL, gsetList = NULL, gsetListCategory = "customize", gsetListSubCategory = "customize", test = NULL) {
  # check input signature symbol and gsetNames are valid:
  if (class(sigSymbol) != "character") {
    stop("trying to run hypeR(): input signature symbols are not character.")
  }
  if (!is.null(gsetNames)) {
    if (class(gsetNames) != "data.frame" && class(gsetNames) != "matrix") {
      stop("trying to run hypeR(): input geneset names not valid, should be a dataframe or matrix.")
    }
    if (any(!c("species", "category", "subcategory") %in% colnames(gsetNames))) {
      stop("trying to run hypeR(): input geneset names does not contain required columns: species, category, subcategory.")
    }
  }
  if (!is.null(gsetList)) {
    if (class(gsetList) != "list" | length(gsetList) == 0) {
      stop("input user-defined gene set not valid, gsetList input should be a list.")
    }
    if (is.null(names(gsetList))) {
      stop("input user-defined gene set not valid. the name of each list should be the gene set name.")
    }
  }
  if (is.null(gsetNames) && is.null(gsetList)) {
    stop("trying to run hypeR(): no genesets found.")
  }

  if (test == "overrep") {
    res <- data.frame(category = character(0), subcategory = character(0), label = character(0), pval = numeric(0), fdr = numeric(0), signature = numeric(0), geneset = numeric(0), overlap = numeric(0), background = numeric(0), hits = character(0))
    if (!is.null(gsetNames)) {
      for (i in c(1:nrow(gsetNames))) {
        tempGset <- hypeR::msigdb_gsets(
          species = as.character(gsetNames$species[i]),
          category = as.character(gsetNames$category[i]), subcategory = as.character(gsetNames$subcategory[i])
        )
        tempHypObjOverrep <- NULL
        tempOverrep <- NULL
        tempHypObjOverrep <- hypeR::hypeR(sigSymbol, tempGset, test = "hypergeometric", background = 23000, fdr = 0.05, plotting = F)
        tempOverrep <- tempHypObjOverrep$data
        if (!is.null(tempOverrep) && nrow(tempOverrep) != 0) {
          tempOverrep <- cbind(
            category = as.character(gsetNames$category[i]),
            subcategory = as.character(gsetNames$subcategory[i]),
            tempOverrep
          )
          res <- rbind(res, tempOverrep)
        }
      }
    }
    if (!is.null(gsetList)) {
      tempHypObjOverrep <- NULL
      tempOverrep <- NULL
      tempHypObjOverrep <- hypeR::hypeR(sigSymbol, gsetList, test = "hypergeometric", background = 23000, fdr = 0.05, plotting = F)
      tempOverrep <- tempHypObjOverrep$data
      if (!is.null(tempOverrep) && nrow(tempOverrep) != 0) {
        tempOverrep <- cbind(
          category = as.character(gsetListCategory),
          subcategory = as.character(gsetListSubCategory),
          tempOverrep
        )
        res <- rbind(res, tempOverrep)
      }
    }
  }

  else if (test == "enrich") {
    res <- data.frame(category = character(0), subcategory = character(0), label = character(0), pval = numeric(0), fdr = numeric(0), signature = numeric(0), geneset = numeric(0), overlap = numeric(0), score = numeric(0))
    if (!is.null(gsetNames)) {
      for (i in c(1:nrow(gsetNames))) {
        tempGset <- hypeR::msigdb_gsets(
          species = as.character(gsetNames$species[i]),
          category = as.character(gsetNames$category[i]), subcategory = as.character(gsetNames$subcategory[i])
        )
        tempHypObjEnrich <- NULL
        tempEnrich <- NULL
        tempHypObjEnrich <- hypeR::hypeR(sigSymbol, tempGset, test = "kstest", fdr = 0.05, plotting = FALSE)
        tempEnrich <- tempHypObjEnrich$as.data.frame()
        if (!is.null(tempEnrich) && nrow(tempEnrich) != 0) {
          tempEnrich <- cbind(
            category = as.character(gsetNames$category[i]),
            subcategory = as.character(gsetNames$subcategory[i]),
            tempEnrich
          )
          res <- rbind(res, tempEnrich)
        }
      }
    }
    if (!is.null(gsetList)) {
      tempHypObjEnrich <- NULL
      tempEnrich <- NULL
      tempHypObjEnrich <- hypeR::hypeR(sigSymbol, gsetList, test = "kstest", fdr = 0.05, plotting = FALSE)
      tempEnrich <- tempHypObjEnrich$as.data.frame()
      if (!is.null(tempEnrich) && nrow(tempEnrich) != 0) {
        tempEnrich <- cbind(
          category = as.character(gsetListCategory),
          subcategory = as.character(gsetListSubCategory),
          tempEnrich
        )
        res <- rbind(res, tempEnrich)
      }
    }
  } else {
    stop("Please select a test: overrep or enrich.")
  }
  return(res)
}

# if set plotting=TRUE in hypeR(), hypeR() function can return a list of Venn diagrams, with length of the genesets that are found to be enriched.
# overrep_plot_dn <- hyp_obj_result_dn$plots
# overrep_plot_up <- hyp_obj_result_up$plots
# hyp_obj_result_dn$plots itself is a *list* variable.
# each of them, e.g. hyp_obj_result_dn$plots[[1]], is a Venn Diagram plot of a gene set, e.g.KEGG_TYROSINE_METABOLISM, and the input signature list

# hypeROverrepEnrichServerFunction----------
#' @title hypeROverrepEnrichServerFunction()
#' @description based on hypeROverrepEnrichFunction(). most of this function is dealing with signature directions.
#' input signature table with score, if needed, and direction,
#' output a list to better processed in RShiny Server.
#' this function allows input BOTH msigdb gsets (a df with names) as gsetNames,
#' and a customized gset list (a list) as gsetList, since hypeROverrepEnrichFunction() allows this.
#' but as for 04/2020, in real RShiny Server SigRepo, the two situations are run independently.
#' last updated 04/21/2020 by vmli
#'
#' @param signatureDF A dataframe or character contains signature information. A dataframe with columns "symbol" or "symbol" and "direction", or a character. No score (weight) for each feature is needed for over-representative test.
#' @param gsetNames required by hypeROverrepEnrichFunction()
#' @param gsetList required by hypeROverrepEnrichFunction()
#' @param gsetListCategory required by hypeROverrepEnrichFunction(); need this to rbind result from msigDB gset and customize gset.
#' @param gsetListSubCategory required by hypeROverrepEnrichFunction(); need this to rbind result from msigDB gset and customize gset.
#' @param test required by hypeROverrepEnrichFunction()
#' @return A list with two objects. $overrep is a data.frame of over-representation analysis result. $signature is the input signature features, used for display signature in Shiny interface.
#'
#' @example
#'
#' signatureDF <- data.frame(cbind(c("AOC1", "LAP3", "FUCA2", "HS3ST1", "LAS1L", "BAD", "WNT16", "FGR", "CFTR", "NFYA"),
#'                                  c(rep("+", 5),rep("-", 5))))
#' colnames(signatureDF) <- c("signature_symbol", "signature_direction")
#' signatureDF$signature_symbol <- as.character(signatureDF$signature_symbol)
#' gsetNames <- data.frame(species = "Homo sapiens",
#' category = c("C2", "C4"),
#' subcategory = c("CP:KEGG", "CGN"))
#' overrep_result <- hypeROverrepEnrichServerFunction(signatureDF, gsetNames)
#' overrep_result$overrep
#'
#'

hypeROverrepEnrichServerFunction <- function(signatureDF, gsetNames = NULL, gsetList = NULL, gsetListCategory = "customize", gsetListSubCategory = "customize", test = NULL) {
  if (class(signatureDF) == "data.frame") {
    # if signature has direction information and is bi-directional with "+" "-" or "Up" "Dn",
    # we have to perform overrep/enrich for up and dn signatures respectively:
    if ("signature_direction" %in% colnames(signatureDF)) {
      signatureDF$signature_direction <- tolower(signatureDF$signature_direction)
      signatureDF$signature_direction <- replace(signatureDF$signature_direction, which(signatureDF$signature_direction == "up"), "+")
      signatureDF$signature_direction <- replace(signatureDF$signature_direction, which(signatureDF$signature_direction == "dn"), "-")
      # order features in signature according to score:
      # (over-rep analysis does not need this though but no harm for doing this)
      if ("signature_score" %in% colnames(signatureDF)) {
        signatureDF <- signatureDF[order(abs(signatureDF$signature_score), decreasing = T), ]
      } else if (test == "enrich") {
        stop("when performing rank-based enichment analysis, input signature need to have signature_score column available. 
              please check your input signature table.")
      }

      # extract up and dn features as sigUp and sigDn characters:
      if (setequal(signatureDF$signature_direction, c("-", "+"))) {
        sigDn <- c()
        sigUp <- c()
        sigDn <- signatureDF$signature_symbol[which(signatureDF$signature_direction == "-")]
        sigUp <- signatureDF$signature_symbol[which(signatureDF$signature_direction == "+")]
        resultDn <- NULL
        resultUp <- NULL

        # perform Over-representation analysis:
        if (test == "overrep") {
          if (length(sigDn) > 0) {
            resultDn <- hypeROverrepEnrichFunction(sigDn, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "overrep")
            if (!is.null(resultDn) && nrow(resultDn) != 0) {
              resultDn <- cbind(resultDn, "direction" = "-")
            }
          }
          if (length(sigUp) > 0) {
            resultUp <- hypeROverrepEnrichFunction(sigUp, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "overrep")
            if (!is.null(resultUp) && nrow(resultUp) != 0) {
              resultUp <- cbind(resultUp, "direction" = "+")
            }
          }
        }

        # perform enrichment analysis:
        if (test == "enrich") {
          if (length(sigDn) > 0) {
            resultDn <- hypeROverrepEnrichFunction(sigDn, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "enrich")
            if (!is.null(resultDn) && nrow(resultDn) != 0) {
              resultDn <- cbind(resultDn, "direction" = "-")
            }
          }
          if (length(sigUp) > 0) {
            resultUp <- hypeROverrepEnrichFunction(sigUp, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "enrich")
            if (!is.null(resultUp) && nrow(resultUp) != 0) {
              resultUp <- cbind(resultUp, "direction" = "+")
            }
          }
        }

        # combine the overrep/enrich results of dn and up:
        # (can't simply do a rbind(), because if one of them is empty, it will give an error)
        if (!is.null(resultDn) && !is.null(resultUp)) {
          result <- rbind(resultUp, resultDn)
          sig <- c(sigDn, sigUp)
        } else if (!is.null(resultDn)) {
          result <- resultDn
          sig <- sigDn
        } else if (!is.null(resultUp)) {
          result <- resultUp
          sig <- sigUp
        }
      }
    } # end of bi-directional signature

    # else if there is no direction information, or there's only one direction:
    else if (!signature_direction %in% colnames(signatureDF) | length(unique(signatureDF$signature_direction)) == 1) {
      sig <- signatureDF$signature_symbol
      if (test == "overrep") {
        result <- hypeROverrepEnrichFunction(sig, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "overrep")
      }
      else if (test == "enrich") {
        result <- hypeROverrepEnrichFunction(sig, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "enrich")
      } else {
        stop("Please select a test: overrep or enrich.")
      }
    } # end of uni-directional signature
  } # end of if input sig is dataframe

  else if (class(signatureDF) == "character") {
    sig <- signatureDF
    remove(signatureDF)
    if (test == "overrep") {
      result <- hypeROverrepEnrichFunction(sig, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "overrep")
    }
    else if (test == "enrich") {
      result <- hypeROverrepEnrichFunction(sig, gsetNames, gsetList, gsetListCategory, gsetListSubCategory, test = "enrich")
    } else {
      stop("Please select a test: overrep or enrich.")
    }
  } # end of if input sig is character

  else {
    stop("input signature not valid. should be a dataframe or character.")
  }

  # result output:
  # all available overrep column names: label pval fdr signature geneset overlap background hits
  # all available enrich column names: label pval fdr signature geneset overlap score
  if (test == "overrep") {
    resultOutput <- result[, c("category", "subcategory", "label", "pval", "fdr", "geneset", "overlap", "hits")]
  } else if (test == "enrich") {
    resultOutput <- result[, c("category", "subcategory", "label", "pval", "fdr", "geneset", "overlap", "score")]
    resultOutput$geneset <- as.integer(resultOutput$geneset)
    resultOutput$overlap <- as.integer(resultOutput$overlap)
  }

  if (!is.null(result$direction)) {
    resultOutput <- cbind(result$direction, resultOutput)
    colnames(resultOutput)[1] <- "direction"
  }

  # output the result:
  output <- list()
  output[[1]] <- resultOutput
  # output[[2]] <- sig
  # names(output) <- c("result", "signature")
  names(output) <- c("result")
  return(output)
}
