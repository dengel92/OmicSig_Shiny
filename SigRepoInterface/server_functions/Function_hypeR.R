library(hypeR)
library(dplyr)
library(magrittr)

#---------------------
#' @title hypeR_overrep_enrich_function()
#' @description Perform over-representative analysis using hypeR package
#' last updated 04/2020
#'
#' NOTE: "pval = 0.05" should be "fdr = 0.05" instead.
#' but since currently the example signature lists are too short, if we set a normal criteria, it won't be able to get any results
#'
#' @param sig_sym A character contains signature symbols.
#' @param gset_names A dataframe or matrix to specify which geneset(s) to use. required columns including "species", "category", "subcategory". See hypeR::msigdb_info() to see available genesets.
#' @param test "overrep" or "enrich"
#' @return A dataframe of over-representation analysis result.
#'
#' @example
#'
#' test_sig_sym <- c("TNMD", "FUCA2", "C1orf112", "FGR", "MAD1L1", "NFYA",
#' "STPG1", "CFTR", "LAS1L", "ENPP4", "SEMA3F", "ANKIB1", "KRIT1", "RAD52",
#' "MYH16", "BAD", "LAP3", "CD99", "HS3ST1", "AOC1", "WNT16", "SNX11",
#' "TMEM176A", "M6PR")
#' gset_names <- data.frame(species = "Homo sapiens",
#' category = c("C2", "C4"),
#' subcategory = c("CP:KEGG", "CGN"))
#' hypeR_overrep_enrich_function(test_sig_sym, gset_names, test = "overrep")
#'
#'

hypeR_overrep_enrich_function <- function(sig_sym, gset_names, test = NULL) {
  # check input signature symbol and gset_names are valid:
  if (class(sig_sym) != "character") {
    stop("trying to run hypeR(): input signature symbols are not character.")
  }
  if (class(gset_names) != "data.frame" && class(gset_names) != "matrix") {
    stop("trying to run hypeR(): input geneset names not valid, should be a dataframe or matrix.")
  }
  if (any(!c("species", "category", "subcategory") %in% colnames(gset_names))) {
    stop("trying to run hypeR(): input geneset names does not contain required columns: species, category, subcategory.")
  }

  if (test == "overrep") {
    res <- data.frame(category = character(0), subcategory = character(0), label = character(0), pval = numeric(0), fdr = numeric(0), signature = numeric(0), geneset = numeric(0), overlap = numeric(0), background = numeric(0), hits = character(0))
    for (i in c(1:nrow(gset_names))) {
      temp_gsets <- hypeR::msigdb_gsets(
        species = as.character(gset_names$species[i]),
        category = as.character(gset_names$category[i]), subcategory = as.character(gset_names$subcategory[i])
      )
      temp_hyp_obj_overrep <- NULL
      temp_overrep <- NULL
      temp_hyp_obj_overrep <- hypeR::hypeR(sig_sym, temp_gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
      temp_overrep <- temp_hyp_obj_overrep$data
      if (!is.null(temp_overrep) && nrow(temp_overrep) != 0) {
        temp_overrep <- cbind(
          category = as.character(gset_names$category[i]),
          subcategory = as.character(gset_names$subcategory[i]),
          temp_overrep
        )
        res <- rbind(res, temp_overrep)
      }
    }
  }

  else if (test == "enrich") {
    res <- data.frame(category = character(0), subcategory = character(0), label = character(0), pval = numeric(0), fdr = numeric(0), signature = numeric(0), geneset = numeric(0), overlap = numeric(0), score = numeric(0))
    for (i in c(1:nrow(gset_names))) {
      temp_gsets <- hypeR::msigdb_gsets(
        species = as.character(gset_names$species[i]),
        category = as.character(gset_names$category[i]), subcategory = as.character(gset_names$subcategory[i])
      )
      temp_hyp_obj_enrich <- NULL
      temp_enrich <- NULL
      temp_hyp_obj_enrich <- hypeR::hypeR(sig_sym, temp_gsets, test = "kstest", pval = 0.05, plotting = FALSE)
      temp_enrich <- temp_hyp_obj_enrich$as.data.frame()
      if (!is.null(temp_enrich) && nrow(temp_enrich) != 0) {
        temp_enrich <- cbind(
          category = as.character(gset_names$category[i]),
          subcategory = as.character(gset_names$subcategory[i]),
          temp_enrich
        )
        res <- rbind(res, temp_enrich)
      }
    }
  } else {
    stop("Please select a test: overrep or enrich.")
  }

  return(res)
}

#---------------------
#' @title hypeR_overrep_enrich_server_function()
#' @description Perform over-representative analysis using hypeR package, output result to RShiny Server
#' last updated 04/2020
#'
#' NOTE: "pval = 0.05" should be "fdr = 0.05" instead.
#' but since currently the example signature lists are too short, if we set a normal criteria, it won't be able to get any results
#'
#' @param signature_df A dataframe or character contains signature information. A dataframe with columns "symbol" or "symbol" and "direction", or a character. No score (weight) for each feature is needed for over-representative test.
#' @param species A string. The species of the input signature. For example, "Homo sapiens".
#' @return A list with two objects. $overrep is a data.frame of over-representation analysis result. $signature is the input signature features, used for display signature in Shiny interface.
#'
#' @example
#'
#' signature_df <- data.frame(cbind(c("AOC1", "LAP3", "FUCA2", "HS3ST1", "LAS1L", "BAD", "WNT16", "FGR", "CFTR", "NFYA"),
#'                                  c(rep("+", 5),rep("-", 5))))
#' colnames(signature_df) <- c("signature_symbol", "signature_direction")
#' signature_df$signature_symbol <- as.character(signature_df$signature_symbol)
#' gset_names <- data.frame(species = "Homo sapiens",
#' category = c("C2", "C4"),
#' subcategory = c("CP:KEGG", "CGN"))
#' overrep_result <- hypeR_overrep_enrich_server_function(signature_df, gset_names)
#' overrep_result$overrep
#'
#'

hypeR_overrep_enrich_server_function <- function(signature_df, gset_names, test = NULL) {
  # check gset_names is valid:
  if (class(gset_names) != "data.frame" && class(gset_names) != "matrix") {
    stop("trying to run hypeR(): input geneset names not valid, should be a dataframe or matrix.")
  }
  if (any(!c("species", "category", "subcategory") %in% colnames(gset_names))) {
    stop("trying to run hypeR(): input geneset names does not contain required columns: species, category, subcategory.")
  }

  if (class(signature_df) == "data.frame") {
    # if signature has direction information and is bi-directional with "+" "-" or "Up" "Dn",
    # we have to perform overrep/enrich for up and dn signatures respectively:
    if ("signature_direction" %in% colnames(signature_df)) {
      signature_df$signature_direction <- tolower(signature_df$signature_direction)
      signature_df$signature_direction <- replace(signature_df$signature_direction, which(signature_df$signature_direction == "up"), "+")
      signature_df$signature_direction <- replace(signature_df$signature_direction, which(signature_df$signature_direction == "dn"), "-")
      signature_df <- signature_df[order(abs(signature_df$signature_score), decreasing = T), ]

      if (setequal(signature_df$signature_direction, c("-", "+"))) {
        sig_dn <- c()
        sig_up <- c()
        sig_dn <- signature_df$signature_symbol[which(signature_df$signature_direction == "-")]
        sig_up <- signature_df$signature_symbol[which(signature_df$signature_direction == "+")]
        result_dn <- NULL
        result_up <- NULL
        # perform Over-representation analysis:
        if (test == "overrep") {
          if (length(sig_dn) > 0) {
            result_dn <- hypeR_overrep_enrich_function(sig_dn, gset_names, test = "overrep")
            if (!is.null(result_dn) && nrow(result_dn) != 0) {
              result_dn <- cbind(result_dn, "direction" = "-")
            }
          }
          if (length(sig_up) > 0) {
            result_up <- hypeR_overrep_enrich_function(sig_up, gset_names, test = "overrep")
            if (!is.null(result_up) && nrow(result_up) != 0) {
              result_up <- cbind(result_up, "direction" = "+")
            }
          }
        } # end of overrep

        # perform enrichment analysis:
        if (test == "enrich") {
          if (length(sig_dn) > 0) {
            result_dn <- hypeR_overrep_enrich_function(sig_dn, gset_names, test = "enrich")
            if (!is.null(result_dn) && nrow(result_dn) != 0) {
              result_dn <- cbind(result_dn, "direction" = "-")
            }
          }
          if (length(sig_up) > 0) {
            result_up <- hypeR_overrep_enrich_function(sig_up, gset_names, test = "enrich")
            if (!is.null(result_up) && nrow(result_up) != 0) {
              result_up <- cbind(result_up, "direction" = "+")
            }
          }
        } # end of enrichment

        # note: if set plotting=TRUE in hypeR(), hypeR() function can return a list of Venn diagrams, with length of the genesets that are found to be enriched.
        # overrep_plot_dn <- hyp_obj_result_dn$plots
        # overrep_plot_up <- hyp_obj_result_up$plots
        # hyp_obj_result_dn$plots itself is a *list* variable.
        # each of them, e.g. hyp_obj_result_dn$plots[[1]], is a Venn Diagram plot of a gene set, e.g.KEGG_TYROSINE_METABOLISM, and the input signature list

        # combine the overrep/enrich results of dn and up:
        # can't simply do a rbind(), because if one of them is empty, it will give an error
        if (!is.null(result_dn) && !is.null(result_up)) {
          result <- rbind(result_up, result_dn)
          sig <- c(sig_dn, sig_up)
        } else if (!is.null(result_dn)) {
          result <- result_dn
          sig <- sig_dn
        } else if (!is.null(result_up)) {
          result <- result_up
          sig <- sig_up
        }
      }
    } # end of bi-directional signature
    # else if there is no direction information, or there's only one direction:
    else if (!signature_direction %in% colnames(signature_df) | length(unique(signature_df$signature_direction)) == 1) {
      sig <- signature_df$signature_symbol
      if (test == "overrep") {
        result <- hypeR_overrep_enrich_function(sig, gset_names, test = "overrep")
      }
      else if (test == "enrich") {
        result <- hypeR_overrep_enrich_function(sig, gset_names, test = "enrich")
      } else {
        stop("Please select a test: overrep or enrich.")
      }
    } # end of one direction
  } # end of if sig is dataframe

  if (class(signature_df) == "character") {
    sig <- signature_df
    remove(signature_df)
    if (test == "overrep") {
      result <- hypeR_overrep_enrich_function(sig, gset_names, test = "overrep")
    }
    else if (test == "enrich") {
      result <- hypeR_overrep_enrich_function(sig, gset_names, test = "enrich")
    } else {
      stop("Please select a test: overrep or enrich.")
    }
  } # end of if sig is character

  # all available overrep column names: label pval fdr signature geneset overlap background hits
  # all available enrich column names: label pval fdr signature geneset overlap score

  if (test == "overrep") {
    result_output <- result[, c("category", "subcategory", "label", "pval", "fdr", "geneset", "overlap", "hits")]
  } else if (test == "enrich") {
    result_output <- result[, c("category", "subcategory", "label", "pval", "fdr", "geneset", "overlap", "score")]
    result_output$geneset<-as.integer(result_output$geneset)
    result_output$overlap<-as.integer(result_output$overlap)
  }

  if (!is.null(result$direction)) {
    result_output <- cbind(result$direction, result_output)
    colnames(result_output)[1] <- "direction"
  }

  # output the result:
  output <- list()
  output[[1]] <- result_output
  #output[[2]] <- sig
  #names(output) <- c("result", "signature")
  names(output) <- c("result")
  return(output)
}
