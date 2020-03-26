library(hypeR)
library(dplyr)
library(magrittr)

#' @title hypeR_overrep_function() Perform over-representative analysis using hypeR package
#' @description Compare two lists of signatures and return Venn diagram and hypergeometric test p-value for intersect.
#' updated 03/2020
#' 
#' NOTE: "pval = 0.05" in all hypeR_overrep_function() should be "fdr = 0.05" instead.
#' but since currently the example signature lists are too short, if we set a normal criteria, it won't be able to get any results
#'
#' @param signature_df A dataframe or character contains signature information. A dataframe with columns "symbol" or "symbol" and "direction", or a character. No score (weight) for each feature is needed for over-representative test.
#' @param species A string. The species of the input signature. For example, "Homo sapiens".
#' @return A list with two objects. $overrep is a data.frame of over-representation analysis result. $signature is the input signature features, used for display signature in Shiny interface.
#'
#' @example
#' 
#' signature_df <- data.frame(cbind(c("DPM1","CFH","FUCA2","MAD1L1","KLHL13","FUCA2"),
#'                                  c(rep("Up", 3),rep("Dn", 3))))
#' colnames(signature_df) <- c("signature_symbol", "signature_direction")
#' signature_df$signature_symbol <- as.character(signature_df$signature_symbol)
#' overrep_result <- hypeR_overrep_function(signature_df, species = "Homo sapiens")
#' overrep_result$overrep
#'  
#' 

hypeR_overrep_function <- function(signature_df, species = "Homo sapiens") {
  # get KEGG gene set info:
  if (species == "Homo sapiens") {
    gsets <- hypeR::msigdb_gsets(species = "Homo sapiens", category = "C2", subcategory = "CP:KEGG")
  }

  if (class(signature_df) == "data.frame") {
    # if signature has direction information and is bi-directional with "+" "-" or "Up" "Dn",
    # we have to perform overrep for up and dn signatures respectively:

    if ("signature_direction" %in% colnames(signature_df) &&
      (setequal(signature_df$signature_direction, c("-", "+")) | setequal(signature_df$signature_direction, c("Dn", "Up")))) {
      sig_dn <- c()
      sig_up <- c()
      sig_dn <- signature_df$signature_symbol[which(signature_df$signature_direction == "-" | signature_df$signature_direction == "Dn")]
      sig_up <- signature_df$signature_symbol[which(signature_df$signature_direction == "+" | signature_df$signature_direction == "Up")]

      # perform Over-representation analysis:
      overrep_dn <- NULL
      overrep_up <- NULL
      if (length(sig_dn) > 0) {
        hyp_obj_overrep_dn <- hypeR::hypeR(sig_dn, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
        if (nrow(hyp_obj_overrep_dn$data) != 0) {
          overrep_dn <- cbind(hyp_obj_overrep_dn$data, "direction" = "Dn")
        }
      }
      if (length(sig_up) > 0) {
        hyp_obj_overrep_up <- try(hypeR::hypeR(sig_up, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F), silent = TRUE)
        if (nrow(hyp_obj_overrep_up$data) != 0) {
          overrep_up <- cbind(hyp_obj_overrep_up$data, "direction" = "Up")
        }
      }

      # note: if set plotting=TRUE in hypeR(), hypeR() function can return a list of Venn diagrams, with length of the genesets that are found to be enriched.
      # overrep_plot_dn <- hyp_obj_overrep_dn$plots
      # overrep_plot_up <- hyp_obj_overrep_up$plots
      # hyp_obj_overrep_dn$plots itself is a *list* variable.
      # each of them, e.g. hyp_obj_overrep_dn$plots[[1]], is a Venn Diagram plot of a gene set, e.g.KEGG_TYROSINE_METABOLISM, and the input signature list

      # combine the overrep results of dn and up:
      # can't simply do a rbind(), because if one of them is empty, it will give an error. could have better way to do this.

      if (!is.null(overrep_dn) && !is.null(overrep_up)) {
        overrep <- rbind(overrep_up, overrep_dn)
        sig <- c(sig_dn, sig_up)
      } else if (!is.null(overrep_dn)) {
        overrep <- overrep_dn
        sig <- sig_dn
      } else if (!is.null(overrep_up)) {
        overrep <- overrep_up
        sig <- sig_up
      }
      # end of bi-directional signature case

      # else if there is no direction information, or there's only one direction:
    } else if (!signature_direction %in% colnames(signature_df) | length(unique(signature_df$signature_direction)) == 1) {
      sig <- signature_df$signature_symbol
      hyp_obj_overrep <- hypeR::hypeR(signature_df$signature_symbol, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
      overrep <- hyp_obj_overrep$data
    }
  }

  if (class(signature_df) == "character") {
    sig <- signature_df
    hyp_obj_overrep <- hypeR::hypeR(sig, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
    overrep <- hyp_obj_overrep$data
  }

  # all available overrep column names: label pval fdr signature geneset overlap background hits direction
  overrep_output <- overrep[, c("label", "pval", "fdr", "geneset", "overlap", "hits")]
  if (!is.null(overrep$direction)) {
    overrep_output <- cbind(overrep$direction, overrep_output)
    colnames(overrep_output)[1] <- "direction"
  }

  # output the result:
  output <- list()
  output[[1]] <- overrep_output
  output[[2]] <- sig
  names(output) <- c("overrep", "signature")
  return(output)
}

# code for testing the function: normally need to comment it out so R Shiny won't run these
# source("Omic.obj/OmicObj.R")
# source("Omic.obj/check_functions/Function_json.R")
# test_Omic.obj <- read_json("Omic.obj/signatures/MDA_AhR_obj.json")
# test_Omic.obj$signatures
# test_result <- overrep_hypeR(test_Omic.obj$signatures, species = "Homo sapiens")
# test_result$overrep


# hypeR_gsea_function()
# input: ranked signature, a lv1 dataframe with symbol and score
# output: a list: gsea=data.frame of gsea analysis result; signature=signature features
