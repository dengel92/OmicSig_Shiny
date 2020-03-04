library(hypeR)
library(dplyr)
library(magrittr)

# gsea_hypeR input: dataframe with symbol and direction, or character;
#           output: list: gsea=data.frame, signature=signature features

# NOTE: "pval = 0.05" in all hypeR() funcitons should be "fdr = 0.05" instead,
# but since currently the example signature list is too short, if we set a normal criteria, it won't be able to get any results

gsea_hypeR <- function(signature_df, species = "Homo sapiens") {
  # get KEGG gene set info:
  if (species == "Homo sapiens") {
    gsets <- hypeR::msigdb_gsets(species = "Homo sapiens", category = "C2", subcategory = "CP:KEGG")
  }
  
  if (class(signature_df) == "data.frame") {
      # if signature has direction information and is bi-directional with "+" "-" or "Up" "Dn",
      # we have to perform GSEA for up and dn signatures respectively:
    if ("signature_direction" %in% colnames(signature_df) && 
            (setequal(signature_df$signature_direction,c("-","+")) | setequal(signature_df$signature_direction,c("Dn","Up"))) ) {
      sig_dn <- c()
      sig_up <- c()
      sig_dn <- signature_df$signature_symbol[which(signature_df$signature_direction == "-" | signature_df$signature_direction == "Dn")]
      sig_up <- signature_df$signature_symbol[which(signature_df$signature_direction == "+" | signature_df$signature_direction == "Up")]

      # perform gsea analysis:
      gsea_dn <- NULL
      gsea_up <- NULL
      if (length(sig_dn) > 0) {
        hyp_obj_overrep_dn <- hypeR::hypeR(sig_dn, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
        if (nrow(hyp_obj_overrep_dn$data) != 0) {
          gsea_dn <- cbind(hyp_obj_overrep_dn$data, "direction" = "Dn")
        }
      }
      if (length(sig_up) > 0) {
        hyp_obj_overrep_up <- try(hypeR::hypeR(sig_up, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F), silent = TRUE)
        if (nrow(hyp_obj_overrep_up$data) != 0) {
          gsea_up <- cbind(hyp_obj_overrep_up$data, "direction" = "Up")
        }
      }
      
      # note: if set plotting=TRUE in hypeR(), hypeR() function can return a list of Venn diagrams, with length of the genesets that are found to be enriched. 
      # gsea_plot_dn <- hyp_obj_overrep_dn$plots
      # gsea_plot_up <- hyp_obj_overrep_up$plots
      # hyp_obj_overrep_dn$plots itself is a *list* variable. 
      # each of them, e.g. hyp_obj_overrep_dn$plots[[1]], is a Venn Diagram plot of a gene set, e.g.KEGG_TYROSINE_METABOLISM, and the input signature list

      # combine the GSEA results of dn and up: 
      # can't simply do a rbind(), because if one of them is empty, it will give an error. could have better way to do this.
      if (!is.null(gsea_dn) && !is.null(gsea_up)) {
        gsea <- rbind(gsea_up, gsea_dn)
        sig <- c(sig_dn, sig_up)
      } else if (!is.null(gsea_dn)) {
        gsea <- gsea_dn
        sig <- sig_dn
      } else if (!is.null(gsea_up)) {
        gsea <- gsea_up
        sig <- sig_up
      }
    # end of bi-directional signature case
    
    # else if there is no direction information, or there's only one direction:
    } else if (!signature_direction %in% colnames(signature_df) | length(unique(signature_df$signature_direction)) == 1) {
      sig <- signature_df$signature_symbol
      hyp_obj_overrep <- hypeR::hypeR(signature_df$signature_symbol, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
      gsea <- hyp_obj_overrep$data
    }
  }

  if (class(signature_df) == "character") {
    sig <- signature_df
    hyp_obj_overrep <- hypeR::hypeR(sig, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
    gsea <- hyp_obj_overrep$data
  }

  # output the result:
  output <- list()
  output[[1]] <- gsea
  output[[2]] <- sig
  names(output) <- c("gsea", "signature")
  return(output)
}


# code for testing the function: normally need to comment it out so R Shiny won't run these
#source("Omic.obj/OmicObj.R")
#source("Omic.obj/check_functions/Function_json.R")
#test_Omic.obj <- read_json("Omic.obj/signatures/MDA_AhR_obj.json")
#test_Omic.obj$signatures
#test_result <- gsea_hypeR(test_Omic.obj$signatures, species = "Homo sapiens")
#test_result$gsea