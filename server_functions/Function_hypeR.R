library(hypeR)
library(dplyr)
library(magrittr)

# gsea_hypeR input: dataframe with symbol and direction, or character;
#           output: list: gsea=data.frame, signature=signature features

# note: "pval = 0.05" in all hypeR() funcitons should be "fdr = 0.05" instead,
# but since currently the example signature list is too short, if we set a normal criteria, it won't be able to get any results
gsea_hypeR <- function(signature_df, species = "Homo sapiens") {
  if (species == "Homo sapiens") {
    gsets <- hypeR::msigdb_gsets(species = "Homo sapiens", category = "C2", subcategory = "CP:KEGG")
  }
  print(colnames(signature_df))
  if (class(signature_df) == "data.frame") {
    if ("signature_direction" %in% colnames(signature_df) && length(unique(signature_df$signature_direction)) > 1) {
      print("Jel_pineapple")
      sig_dn <- c()
      sig_up <- c()
      sig_dn <- signature_df$signature_symbol[which(signature_df$signature_direction == "-" | signature_df$signature_direction == "Dn")]
      sig_up <- signature_df$signature_symbol[which(signature_df$signature_direction == "+" | signature_df$signature_direction == "Up")]
      print(sig_dn)
      print(sig_up)

      gsea_dn <- NULL
      gsea_up <- NULL
      if (length(sig_dn) > 0) {
        hyp_obj_overrep_dn <- hypeR::hypeR(sig_dn, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F)
        print(hyp_obj_overrep_dn)
        if (nrow(hyp_obj_overrep_dn$data) != 0) {
          gsea_dn <- cbind(hyp_obj_overrep_dn$data, "direction" = "Dn")
          print("fish_dn")
        }
        print("oven_dn")
      }
      # gsea_plot_dn <- hyp_obj_overrep_dn$plots
      if (length(sig_up) > 0) {
        hyp_obj_overrep_up <- try(hypeR::hypeR(sig_up, gsets, test = "hypergeometric", background = 23000, pval = 0.05, plotting = F), silent = TRUE)
        if (nrow(hyp_obj_overrep_up$data) != 0) {
          gsea_up <- cbind(hyp_obj_overrep_up$data, "direction" = "Dn")
          print("fish_up")
        }
        print("oven_up")
      }

      # gsea_plot_up <- hyp_obj_overrep_up$plots
      print(head(gsea_dn))
      print(head(gsea_up))
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
    } else if (!signature_direction %in% colnames(signature_df) | length(unique(signature_df$signature_direction)) == 1) {
      print("Jel_apple")
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
  output <- list()
  output[[1]] <- gsea
  output[[2]] <- sig
  names(output) <- c("gsea", "signature")
  return(output)
}

# source("Omic.obj/check_functions/Function_json.R")
# Omic.obj.new <- read_json("Omic.obj/signatures/Cal27_CB113_obj.json")
# a <- gsea_hypeR(Omic.obj.new$signatures, species = "Homo sapiens")
# a
# a$signature
