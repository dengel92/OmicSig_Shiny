## Note: don't need this function anymore after change signature in OmicSig Obj from list to dataframe

write_sig_bi <- function
(
    Omic.obj,
    name = "new_sample",
    type = Omic.obj$metadata$type,
    address = "OmicSignature/signatures/")
{
    signature <- Omic.obj$signature
    temp_upregulated <- data.frame(
        cbind(signature$Up_Regulated_Symbol, 
              signature$Up_Regulated_Score, 
              rep("Up", length(signature$Up_Regulated_Symbol)))
    )
    temp_dnregulated <- data.frame(
        cbind(signature$Dn_Regulated_Symbol, signature$Dn_Regulated_Score, rep("Dn", length(signature$Dn_Regulated_Symbol)))
    )
    example_lv2_df <- data.frame(rbind(temp_upregulated, temp_dnregulated))
    colnames(example_lv2_df) <- c("symbol", "score", "direction")
    example_lv3_df <- example_lv2_df[, -2]
    remove(temp_upregulated, temp_dnregulated)
    write.table(example_lv2_df, paste(address, name, "_lv2.txt", sep=""), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
    write.table(example_lv3_df, paste(address, name, "_lv3.txt", sep=""), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)

    return("finished")
}

