source("Omic.obj/OmicObj.R")
temp_upregulated <- data.frame(
  cbind(signatures$Up_Regulated_Symbol, signatures$Up_Regulated_Score, rep("Up", length(signatures$Up_Regulated_Symbol)))
)
temp_dnregulated <- data.frame(
  cbind(signatures$Dn_Regulated_Symbol, signatures$Dn_Regulated_Score, rep("Dn", length(signatures$Dn_Regulated_Symbol)))
)
example_lv2_df <- data.frame(rbind(temp_upregulated, temp_dnregulated))
colnames(example_lv2_df) <- c("symbol", "score", "direction")
example_lv3_df <- example_lv2_df[, -2]
remove(temp_upregulated, temp_dnregulated)

write.table(example_lv2_df, "Omic.obj/example_lv2.txt", sep = "\t", quote = F, row.names = F, col.names = T)
write.table(example_lv3_df, "Omic.obj/example_lv3.txt", sep = "\t", quote = F, row.names = F, col.names = T)
