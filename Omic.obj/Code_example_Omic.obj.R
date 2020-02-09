
#### test json, objCheck functions ####
source("Omic.obj/Function_json.R")
source("Omic.obj/Function_objCheck.R")

write_Obj_to_json(Omic.obj, "Omic.obj/example_Omic.obj.txt")

Omic.obj.new <- read_json_to_Obj("Omic.obj/example_Omic.obj.txt")
check_metadata(Omic.obj.new)
check_signatures(Omic.obj.new)
check_difexp(Omic.obj.new)
Omic.obj.new$extract.signature("logFC < -1; fdr < 0.001")
Omic.obj.new$signatures
# write_Obj_to_json(Omic.obj.new,"Omic.obj/example_Omic.obj.txt")
# write.table(Omic.obj.new$difexp, "Omic.obj/example_lv1.txt", sep = "\t", quote = F, col.names = T, row.names = F)

#### prepare lv2lv3.omic.obj example ####
Omic.obj.new$difexp <- NULL
write_Obj_to_json(Omic.obj.new, "Omic.obj/example_lv2lv3_Omic.obj.txt")
# re-read to check:
Omic.obj.lv2 <- read_json_to_Obj("Omic.obj/example_lv2lv3_Omic.obj.txt")
Omic.obj.lv2$signatures
check_metadata(Omic.obj.lv2)
check_signatures(Omic.obj.lv2)
check_difexp(Omic.obj.lv2)
