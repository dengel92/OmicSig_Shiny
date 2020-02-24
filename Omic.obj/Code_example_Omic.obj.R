
#### json, objCheck functions ####
source("Omic.obj/Function_json.R")
source("Omic.obj/Function_objCheck.R")
source("Omic.obj/OmicObj.R")

Omic.obj.new <- read_json("Omic.obj/signatures/Cal27_CB113_obj.txt")

# check functions:
check_metadata(Omic.obj.new)
check_signatures(Omic.obj.new)
check_difexp(Omic.obj.new)
# view signatures:
Omic.obj.new$signatures
# use new criteria to extract signatures:
Omic.obj.new$extract.signature("logFC < -1; fdr < 0.05")
# write files:
write_obj(Omic.obj.new,"Omic.obj/example_Omic.obj.txt")
write.table(Omic.obj.new$difexp, "Omic.obj/example_lv1.txt", sep = "\t", quote = F, col.names = T, row.names = F)


#### prepare an obj example without lv1 data ####
Omic.obj.new$difexp <- NULL
write_obj(Omic.obj.new, "Omic.obj/example_lv2lv3_Omic.obj.txt")

# read again to check:
Omic.obj.lv2 <- read_json("Omic.obj/example_lv2lv3_Omic.obj.txt")
Omic.obj.lv2$signatures
check_metadata(Omic.obj.lv2)
check_signatures(Omic.obj.lv2)
check_difexp(Omic.obj.lv2)
