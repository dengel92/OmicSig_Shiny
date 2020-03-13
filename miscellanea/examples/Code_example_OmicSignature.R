
#### json, objCheck functions ####
source("OmicSignature/check_functions/Function_json.R")
source("OmicSignature/check_functions/Function_objCheck.R")
source("OmicSignature/OmicSignature.R")

Omic.obj.new <- read_json("OmicSignature/signatures/Cal27_CB113_obj.json")

# check functions:
check_metadata(Omic.obj.new)
check_signatures(Omic.obj.new)
check_difexp(Omic.obj.new)
# view signatures:
Omic.obj.new$signatures
# use new criteria to extract signatures:
Omic.obj.new$extract.signature("fdr < 0.05")

# write files:
write_json(Omic.obj.new,"OmicSignature/example_Omic.obj.json")
write.table(Omic.obj.new$difexp, "OmicSignature/example_lv1.txt", sep = "\t", quote = F, col.names = T, row.names = F)


#### prepare an obj example without lv1 data ####
Omic.obj.new$difexp <- NULL
write_json(Omic.obj.new, "OmicSignature/example_lv2lv3_Omic.obj.txt")

# read again to check:
Omic.obj.lv2 <- read_json("OmicSignature/example_lv2lv3_Omic.obj.txt")
Omic.obj.lv2$signatures
check_metadata(Omic.obj.lv2)
check_signatures(Omic.obj.lv2)
check_difexp(Omic.obj.lv2)
