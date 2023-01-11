library(tcpl)
library(readxl)
library(data.table)
tcplConf(drvr = "MySQL", user = Sys.getenv('INVITRODB_USER_RO'), db = "invitrodb", pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'))
tcplListFlds("sample")
# "spid"             "chid"             "stkc"             "stkc_unit"        "tested_conc_unit"

# file with all GF spids:
adat <- read_excel("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/GF2019/spidmap_2020-06-22.xlsx")
setDT(adat)

samples <- tcplQuery(paste0("SELECT * FROM sample WHERE spid IN('EX000374','EX000371','EX000408','EX000411','EX000373','EX000372')"))
samples <- tcplQuery(paste0("SELECT * FROM sample WHERE spid IN('EX000374')"))
nrow(samples) == nrow(adat) # TRUE!! got them all!s
samples # oaky, all are 20!
save(samples, file = paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/GF2019/samples_stkc_invitrodb_2020-06-22.RData"))

# code from SOP on how to merge:
# spids <- unique(srcf[wllt=="t", spid])
# samples <- samples[spid %in% spids, list(spid, stkc = signif(stkc,3), stkc_unit)]
# setkey(samples, "spid")
# setkey(srcf, "spid")
# srcf <- samples[srcf]
# srcf[ wllt == 't', conc := signif(stkc/20 * conc, 3)]
