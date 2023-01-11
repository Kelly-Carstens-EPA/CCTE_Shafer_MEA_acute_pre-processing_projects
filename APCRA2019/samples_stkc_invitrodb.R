library(tcpl)
library(readxl)
library(data.table)
tcplConf(drvr = "MySQL", user = Sys.getenv('INVITRODB_USER_RO'), db = "invitrodb", pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'))
tcplListFlds("sample")
# "spid"             "chid"             "stkc"             "stkc_unit"        "tested_conc_unit"

# file with all APCRA spids:
adat <- read_excel("L:/Lab/NHEERL_MEA/Project TSCA_APCRA/EPA_18235_EPA-Shafer_84_20181129.xlsx")
setDT(adat)

samples <- tcplQuery("SELECT * FROM sample WHERE spid LIKE 'EPAPLT0154%'")
nrow(samples) == nrow(adat) # TRUE!! got them all!s
samples # oaky, some are not exactly 20. 
save(samples, file = paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/samples_stkc_invitrodb_2020-06-12.RData"))

# code from SOP on how to merge:
# spids <- unique(srcf[wllt=="t", spid])
# samples <- samples[spid %in% spids, list(spid, stkc = signif(stkc,3), stkc_unit)]
# setkey(samples, "spid")
# setkey(srcf, "spid")
# srcf <- samples[srcf]
# srcf[ wllt == 't', conc := signif(stkc/20 * conc, 3)]
