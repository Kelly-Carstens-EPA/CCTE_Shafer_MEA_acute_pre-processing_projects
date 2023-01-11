# prepare lvl 0 data for Kelly with the acid's from sbox, so that she can compare to the sbox data
# I shared sbox_dat_2020-06-21.RData with her, which was ran from mea_acute_lvl0_2020-06-19.RData
# sharing the lvl 0 data with Kelly so that she can checkout the wllq, etc on each plate

library(tcpl)
library(data.table)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")

load("lvl0_snapshots/dat4_2020-06-19.RData")

# Assign acid's from sbox
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.acnm.idb <- tcplLoadAcid(fld = "acid", val = unique(dat4$acid))
dat4 <- merge(dat4, acid.acnm.idb, all.x = T)
dat4[, acid := NULL]
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.info <- tcplLoadAcid(fld = "asid", val = 20)
(dat4 <- merge(dat4, acid.info, by = "acnm"))
dat4[, c("asid") := list(NULL)]

mea_acute_lvl0 <- dat4

save(mea_acute_lvl0, file = "data_to_share/mea_acute_lvl0_2020-06-19_sbox_acids.RData")
