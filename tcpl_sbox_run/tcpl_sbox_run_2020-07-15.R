# 07/15/2020
# re-running the LDH values with any negative blank-corrected values set to 0
# to see if the hit calls will change
library(tcpl)
library(data.table)
library(RMySQL)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")

# PREPARE MC0 ------------------------------------------------------
load("lvl0_snapshots/mea_acute_lvl0_2020-06-19.RData")

# Assign acid's from sbox
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.acnm.idb <- tcplLoadAcid(fld = "acid", val = unique(mea_acute_lvl0$acid))
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm.idb, all.x = T)
mea_acute_lvl0[, acid := NULL]
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.info <- tcplLoadAcid(fld = "asid", val = 20)
(mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.info, by = "acnm"))
mea_acute_lvl0[, c("asid") := list(NULL)]

# isolate the data that I want to run
mc0 <- mea_acute_lvl0[grepl("LDH",acnm)]
mc0[rval < 0, rval := 0.0]

# write it!
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList()

# Need to remove these, since not present in sbox
mc0 <- mc0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01"))]
mc0 <- mc0[, .(acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)] # getting rid of the extra "attributes" stuff
tcplConfList() # sbox confirmed
tcplWriteLvl0(mc0, type = "mc") # July 15, 2020 6:26pm
# Completed delete cascade for 1 ids (61.89 secs)
# [1] TRUE

# REGISTER/CONFRIM METHODS -----------------------------------------
mc0[wllt == "n",.N]
# 870

# view 3 - 6
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = 2439, type = "mc"))
}
# lvl 3
# aeid                       mthd mthd_id ordr
# 1: 2439 bval.apid.nwllslowconc.med      17    1
# 2: 2439        pval.apid.pwlls.med      13    2
# 3: 2439                    resp.pc       5    3
# I am leaving level 4 and 5 as bmad.aeid.lowconc.nwells and bmad3 for all aeid

# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "aeid", val=c(2439))

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# July 15, 2020 6:35pm
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")

# Save data and plots ----------------------------

assay.list <- tcplLoadAeid(fld = "acid", val=assay.list$acid)
# save the data, see some plots
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/")
for (i in unique(assay.list$aeid)){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   fname = paste0(i,"AEID_L6_",assay.list[aeid==i,aenm],"_sbox_",as.character.Date(Sys.Date()),".pdf"),
                   odir=getwd())
}

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
mc3 <- dbGetQuery(con, paste0("SELEct m0id, aeid, acid, bval, pval, logc, resp FROM mc3 WHERE aeid IN (",paste0(assay.list$aeid,collapse=","),")"))
setDT(mc3)
mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
nrow(mc3)
mc3 <- merge(mc3, mc1, all.x = T)
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = unique(assay.list$acid), type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)], all.x = T)
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=assay.list$aeid, type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=assay.list$aeid, type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = assay.list$aeid, "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = assay.list$aeid, "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = assay.list$aeid, "mc")
description <- paste0("Testing LDH results when set negative blank-corrected rval's to 0")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
save(description, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_6p_",as.character.Date(Sys.Date()),".RData"))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))
