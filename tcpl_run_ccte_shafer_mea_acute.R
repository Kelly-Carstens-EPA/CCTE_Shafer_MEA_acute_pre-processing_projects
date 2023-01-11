# script to run MEA_acute in tcpl
library(tcpl)
library(data.table)
library(RMySQL)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))


# PREPARE MC0 ------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
load("lvl0_snapshots/mea_acute_lvl0_2020-07-28.RData")

# Assign acid's
acid.acnm <- tcplLoadAcid(fld = "acnm", val = unique(mea_acute_lvl0$acnm))
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm, all.x = T, by = "acnm")
mea_acute_lvl0[, acnm := NULL]

tcplConfList()
tcplWriteLvl0(mc0, type = "mc")


# REGISTER/CONFIRM METHODS -----------------------------------------
acids <- unique(mea_acute_lvl0$acid)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdClear(lvl=2L, id = acids, type = "mc")
tcplMthdAssign(lvl = 2L, id = acids, mthd_id = 1, ordr = c(1), type = "mc") # all none for lvl 2
tcplMthdLoad(lvl=2, id = acids)

# lvl 3
dn.mea <- aeid.info[grepl("_dn",aenm), aeid]
tcplMthdLoad(lvl=3, id = dn.mea)
tcplMthdClear(lvl=3, id = dn.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = dn.mea, mthd_id = c(17, 35, 6), ordr = c(1:3), type = "mc")
tcplMthdLoad(lvl=3, id = dn.mea)
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 35                       resp.logfc                        Calculate the response as a fold change over baseline for logged values
# 6                    resp.multneg1                                                                    multiply the response by -1

up.mea <- aeid.info[grepl("_up",aenm) & !(grepl("(LDH)|(AB)",aenm)), aeid]
tcplMthdLoad(lvl=3, id = up.mea)
tcplMthdClear(lvl=3, id = up.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = up.mea, mthd_id = c(17,35), ordr = c(1:2), type = "mc")
tcplMthdLoad(lvl=3, id = up.mea)
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 35                       resp.logfc                        Calculate the response as a fold change over baseline for logged values

alamarblue <- aeid.info[grepl("AB",aenm), aeid]
tcplMthdLoad(lvl=3, id = alamarblue)
tcplMthdClear(lvl=3, id = alamarblue, type = "mc")
tcplMthdAssign(lvl = 3, id = alamarblue, mthd_id = c(17,32,5), ordr = c(1:3), type = "mc")
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 32                        pval.zero                                                                                 Set pval to 0.
# 5                          resp.pc                                                                      response percent activity

ldh <- aeid.info[grepl("LDH",aenm), aeid]
tcplMthdLoad(lvl=3, id = ldh)
tcplMthdClear(lvl=3, id = ldh, type = "mc")
tcplMthdAssign(lvl = 3, id = ldh, mthd_id = c(17,13,5), ordr = c(1:3), type = "mc")
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 13              pval.apid.pwlls.med                                       plate-wise median based on positive control, single dose
# 5                          resp.pc                                                                      response percent activity

# lvl 4
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
tcplMthdClear(lvl=4L, id = aeid.info$aeid, type = "mc")
tcplMthdAssign(lvl=4L, id = aeid.info$aeid, mthd_id = c(2), ordr = c(1), type = "mc")
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
# bmad.aeid.lowconc.nwells

# lvl 5
tcplMthdLoad(lvl=5L, id = aeid.info$aeid)
tcplMthdClear(lvl=5L, id = aeid.info$aeid, type = "mc")
tcplMthdAssign(lvl=5L, id = aeid.info$aeid, mthd_id = c(1), ordr = c(1), type = "mc")
# bmad3

# lvl 6
tcplMthdLoad(lvl=6L, id = aeid.info$aeid)
tcplMthdClear(lvl = 6L, id = aeid.info$aeid, type = "mc")
lvl6_mthds <- tcplMthdList(lvl = 6L, type = "mc")
tcplMthdAssign(lvl = 6L, id = c(dn.mea, up.mea), mthd_id = lvl6_mthds[mc6_mthd != "viability.gnls",mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
tcplMthdAssign(lvl = 6L, id = c(alamarblue, ldh), mthd_id = lvl6_mthds[,mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
tcplMthdLoad(lvl=6L, id = aeid.info$aeid)


# TCPL RUN ------------------------------------------------------------
rm(list = ls())

assay.list <- tcplLoadAcid(fld = "asid", val= 20)[grepl("acute",acnm), ]

dbListConnections(MySQL())
tcplConfList()

tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

