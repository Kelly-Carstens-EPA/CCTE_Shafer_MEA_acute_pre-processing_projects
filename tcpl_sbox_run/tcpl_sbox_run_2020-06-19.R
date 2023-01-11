# 06/19/2020
# trying out new LDH methods: resp := (cval - bval) / (pval - bval)
# but with wllt assigned in the scripts.
# want to confirm wllt corrrect assignment, and that Na for 1/2 lysis well conc is okay
library(tcpl)
library(data.table)
library(RMySQL)
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R')
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList()
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")

# first, want to check out wllt=="v" stuff
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='stg_invitro', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
mc0_wlltv <- tcplLoadData(lvl = 0L, fld = "wllt", val = "v", type = "mc")
mc0_wlltv[, .N, by = c("spid")]
# spid      N
# 1:     Tetraoctyl ammonium bromide 154743
# 2:                 tetraoctylNH4Br  19872
# 3: Tetra-N-Octyle ammonium bromide  93120
# 4:      Tetraoctylammonium bromide  23264
# 5:  Tetra-N-Octyl ammonium bromide  25920
# 6:    Tetra-octyl ammonium bromide  41760
# 7:                     Total Lysis  45684
# 8:                    Aflatoxin B1  61101
# 9:                            DCNQ   1800
# 10:  tetra-n-octyl-ammonium bromide   3000
# 11:    Tetra octyl ammonium bromide 101952
# 12:     Tetra-octylammonium bromide  34560
# 13:                           Tetra  40224
# 14:   Tetra-n-octylammonium bromide    384

# total Lysis...
acids <- mc0_wlltv[spid == "Total Lysis", unique(acid)]
tcplListFlds("assay_component")
tcplLoadAcid(fld = "acid", val = acids, add.fld = "assay_component_desc")

# Tetra... are these al the same?
mc0_wlltv[grepl("[Tt]etra",spid), unique(acid)] # several

# DCNQ - acutely Toxic
mc0_wlltv[spid == "DCNQ", unique(acid)]
tcplLoadAcid(fld = "acid", val = c(1919, 1992), add.fld = "assay_component_desc")

# Aflatoxin B1 - carnciogen, mutagen, etc.
mc0_wlltv[spid == "Aflatoxin B1", unique(acid)] # several
tcplLoadAsid(fld = "acid", val = c(722,599,600, 694)) # just chose a few. All are LTea
tcplLoadAcid(fld = "acid", val = c(722,599,600, 694), add.fld = "assay_component_desc")
# out of 100 assay components.
# we'll get back to this

head(unique(mc0_wlltv$srcf))

# PREPARE MC0 ------------------------------------------------------
load("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/lvl0_snapshots/mea_acute_lvl0_2020-06-19.RData")
setdiff(names(mea_acute_lvl0), names(mc0_wlltv))
setdiff(names(mc0_wlltv), names(mea_acute_lvl0)) # m0id

# Assign acid's from sbox
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.acnm.idb <- tcplLoadAcid(fld = "acid", val = unique(mea_acute_lvl0$acid))
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm.idb, all.x = T)
mea_acute_lvl0[, acid := NULL]

tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.info <- tcplLoadAcid(fld = "asid", val = 20)
(mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.info, by = "acnm"))
mea_acute_lvl0[, c("acnm","asid") := list(NULL, NULL)]

# write it!
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConfList()
names(mea_acute_lvl0)

mea_acute_lvl0[is.na(conc)]

# Need to remove these, since not present in sbox
mea_acute_lvl0 <- mea_acute_lvl0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01"))]
mc0 <- mea_acute_lvl0[acid %in% c(2432, 2433, 2445, 2446, 2447)]
tcplConfList() # sbox confirmed
tcplWriteLvl0(mc0, type = "mc") # June 19, 2020 5:34pm
#Completed delete cascade for 1 ids (83.46 secs)
#

# checking db for 2 compounds, to see if these are glypho and lop
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplLoadChem(field = "spid", val = c("EPAPLT0167G09","EPAPLT0170D03"))
# spid  chid      casn                 chnm dsstox_substance_id     code
# 1: EPAPLT0167G09 27403 1461-22-9 Tributyltin chloride       DTXSID3027403 C1461229
# 2: EPAPLT0170D03 21389   52-68-6          Trichlorfon       DTXSID0021389   C52686


# REGISTER/COnfRIM METHODS -----------------------------------------
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acids <- c(2432, 2433, 2445, 2446, 2447)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# I think AB and LDH should be all good
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = c(2439)))
}
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = c(2440)))
}

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # all none for lvl 2

# lvl 3
dn.mea <- c(2442, 2443, 2455)
tcplMthdLoad(lvl=3, id = dn.mea)
tcplMthdClear(lvl=3, id = dn.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = dn.mea, mthd_id = c(17, 35, 6), ordr = c(1:3), type = "mc") # set bval, resp.logfc, resp.multneg1

up.mea <- c(2425, 2426, 2438)
tcplMthdLoad(lvl=3, id = up.mea)
tcplMthdClear(lvl=3, id = up.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = up.mea, mthd_id = c(17,35), ordr = c(1:2), type = "mc")

# view 4 - 6
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = c(dn.mea, up.mea)))
}

# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "acid", val=c(2432, 2433, 2445, 2446, 2447))

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# June 21, 2020 11:40am 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# Save data and plots ----------------------------

assay.list <- tcplLoadAeid(fld = "acid", val=c(2432, 2433, 2445, 2446, 2447))
# save the data, see some plots
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/")
for (i in unique(assay.list$aeid)){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
mc3 <- dbGetQuery(con, paste0("SELEct m0id, aeid, acid, bval, pval, logc, resp FROM mc3 WHERE aeid IN (",paste0(assay.list$aeid,collapse=","),")"))
setDT(mc3)
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = unique(assay.list$acid), type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, srcf)], all.x = T)
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=assay.list$aeid, type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=assay.list$aeid, type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = assay.list$aeid, "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = assay.list$aeid, "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = assay.list$aeid, "mc")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
save(mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))

# adding cndx
load("sbox_dat/sbox_dat_2020-06-21.RData")
dbGetQuery(con, "DESC mc1") # can get cndx here...
mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
nrow(mc3)
mc3 <- merge(mc3, mc1, all.x = T)
save(mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))

# adding in cval
load("sbox_dat/sbox_dat_2020-06-21.RData")
dbGetQuery(con, "DESC mc2") # can get cndx here...
mc2 <- dbGetQuery(con, paste0("SELEct m0id, acid, cval FROM mc2 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
nrow(mc3)
mc3 <- merge(mc3, mc2, all.x = T)
save(mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))

# ---------------------------

# comapre results with what was previously pipelined
mc5_org <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/output/01APR2020/mc5_mc6_nheerl_mea_acute.csv", stringsAsFactors = FALSE)
setDT(mc5_org)

names(mc5_org)
length(unique(mc5_org$spid))*32
nrow(mc5_org)
usecols <- c("aenm","spid","dsstox_substance_id","casn","hitc","aeid")

# can only combine for compounds that were ran previously, the toxcast compounds
cmc5 <- merge(mc5_mc6[, ..usecols], mc5_org[,..usecols], by = setdiff(usecols,c("hitc","aeid")), suffixes = c(".new",".org"))
cmc5[hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 2775
cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 285

# just comparing LDH
cmc5[grepl("LDH",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 355 same hitc
cmc5[grepl("LDH",aenm) & hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 29 diff hitc

# added LDH hits
cmc5[grepl("LDH",aenm) & hitc.org == 0 & hitc.new == 1, .N] # 6
cmc5[grepl("LDH",aenm) & hitc.org == 1 & hitc.new == 0, .N] # 23 removed hits

# percent agreement
total_hitcs <- cmc5[grepl("LDH",aenm) & hitc.org != -1 & hitc.new != -1, .N]
cmc5[grepl("LDH",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N/total_hitcs] # 92.44% percent agreement


# compare to cval - bval "zero-centering" method
mc5_mc6.21 <- mc5_mc6
mc3.21 <- mc3
load("sbox_dat/sbox_dat_2020-06-11.RData")
mc3_mthds # yep, cval - bval methods
# 2: 2439 bval.apid.nwllslowconc.med      17    1
# 3: 2439                  pval.zero      32    2
# 4: 2439                 resp.logfc      35    3

cmc5.2 <- merge(mc5_mc6.21[, ..usecols], mc5_mc6[,..usecols], by = setdiff(usecols,c("hitc")), suffixes = c(".21",".11"))
cmc5.2[grepl("LDH",aenm) & hitc.11 == hitc.21, .N] # 514 agreeing hitc's
cmc5.2[grepl("LDH",aenm) & hitc.11 != hitc.21, .N] # 1 different hitc
# cmc5.2[grepl("LDH",aenm) & hitc.11 != hitc.21]
# aenm         spid dsstox_substance_id       casn aeid hitc.21 hitc.11
# 1: NHEERL_MEA_acute_LDH_up TP0001413B06       DTXSID9023881 74115-24-5 2439       1       0


# OTHER ----------------------------------------------------------------------------------------

# checking out NA conc wells, make sure it's all good
mc5_mc6.21[spid == "Tritonx100"] # empty...
mc5_mc6.21[, unique(spid)] # only DMSO and treated wells
unique(mc3.21$spid)
mc3.21[spid == "Tritonx100" & aeid == 2439, unique(logc), by = "wllt"]
# looks like where conc's NA, logc of conc just went to NA as well w/o a prob

# create methods summary
mea.aeids <- tcplLoadAeid(fld = "asid", val = 20)

i <- 3
mthds <- tcplMthdLoad(lvl = i, id = mea.aeids$aeid, type = "mc")

# actually, let's just do a thorough verbal summary
tcplMthdLoad(lvl=2L, id = unique(acid.info$acid), type = "mc") # yep, all are none

# Alamar Blue - "NHEERL_MEA_acute_AB_up" aeid == 2440
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = 2440, type = "mc"))
}

# LDH
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = 2439, type = "mc"))
}

# dn endpoint example
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = 2455, type = "mc"))
}

# up endpoint example
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = 2438, type = "mc"))
}
