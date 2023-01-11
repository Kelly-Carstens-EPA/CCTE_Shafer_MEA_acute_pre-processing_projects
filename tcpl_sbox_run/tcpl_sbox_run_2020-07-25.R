# 07/25/2020
# checking that LDH data will run smoothly even with lysis and 1/2 lysis well replicates remvoed
library(tcpl)
library(data.table)
library(RMySQL)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")

# PREPARE MC0 ------------------------------------------------------
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R')
dat4 <- get_latest_dat("dat4")
# Getting data from folders APCRA2019, DNT2019, GF2019, ToxCast2016 
# APCRA2019_dat4_2020-07-21.RData 
# DNT2019_dat4_2020-07-22.RData 
# GF2019_dat4_2020-07-21.RData 
# ToxCast2016_dat4_2020-07-23.RData 

LDH_dat <- dat4[grepl("LDH",acnm)]

# find the Lysis wells used
# (Since the Lysis is sometimes added after teh LDH plate is created, but always before the CTB experiment, 
# the AB data reliably tells me which well was Lysed on each MEA plate.id)
lysis_wells <- dat4[grepl("AB",acnm) & spid == "Tritonx100", .(experiment.date, plate.id, rowi, coli, wllq, wllq_notes, spid)]

# confirm there is exactly 1 Lysis well per plate
if(nrow(lysis_wells[, .N, by = c("experiment.date","plate.id")][N != 1]) > 0 ) {
  stop(paste0("The following plates do not have exactly 1 Lysis well:\n",lysis_wells[, .N, by = c("experiment.date","plate.id")][N != 1]))
}

# if wllq==0 for an AB well only because the rval is 0, then set that wllq assignment to 1 for the LDH wells
lysis_wells[wllq_notes == "rval is NA; ", `:=`(wllq = 1, wllq_notes = "")]

# remove "rval is NA; " from the wllq notes string
lysis_wells[, wllq_notes := sub("rval is NA; ","",wllq_notes)]

# append the wllq info for the MEA plate Lysis wells, from which all other Lysis wells are the plate were derived.
# e.g. the 3 Lysis and 3 1/2 Lysis wells in Row H of the LDH plate derived from the F1 Lysis well
LDH_dat <- merge(LDH_dat, lysis_wells, by = c("experiment.date","plate.id","spid"), all.x = T, suffixes = c("",".mea_plate"))

# update the wllq in the triplecate Lysis and 1/2 Lysis wells where needed
if (LDH_dat[wllq == 1 & wllq.mea_plate == 0, any(rowi < 7)]) {
  stop("Unexpected rows matched Lysis wells update.")
}
LDH_dat[wllq == 1 & wllq.mea_plate == 0, `:=`(wllq = 0, wllq_notes = paste0("MEA plate Lysis well ",LETTERS[rowi.mea_plate], coli.mea_plate, " has wllq = 0: ",wllq_notes.mea_plate))]
cat("wllq updated for the following LDH plate wells:\n")
print(LDH_dat[grepl("MEA plate Lysis well",wllq_notes), .(experiment.date, rowi, coli, treatment, wllq, wllq_notes, rval)])
LDH_dat[, grep(".mea_plate",names(LDH_dat), val = T) := NULL]

mc0 <- LDH_dat

# Assign acid's from sbox
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.info <- tcplLoadAcid(fld = "asid", val = 20)
ldh_acid <- acid.info[grepl("LDH",acnm),acid]
# acid.info[, acnm := sub("NHEERL","CCTE_Shafer",acnm)]
mc0[, acid := ldh_acid]

# confirm all positive
mc0[rval < 0]
# Empty data.table (0 rows and 21 cols): acnm,experiment.date,plate.id,spid,treatment,apid...

# confirming that there are some apid with no p wells with wllq == 0
mc0[wllt == "p", .(num_p = sum(wllq==1)), by = "apid"][num_p == 0]
# apid num_p
# 1: 20190423     0

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
# apparently the 6 GF compounds have not been registered, so need to remove those as well
mc0 <- mc0[!(spid %in% c("EX000371", "EX000372", "EX000373", "EX000374", "EX000408", "EX000411"))]
mc0 <- mc0[, .(acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)] # getting rid of the extra "attributes" stuff
tcplConfList() # sbox confirmed
tcplWriteLvl0(mc0, type = "mc") # July 25, 2020 12:23pm
# Completed delete cascade for 1 ids (81.49 secs)
# [1] TRUE

# REGISTER/CONFRIM METHODS -----------------------------------------
mc0[wllt == "n",.N]
# [1] 879

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

# July 25, 2020 12:41pm
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")
# Did not work!!
# Loaded L2 ACID2446 (13283 rows; 34.55 secs)
# Warning in FUN(X[[i]], ...) :
#   AEID(S) 2439 (mapped to ACID2446) contain NA in the response column. Level 3 processing incomplete; no updates
# made to the mc3 table for ACID2446.
# Writing level 3 data for 0 ids...
# Writing level 3 complete. (0 secs)
# Warning in tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc") :
#   Pipeline stopped early at level 3; processing errors occured with all given acids by level 3.
# 
# 
# Total processing time: 4.37 mins 
# 
# $l1
# ACID2446 
# TRUE 
# 
# $l1_failed
# character(0)
# 
# $l2
# ACID2446 
# TRUE 
# 
# $l2_failed
# character(0)
# 
# $l3
# ACID2446 
# FALSE 
# 
# $l3_failed
# [1] "ACID2446"

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
description <- paste0("Testing LDH when all 'p' wells have wllq=0 on some apid (see 20190423).")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
save(description, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))
