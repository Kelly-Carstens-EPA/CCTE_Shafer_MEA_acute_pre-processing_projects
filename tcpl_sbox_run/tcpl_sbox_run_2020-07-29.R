# TO ELETEDL:
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
# changing acnm prefix
# the "hack function"
# saving plots, final comparisons


# script to run MEA acute lvl 0 data
library(tcpl)
library(data.table)
library(RMySQL)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# REVIEW REGISTRATION>UPDATE --------------------------------------------------------------------------------

tcplLoadAcid(fld = "acnm", val = "NHEERL_MEA_acute_cross_correlation_area")
# acnm acid
# 1: NHEERL_MEA_acute_cross_correlation_area 2443
tcplUpdate(what = "acid", id = 2443, flds = list(assay_component_name = "CCTE_Shafer_MEA_acute_cross_correlation_area"))
# TRUE
tcplLoadAcid(fld = "acid", val = 2443, add.fld = "acsn")
# acid                                     acnm                                    acsn
# 1: 2443 CCTE_Shafer_MEA_acute_cross_correlation_area NHEERL_MEA_acute_cross_correlation_area
# ayyy, so fast and easy!!
tcplLoadAeid(fld = "acid", val = 2443, add.fld = c("acnm","aenm"))
# acid aeid                                       aenm                                     acnm
# 1: 2443 2436 NHEERL_MEA_acute_cross_correlation_area_up CCTE_Shafer_acute_cross_correlation_area
# 2: 2443 2453 NHEERL_MEA_acute_cross_correlation_area_dn CCTE_Shafer_acute_cross_correlation_area

# Oh wow, the acnm is already updated here too!!
tcplUpdate(what = "aeid", id = c(2436, 2453), flds = list(assay_component_endpoint_name = c("CCTE_Shafer_MEA_acute_cross_correlation_area_up","CCTE_Shafer_MEA_acute_cross_correlation_area_dn")))
tcplLoadAeid(fld = "acid", val = 2443, add.fld = c("acnm","aenm"))
# acid aeid                                            aenm                                     acnm
# 1: 2443 2436 CCTE_Shafer_MEA_acute_cross_correlation_area_up CCTE_Shafer_acute_cross_correlation_area
# 2: 2443 2453 CCTE_Shafer_MEA_acute_cross_correlation_area_dn CCTE_Shafer_acute_cross_correlation_area


# REGISTER NEW ACID/AEID --------------------------------------------------------------------------------

tcplLoadAsid(fld = "asid", val = 20)
tcplLoadAid(fld = "asid", val = 20)
# asid aid        anm
# 1:   20 685 NHEERL_MEA
# meh, I won't worry about updating these

# update everythign that is already registered
registered.acids <- tcplLoadAcid(fld = "asid", val = 20)[grepl("acute",acnm)]
tcplUpdate(what = "acid", id = registered.acids$acid, flds = list(assay_component_name = sub("NHEERL","CCTE_Shafer",registered.acids$acnm)))
tcplUpdate(what = "acsn", id = registered.acids$acid, flds = list(acsn = sub("NHEERL","CCTE_Shafer",registered.acids$acnm)))
# Warning message:
#   In tcplUpdate(what = "acsn", id = registered.acids$acid, flds = list(acsn = sub("NHEERL",  :
#   Error updating the following ids: 2431, 2432, 2433, 2434, 2435, 2436, 2437, 2438, 2439, 2440, 2441, 2442, 2443, 2444, 2445, 2446, 2447
# not sure why this won't update, but doesn't really matter

registered.aeids <- tcplLoadAeid(fld = "asid", val = 20)[grepl("acute",aenm)]
tcplUpdate(what = "aeid", id = registered.aeids$aeid, flds = list(assay_component_endpoint_name = sub("NHEERL","CCTE_Shafer",registered.aeids$aenm)))

# what is the normalized data type right now?
tcplLoadAeid(fld = "aeid", val = registered.aeids$aeid, add.fld = "normalized_data_type")
# all percent_activity
# just read in teh SOP that this defaults to percent_Activity, and the only 
# other 2 opttions are "log2_fold_induction" and "log10_fold_induction"
# let's stick with percent_activity

# identify new acnm
new_acnm <- setdiff(unique(mea_acute_lvl0$acnm), tcplLoadAcid(fld = "acnm", val = unique(mea_acute_lvl0$acnm))$acnm)
tcplRegister(what = "acid", flds = list(aid = , acnm = ""))

new.acid.info <- as.data.table(list(asid = rep(20,length(new_acnm)), aid = rep(685,length(new_acnm)), acnm = new_acnm))
tcplConfList() # sbox confrimed
for (i in 1:nrow(new.acid.info)){
  #Assay Component Registration
  tcplRegister(what="acid", flds=list(aid= new.acid.info$aid[i], 
                                      acnm = new.acid.info$acnm[i]))
  
  # acsn registreation
  acid <- tcplLoadAcid(fld="acnm",val= new.acid.info$acnm[i])$acid
  tcplRegister(what="acsn", flds=list(acid= acid, acsn = new.acid.info$acnm[i]))
  
  # aeid registration
  tcplRegister(what = "aeid", flds = list(acid = rep(acid,2), 
                                          aenm = paste0(new.acid.info$acnm[i],c("_up","_dn")), 
                                          normalized_data_type = rep("percent_activity",2)))
}

# check it out
tcplLoadAcid(fld = "acnm", val = new.acid.info$acnm, add.fld = "acsn")
# yep, all there!
tcplLoadAeid(fld = "acnm", val = new.acid.info$acnm, add.fld = c("normalized_data_type"))
# looks great!

# oops... I just realized that I registered as a new acnm
# instead of updating _.
# oh well, it can just be a new acnm for the sbox


# PREPARE MC0 ------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
load("lvl0_snapshots/mea_acute_lvl0_2020-07-28.RData")

# Assign acid's
acid.acnm <- tcplLoadAcid(fld = "acnm", val = unique(mea_acute_lvl0$acnm))
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm, all.x = T, by = "acnm")
mea_acute_lvl0[, acnm := NULL]

# I don't want this to take forever adn take up a lot of memory
# So I am randomly goign to remove half of the apid
nrow(mea_acute_lvl0) # 639990
mea_acute_lvl0[, apid := as.character(apid)]
all_apids <- mea_acute_lvl0[, unique(apid)]
use_apids <- all_apids[seq(1,length(all_apids),by = 2)]
mc0 <- mea_acute_lvl0[apid %in% use_apids, .(acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)]
nrow(mc0) # 324270
mc0[wllt == "n", .N]

# I have to remove a few compounds that are not registered
mc0 <- mc0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01",
                         "EX000371", "EX000372", "EX000373", "EX000374", "EX000408", "EX000411"))]
nrow(mc0) # 316710

# close any connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConfList() # confirming db
tcplWriteLvl0(mc0, type = "mc") # July 07/28/2020 11:56pm


# REGISTER/CONFIRM METHODS -----------------------------------------
acids <- unique(mea_acute_lvl0$acid)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # totally empty
# tcplMthdClear(lvl=2L, id = acids, type = "mc") # this is what froze for 3 hrs last time!
tcplMthdAssign(lvl = 2L, id = acids, mthd_id = 1, ordr = c(1), type = "mc") # all none for lvl 2
tcplMthdLoad(lvl=2, id = acids) # yay, all 45 are none!

# lvl 3
dn.mea <- aeid.info[grepl("_dn",aenm), aeid]
tcplMthdLoad(lvl=3, id = dn.mea)
# tcplMthdClear(lvl=3, id = dn.mea, type = "mc")
dn.mea.new <- setdiff(dn.mea, tcplMthdLoad(lvl=3L, id = dn.mea, type="mc")[, unique(aeid)])
tcplMthdAssign(lvl = 3, id = dn.mea.new, mthd_id = c(17, 35, 6), ordr = c(1:3), type = "mc")
tcplMthdLoad(lvl=3, id = dn.mea)
# great! 43 dn.mea * 3 mthds = 129 data rows, check.
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 35                       resp.logfc                        Calculate the response as a fold change over baseline for logged values
# 6                    resp.multneg1                                                                    multiply the response by -1

up.mea <- aeid.info[grepl("_up",aenm) & !(grepl("(LDH)|(AB)",aenm)), aeid]
tcplMthdLoad(lvl=3, id = up.mea)
# tcplMthdClear(lvl=3, id = up.mea, type = "mc")
up.mea.new <- setdiff(up.mea, tcplMthdLoad(lvl=3L, id =up.mea,type="mc")[, unique(aeid)])
tcplMthdAssign(lvl = 3, id = up.mea.new[1], mthd_id = c(17,35), ordr = c(1:2), type = "mc")
tcplMthdAssign(lvl = 3, id = up.mea.new[2:length(up.mea.new)], mthd_id = c(17,35), ordr = c(1:2), type = "mc")
# great!
# I would really like to clear the pval methods.. for the curently registered endpoints.. but eh, not worth it
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 35                       resp.logfc                        Calculate the response as a fold change over baseline for logged values

alamarblue <- aeid.info[grepl("AB",aenm), aeid]
tcplMthdLoad(lvl=3, id = alamarblue)
# tcplMthdClear(lvl=3, id = alamarblue, type = "mc")
tcplMthdAssign(lvl = 3, id = alamarblue, mthd_id = c(17,32,5), ordr = c(1:3), type = "mc")
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 32                        pval.zero                                                                                 Set pval to 0.
# 5                          resp.pc                                                                      response percent activity

ldh <- aeid.info[grepl("LDH",aenm), aeid]
tcplMthdLoad(lvl=3, id = ldh)
# tcplMthdClear(lvl=3, id = ldh, type = "mc")
tcplMthdAssign(lvl = 3, id = ldh, mthd_id = c(17,13,5), ordr = c(1:3), type = "mc")
# 17       bval.apid.nwllslowconc.med                  Take the median cval of the n wells and the first two concentrations, by apid
# 13              pval.apid.pwlls.med                                       plate-wise median based on positive control, single dose
# 5                          resp.pc                                                                      response percent activity

# lvl 4
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
# tcplMthdClear(lvl=4L, id = aeid.info$aeid, type = "mc")
aeid.new <- setdiff(aeid.info$aeid, tcplMthdLoad(lvl=4L, id = aeid.info$aeid)[, unique(aeid)])
tcplMthdAssign(lvl=4L, id = aeid.new, mthd_id = c(2), ordr = c(1), type = "mc")
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
# bmad.aeid.lowconc.nwells

# lvl 5
tcplMthdLoad(lvl=5L, id = aeid.info$aeid)
# tcplMthdClear(lvl=5L, id = aeid.info$aeid, type = "mc")
aeid.new <- setdiff(aeid.info$aeid, tcplMthdLoad(lvl=5L, id = aeid.info$aeid)[, unique(aeid)])
tcplMthdAssign(lvl=5L, id = aeid.new, mthd_id = c(1), ordr = c(1), type = "mc")
# bmad3

# lvl 6
tcplMthdLoad(lvl=6L, id = aeid.info$aeid)
# tcplMthdClear(lvl = 6L, id = aeid.info$aeid, type = "mc")
aeid.new <- setdiff(aeid.info$aeid, tcplMthdLoad(lvl=6L, id = aeid.info$aeid)[, unique(aeid)])
lvl6_mthds <- tcplMthdList(lvl = 6L, type = "mc")
tcplMthdAssign(lvl = 6L, id = aeid.new, mthd_id = lvl6_mthds[mc6_mthd != "viability.gnls",mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
# tcplMthdAssign(lvl = 6L, id = c(dn.mea, up.mea), mthd_id = lvl6_mthds[mc6_mthd != "viability.gnls",mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
# tcplMthdAssign(lvl = 6L, id = c(alamarblue, ldh), mthd_id = lvl6_mthds[,mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
tcplMthdLoad(lvl=6L, id = aeid.info$aeid)

# view all 
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = aeid.info$aeid))
}


# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "asid", val= 20)[grepl("acute",acnm) , ]

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# June 29, 12:43am 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# July 29, 9:50am - stopped right towards the end of level 2
# Loaded L1 ACID2946 (7032 rows; 25.46 secs)
# Processed L2 ACID2946 (5757 rows; 0.05 secs)
# Error in .local(conn, statement, ...) : 
#   could not run statement: Lost connection to MySQL server during query
# Called from: .local(conn, statement, ...)
# Browse[1]> 

# let's see how much made it into the db
con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
mc2 <- dbGetQuery(con, "SELECT * FROM mc2 WHERE acid=2432 LIMIT 5")
# empty
# I think we just have to re-run from level 1
dbDisconnect(con)

# close any connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())
tcplConfList()

# June 29, 9:56am 
tcplRun(slvl = 2L, elvl = 6L, id = unique(assay.list$acid), type = "mc")
# (at 3:13 pm, approximated that delete cascade finised at 1:13 pm)
# Loaded L1 ACID2960 (7032 rows; 21.58 secs)
# Processed L2 ACID2960 (5656 rows; 0.07 secs)
# Writing level 2 data for 46 ids...
# Completed delete cascade for 90 ids (10825.7 secs)
# Error: Lost connection to MySQL server during query [2013]
# I'm giving up, for now

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
