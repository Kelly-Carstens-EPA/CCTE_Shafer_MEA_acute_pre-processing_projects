# Re-running updated normalization method for down endpoints
# with CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean_dn included

# with (cval - bval)/(-100 - bval)*100 for dn,
# (cval - bval)/(100 - bval)*100 for up

# script to run MEA acute lvl 0 data
library(tcpl)
library(data.table)
library(RMySQL)
setwd('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl')
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# REVIEW REGISTRATION>UPDATE --------------------------------------------------------------------------------

acnm.tb <- tcplLoadAcid(fld = 'asid', val = 20)
acnm.tb[grepl('acute',acnm)]
# So we have both
# CCTE_Shafer_MEA_acute_bursting_electrodes_number_mean
# and 
# CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean
# ... hmmm...

# yep, it's current CCTE_Shafer_MEA_acute_bursting_electrodes_number_mean
# should be CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean_dn
aenm.tb <- tcplLoadAeid(fld = 'acid', val = 2441)
aenm.tb

# what is in the current level 0 snapshot?
load('lvl0_snapshots/mea_acute_lvl0_2020-07-29.Rdata')
mea_acute_lvl0[, .N, by = .(acnm)]
mea_acute_lvl0[grepl('acute_bursting_electrodes_number_mean',acnm), unique(acnm)]
# empty
mea_acute_lvl0[grepl('acute_per_network_burst_electrodes_number_mean',acnm), unique(acnm)]
# [1] "CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean"
# okay... so I guess since I ran this previously with the deprecated aenm list

# Get the current list of compounds to normalize by
main15_tb <- as.data.table(read.csv('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/mea_acute_main15_acnm_aenm_2020-12-08.csv',stringsAsFactors = F))

setdiff(main15_tb$aenm, tcplLoadAeid(fld = 'asid', val = 20)$aenm)
# empty
setdiff(sub('_dn','',grep('dn',main15_tb$aenm,val=T)), mea_acute_lvl0$acnm)
# empty
length(intersect(mea_acute_lvl0$acnm, sub('_dn','',grep('dn',main15_tb$aenm,val=T))))
# 15 - all 15 endpoints are in teh lvl0 snapshot


# REGISTER NEW ACID/AEID --------------------------------------------------------------------------------



# PREPARE MC0 ------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
load("lvl0_snapshots/mea_acute_lvl0_2020-07-29.RData")

# Assign acid's
acid.acnm <- tcplLoadAcid(fld = "acnm", val = sub('_dn','',grep('dn',main15_tb$aenm,val=T)))
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm, by = "acnm")
mea_acute_lvl0[, acnm := NULL]
mea_acute_lvl0[, length(unique(acid))] # 15

# I have to remove a few compounds that are not registered
# Are these still not present?
tcplLoadChem(field = 'spid', val = c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01",
                                     "EX000371", "EX000372", "EX000373", "EX000374", "EX000408", "EX000411"))
# Empty data.table (0 rows and 5 cols): spid,chid,casn,chnm,dsstox_substance_id
# Warning message:
#   In tcplLoadChem(field = "spid", val = c("EPAPLT0167A11", "EPAPLT0167D11",  :
#                                             The given spid(s) are not in the tcpl database.
mc0 <- mea_acute_lvl0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01",
                                    "EX000371", "EX000372", "EX000373", "EX000374", "EX000408", "EX000411"))]
rm(mea_acute_lvl0)

# Which methods to use...
tcplMthdList(lvl=3L, type = 'mc')
# could use method 41...

# What if I add my own dummy p wells?
pdummyrow <- mc0[1:length(unique(mc0$acid)),]
pdummyrow$spid <- 'Dummy p well'
pdummyrow$rval <- -100
pdummyrow$acid <- unique(mc0$acid)
pdummyrow$wllq <- 1
pdummyrow$srcf <- 'placeholder_minus_100'
# apid, row, col, conc, srcf all irrelevant

# Remove teh current p value wells
mc0 <- mc0[wllt != 'p']
mc0 <- rbind(mc0, pdummyrow)
mc0[, max(rval[wllt == 'p']), by = .(acid)]
# all -100

# close any connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList() # confirming db
tcplWriteLvl0(mc0, type = "mc") # Mar 26, 2020 5:32pm
# Completed delete cascade for 30 ids (171.72 secs)
# [1] TRUE
# (lasted ~ 45 minutes)


# REGISTER/CONFIRM METHODS -----------------------------------------
acids <- unique(mc0$acid)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # none for all


# lvl 3
dn.mea <- aeid.info[grepl("_dn",aenm), aeid]
tcplMthdLoad(lvl=3, id = dn.mea)
tcplMthdClear(lvl=3, id = dn.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = dn.mea, mthd_id = c(1,17,41,5), ordr = c(1:4), type = "mc") # none for all
tcplMthdLoad(lvl=3, id = dn.mea)

up.mea <- aeid.info[grepl("_up",aenm) & !(grepl("(LDH)|(AB)",aenm)), aeid]
tcplMthdLoad(lvl=3, id = up.mea)
tcplMthdClear(lvl=3, id = up.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = up.mea, mthd_id = c(1,17,41,5,6), ordr = c(1:5), type = "mc") # initialize resp, then multiply by -1
tcplMthdLoad(lvl=3, id = up.mea)

# lvl 4
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
# bmad.aeid.lowconc.nwells

# lvl 5
tcplMthdLoad(lvl=5L, id = aeid.info$aeid)
# bmad3

# lvl 6
mc6.mthds <- tcplMthdLoad(lvl=6L, id = aeid.info$aeid)
mc6.mthds[, .N, by = .(aeid)] # 10 for all
tcplMthdList(lvl=6L, 'mc')
# # tcplMthdClear(lvl = 6L, id = aeid.info$aeid, type = "mc")
# aeid.new <- setdiff(aeid.info$aeid, tcplMthdLoad(lvl=6L, id = aeid.info$aeid)[, unique(aeid)])
# lvl6_mthds <- tcplMthdList(lvl = 6L, type = "mc")
# tcplMthdAssign(lvl = 6L, id = aeid.new, mthd_id = lvl6_mthds[mc6_mthd != "viability.gnls",mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
# # tcplMthdAssign(lvl = 6L, id = c(dn.mea, up.mea), mthd_id = lvl6_mthds[mc6_mthd != "viability.gnls",mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
# # tcplMthdAssign(lvl = 6L, id = c(alamarblue, ldh), mthd_id = lvl6_mthds[,mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
# tcplMthdLoad(lvl=6L, id = aeid.info$aeid)

# view all 
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = aeid.info$aeid))
}


# ----------------------------------- tcpl run with hack function
rm(list = setdiff(ls(),'acids'))
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "acid", val= acids)

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# Mar 26, 2021 6:30 pm
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# yay!

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# Save data and plots ----------------------------

assay.list <- tcplLoadAeid(fld = "acid", val=assay.list$acid)
# save the data, see some plots
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/")
for (i in assay.list[grepl('dn',aenm),aeid]){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}

for (i in assay.list[grepl('dn',aenm),aeid][2:15]){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))

# get mc3, add spid, apid, rowi, coli, etc from mc0, add cndx, repi, cval from mc1, add cval from mc2
aeids <- assay.list
mc3 <- dbGetQuery(con, paste0("SELEct m0id, aeid, acid, bval, pval, logc, resp FROM mc3 WHERE aeid IN (",paste0(aeids$aeid,collapse=","),")"))
setDT(mc3)
mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
mc3 <- merge(mc3, mc1, all.x = T)
mc2 <- dbGetQuery(con, paste0("SELEct m0id, acid, cval FROM mc2 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
mc3 <- merge(mc3, mc2, all.x = T)
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = unique(assay.list$acid), type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, srcf, rval)], all.x = T)

# get mc5_mc6 data
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=aeids$aeid, type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=assay.list$aeid, type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), mc6_mthd = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)

# get the methods
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = assay.list$aeid, "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = assay.list$aeid, "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = assay.list$aeid, "mc")

# save the data
sbox_run_summary <- paste0("Created dummy p-rows with rval=-100. Normalized at level 3 with resp.pc. Up endpoints resp.multneg1.
\nIncluded a previously dropped endpoint (CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean_dn).
                           \nUsing mea_acute_lvl0_2020-07-29.Rdata. Date ran: 2021-03-26. Date saved:",as.character.Date(Sys.Date()),".")
save(sbox_run_summary, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, 
     file = file.path(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData")))
save(sbox_run_summary, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6,
     file = file.path(paste0('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/normalization_ideas_Dec2020/sbox_run/sbox_dat_',as.character.Date(Sys.Date()),'.RData')))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))
