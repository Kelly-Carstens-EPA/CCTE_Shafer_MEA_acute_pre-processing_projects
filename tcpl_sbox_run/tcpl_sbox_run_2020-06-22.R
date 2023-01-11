# 06/22/2020
# what to compare results of finding bval by DMSO + cndx 1 onlye
# versus DMSO + cndx1&2
# since there is no lvl3 method for this, 
# I am going to add in the cndx 1 t compounds as exra "n" wells
# while keeping the original cndx 1 data in the mc0 table
library(tcpl)
library(data.table)
library(RMySQL)
# tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList()
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")


# PREPARE MC0 ------------------------------------------------------
acids <- c(2432, 2433, 2445, 2446, 2447) # the acid's ran in the previus run 06/19
# mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = 2432, type = "mc")
con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
tcplListFlds("mc1")
mc1 <- dbGetQuery(con, paste0("SELECT m0id, acid, cndx, repi FROM mc1 WHERE acid IN(",paste0(acids,collapse=","),")"))
setDT(mc1)
tcplListFlds("mc0")
# mc0 <- dbGetQuery(con, paste0("SELECT m0id FROM mc0 WHERE acid=2447 LIMIT 5")) # it worked!!!
# mc0 <- dbGetQuery(con, paste0("SELECT m0id, acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf FROM mc0 WHERE acid IN(2432) LIMIT 10")) # it worked!
mc0 <- dbGetQuery(con, paste0("SELECT m0id, acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf FROM mc0 WHERE acid IN(",paste0(acids,collapse=","),")"))
setDT(mc0)

# get cdnx1 data as wllt "n"
cndx1 <- mc1[cndx == 1]
cndx1 <- merge(cndx1, mc0, all.x = T)
cndx1.twells <- cndx1[wllt == "t"]
cndx1.twells[, wllt := "n"]
cndx1.twells[, spid := "test_cndx1"]
cndx1.twells[, c("cndx","repi") := list(NULL)]
mc0 <- rbind(mc0, cndx1.twells)
mc0[, m0id := NULL]

# Assign acid's from sbox

# write it!
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConfList()
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# Need to remove these, since not present in sbox
mc0 <- mc0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01"))]
# let's only do it for Alamar Blue first
tcplLoadAcid(fld = "acid", val = acids)
mc0 <- mc0[acid == 2447]
mc0 <- mc0[, .(acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)] # getting rid of the extra "attributes" stuff
tcplConfList() # sbox confirmed
tcplWriteLvl0(mc0, type = "mc") # June 22, 2020 1:11pm
# 3 hours later... it worked!!!
# Completed delete cascade for 1 ids (10919.5 secs)
# [1] TRUE


# REGISTER/COnfRIM METHODS -----------------------------------------
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
# acids <- c(2432, 2433, 2445, 2446, 2447)
acids <- c(2447)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # all none for lvl 2

# lvl 3 - just use "n" wells, with the cheat
tcplMthdLoad(lvl = 3L, id = aeid.info$aeid, type = "mc")
# aeid                       mthd mthd_id ordr
# 1: 2440 bval.apid.nwllslowconc.med      17    1
# 2: 2440                  pval.zero      32    2
# 3: 2440                    resp.pc       5    3
tcplMthdClear(lvl = 3L, id = aeid.info$aeid, type = "mc")
tcplMthdAssign(lvl = 3L, id = aeid.info$aeid, mthd_id = c(11, 32, 5), ordr = c(1:3), type = "mc") 
# set bval as median apid n well, pval.zero, resp.pc

# view 4 - 6
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = aeid.info$aeid))
}

# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "acid", val=c(2447))

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# June 22, 2020 4:24pm 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# Save data and plots ----------------------------

assay.list <- tcplLoadAeid(fld = "acid", val=c(2447))
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
mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
nrow(mc3)
mc3 <- merge(mc3, mc1, all.x = T)
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = unique(assay.list$acid), type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, srcf)], all.x = T)
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=assay.list$aeid, type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=assay.list$aeid, type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = assay.list$aeid, "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = assay.list$aeid, "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = assay.list$aeid, "mc")
description <- paste0("Comparining bval by 'n' and cndx 1 only, versus 'n' and cndx 1&2. For AB only")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
save(description, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))

# ---------------------------

# comapre results with results when used cndx 1 and 2
mc5_mc6.1 <- mc5_mc6
mc3.1 <- mc3
load("sbox_dat/sbox_dat_2020-06-21.RData") # created in script dated 6-19. Ran to test updated LDH wllt, NA conc's.
# so the AB data is correct
mc3_mthds
# 2440 bval.apid.nwllslowconc.med      17    1
# 11: 2440                  pval.zero      32    2
# 12: 2440                    resp.pc       5    3
# yep, has previous bval method

# compare hit calls!
nrow(mc5_mc6.1) # 510
usecols <- c("aenm","spid","dsstox_substance_id","casn","hitc","aeid","modl_ga")
cmc5 <- merge(mc5_mc6.1[, ..usecols], mc5_mc6[,..usecols], by = setdiff(usecols,c("hitc","modl_ga")), suffixes = c(".new",".org"))
nrow(cmc5) # 509 - my dummy spid "test_control" probs got dropped

cmc5[hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 501
cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 1
cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1] # just looked, this is a borderlien hit in new graph
mc3.1[spid == "TP0001414E11" & cndx == 7] # 2 points right on top of each other at highest conc
# m0id acid aeid     bval pval logc     resp cndx repi         spid     apid rowi coli wllt wllq conc                      srcf
# 1: 528039302 2447 2440 32927.33    0    1 37.87633    7    1 TP0001414E11 20160419    3    2    t    1   10 Conc_Response_Log_JS.xlsx
# 2: 528039308 2447 2440 32927.33    0    1 37.41167    7    2 TP0001414E11 20160419    3    2    t    1   10 Conc_Response_Log_JS.xlsx
# great agreement...
# just realized something - the coff is based on n wells... and I just added a lot to the "n" wells.

# let's see how different the coff's are:
mc5_mc6.1[, unique(coff)] # 37.02672
mc5_mc6[grepl("AB",aenm), unique(coff)] # 39.2768
# oh wow! So the coff is actually lowered when including cndx1...

# okay, so I did this test because I/Kathleen suspected that we might have more hit calls if we don't include the 
# we know that this added hit call resulted because the hill became the winning model.
# and the max med just snuck above coff
# If the coff was only based on the true "n" wells, then this compound would not have been a hit

# what are the changes in potency?
cmc5[hitc.org == 1 & hitc.new == 1] # only 21 shared hit calls
cmc5[hitc.org == 1 & hitc.new == 1, summary(modl_ga.new - modl_ga.org)]
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.047220 -0.002733  0.001382 -0.001081  0.009551  0.043372 
# very, very tight!!
# correlation plot:
plot(modl_ga.new ~ modl_ga.org,cmc5[hitc.org == 1 & hitc.new == 1], ylab = "log10(AC50) - DMSO + 1st lowest conc only used to normalize",
     xlab = "log10(AC50) - DMSO + 1st and 2nd lowest conc's used to normalize", main = "Correlation Plot of log10(AC50) with Different Normalization Approaches")
abline(a = 0, b = 1)
boxplot(cmc5[hitc.org == 1 & hitc.new == 1, modl_ga.new - modl_ga.org])

# what about compounds re-tested at lower conc's?
mc5_mc6.1[chnm == "Emamectin benzoate"]
# eh.
