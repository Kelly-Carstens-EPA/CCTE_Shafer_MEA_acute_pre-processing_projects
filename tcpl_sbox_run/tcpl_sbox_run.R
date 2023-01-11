# practice pipelining mea acute for mfr, LDH, and AB
library(tcpl)
library(data.table)
library(RMySQL)
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList()
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")

# LOAD MY DATA TO WRITE------------------------
load("DNT2019/output/DNT2019_mc0_2020-06-10.RData")
load("APCRA2019/output/APCRA2019_mc0_2020-06-10.RData")
load("ToxCast2016/output/ToxCast2016_mc0_2020-06-10.RData")
mc0 <- rbind(ToxCast2016_mc0, DNT2019_mc0, APCRA2019_mc0)
rm(list = c("ToxCast2016_mc0", "DNT2019_mc0", "APCRA2019_mc0"))

# just get the 3 endpoints that I want to pipeline, for now
mc0 <- mc0[grepl("(LDH)|(AB)|(firing)",acsn)]
tcplWriteLvl0(mc0, type = "mc")

str(mc0)
# wllt, apid, spid, and srcf are all characters

mc0[is.na(rval), .N, by = "wllq"] # all NA wells have wllq=0 

# Assign acid's from sbox
acid.info <- tcplLoadAcid(fld = "asid", val = 20, add.fld = "acsn")
(mc0 <- merge(mc0, acid.info, by = "acsn"))
mc0[, unique(acnm), by = c("acsn","acid")] # gut check confirmation
mc0[, c("acsn","acnm","asid") := list(NULL, NULL, NULL)]

# write it!
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConfList()
names(mc0)
tcplWriteLvl0(mc0, type = "mc") # June 10, 2020 9:53pm
# The following test compounds did not map to the tcpl databases:
#   [1] "EPAPLT0167A11" "EPAPLT0167D11" "EPAPLT0154A05" "EPAPLT0154C04" "EPAPLT0154F01"
# Error in tcplWriteLvl0(mc0, type = "mc") : 
#   Must correct the test compound mapping before loading the data.
# right, I need to remove these. Might want to mention to Katie as well
mc0 <- mc0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01"))]
tcplWriteLvl0(mc0, type = "mc") # June 10, 2020 9:55pm
# Completed delete cascade for 4 ids (88 secs)
# [1] TRUE

# REGISTER/COnfRIM METHODS -----------------------------------------
# mean firing rate
tcplLoadAcid(fld = "acnm", val= "NHEERL_MEA_acute_firing_rate_mean")
# acnm acid
# 1: NHEERL_MEA_acute_firing_rate_mean 2432
tcplMthdLoad(lvl = 2L, id = 2431:2447) # all are none for lvl 2

tcplLoadAeid(fld = "acnm", val= "NHEERL_MEA_acute_firing_rate_mean")
# acnm aeid                                 aenm
# 1: NHEERL_MEA_acute_firing_rate_mean 2425 NHEERL_MEA_acute_firing_rate_mean_up
# 2: NHEERL_MEA_acute_firing_rate_mean 2442 NHEERL_MEA_acute_firing_rate_mean_dn
tcplLoadAeid(fld = "acnm", val= "NHEERL_MEA_acute_AB")
# acnm aeid                   aenm
# 1: NHEERL_MEA_acute_AB 2440 NHEERL_MEA_acute_AB_up
tcplLoadAeid(fld = "acnm", val= "NHEERL_MEA_acute_LDH")
# acnm aeid                    aenm
# 1: NHEERL_MEA_acute_LDH 2439 NHEERL_MEA_acute_LDH_up

for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = c(2425, 2442, 2440, 2439)))
}

# for first round, I will do minimal normalization/zero-centering
# leaving LDH with none
tcplMthdClear(lvl = 3L, id = 2440, type = "mc")
tcplMthdList(lvl = 3L, type = "mc")
# tcplMthdAssign(lvl = 3L, id = 2440, mthd_id = c(17, 32, 5), ordr = c(1:3), type = "mc") # bval of n and low cndx t wells, pval to zero, resp.pc
tcplMthdAssign(lvl = 3L, id = 2440, mthd_id = c(17, 9), ordr = c(1:2), type = "mc") # bval of n and low cndx t wells, resp.fc (cval/bval)
tcplMthdAssign(lvl = 3L, id = 2440, mthd_id = c(6), ordr = c(3), type = "mc") # multiply by -1
tcplMthdLoad(lvl = 3L, id = 2440, type = "mc")

# lvl 4, all are
# bmad.aeid.lowconc.nwells       2

# lvl 5, all are
# bmad3       1

# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "aeid", val=c(2425, 2442, 2440, 2439), add.fld = "acid")

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# June 10, 2020 10:33pm 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")
# completed 6/10/2020

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")

# save the data, see some plots
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/")
for (i in unique(assay.list$aeid)){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}

# 06/11/2020
con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
dbGetQuery(con, "DESC mc3")
mc3 <- dbGetQuery(con, "SELEct m0id, aeid, acid, bval, logc, resp FROM mc3 WHERE aeid IN (2425, 2442, 2440, 2439)")
setDT(mc3)
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=c(2425, 2442, 2440, 2439), type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=c(2425, 2442, 2440, 2439), type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = c(2425, 2442, 2440, 2439), "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = c(2425, 2442, 2440, 2439), "mc")
dir.create(path = paste0(getwd(), "/sbox_dat"))
save(mc3_mthds, mc4_mthds, mc3, mc5_mc6, file = "sbox_dat/sbox_dat_2020-06-10.RData")
dbDisconnect(con)
rm(list = c("mc3","mc5","mc6","mc6_collapsed"))


# trying next:
# - subtract bval from cval for LDH
# - calculate resp.pc for AB

# REGISTER/COnfRIM METHODS -----------------------------------------
tcplConfList()
# mean firing rate - let's try not zero-centering, just to see how it looks
tcplLoadAeid(fld = "acnm", val= "NHEERL_MEA_acute_firing_rate_mean")
# acnm aeid                                 aenm
# 1: NHEERL_MEA_acute_firing_rate_mean 2425 NHEERL_MEA_acute_firing_rate_mean_up
# 2: NHEERL_MEA_acute_firing_rate_mean 2442 NHEERL_MEA_acute_firing_rate_mean_dn
tcplMthdClear(lvl = 3L, id = c(2425, 2442), type = "mc")
tcplMthdAssign(lvl = 3L, id = 2442, mthd_id = c(1,6), ordr = c(1,2), type = "mc") # multi -1
tcplMthdAssign(lvl = 3L, id = 2425, mthd_id = c(1), ordr = c(1), type = "mc") # none

# Cell Titer Blue
tcplLoadAeid(fld = "acnm", val= "NHEERL_MEA_acute_AB")
# acnm aeid                   aenm
# 1: NHEERL_MEA_acute_AB 2440 NHEERL_MEA_acute_AB_up
tcplMthdClear(lvl = 3L, id = 2440, type = "mc")
tcplMthdAssign(lvl = 3L, id = 2440, mthd_id = c(17, 32, 5), ordr = c(1:3), type = "mc") # bval, pval.zero, resp.pc

# LDH
tcplLoadAeid(fld = "acnm", val= "NHEERL_MEA_acute_LDH")
# acnm aeid                    aenm
# 1: NHEERL_MEA_acute_LDH 2439 NHEERL_MEA_acute_LDH_up
tcplMthdClear(lvl = 3L, id = 2439, type = "mc")
tcplMthdAssign(lvl = 3L, id = 2439, mthd_id = c(17, 32, 35), ordr = c(1:3), type = "mc") # bval n and low conc med, pval.zero, resp.logfc

for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = c(2425, 2442, 2440, 2439)))
}
# aeid                       mthd mthd_id ordr
# 1: 2425                       none       1    1
# 2: 2439 bval.apid.nwllslowconc.med      17    1
# 3: 2439                  pval.zero      32    2
# 4: 2439                 resp.logfc      35    3
# 5: 2440 bval.apid.nwllslowconc.med      17    1
# 6: 2440                  pval.zero      32    2
# 7: 2440                    resp.pc       5    3
# 8: 2442                       none       1    1
# 9: 2442              resp.multneg1       6    2

# lvl 4, all are
# bmad.aeid.lowconc.nwells       2

# lvl 5, all are
# bmad3       1

# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "aeid", val=c(2425, 2442, 2440, 2439), add.fld = "acid")

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# June 11, 2020 10:38am
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")
# completed 6/11/2020

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")

# save the data, see some plots
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/")
for (i in unique(assay.list$aeid)){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}

# save the data
con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
dbGetQuery(con, "DESC mc3")
mc3 <- dbGetQuery(con, "SELEct m0id, aeid, acid, bval, logc, resp FROM mc3 WHERE aeid IN (2425, 2442, 2440, 2439)")
setDT(mc3)
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=c(2425, 2442, 2440, 2439), type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=c(2425, 2442, 2440, 2439), type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = c(2425, 2442, 2440, 2439), "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = c(2425, 2442, 2440, 2439), "mc")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
save(mc3_mthds, mc4_mthds, mc3, mc5_mc6, file = "sbox_dat/sbox_dat_2020-06-11.RData")
dbDisconnect(con)
rm(list = c("mc3","mc5","mc6","mc6_collapsed"))

mc5_mc6[grepl("AB",aenm), .N, by = "hitc"]
# hitc   N
# 1:   -1   7
# 2:    0 481
# 3:    1  21

# comapre results with what was previously pipelined
mc5_org <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/output/01APR2020/mc5_mc6_nheerl_mea_acute.csv", stringsAsFactors = FALSE)
setDT(mc5_org)

names(mc5_org)
length(unique(mc5_org$spid))*32
nrow(mc5_org)
usecols <- c("aenm","spid","dsstox_substance_id","casn","hitc","aeid")

# can only combine for compounds that were ran previously, the toxcast compounds
cmc5 <- merge(mc5_mc6[, ..usecols], mc5_org[,..usecols], by = setdiff(usecols,c("hitc","aeid")), suffixes = c(".new",".org"))
cmc5[hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 1368
cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 160
cmc5[hitc.org == -1, .N, by = "hitc.new"]
unique(cmc5$hitc.org)
# [1] 0 1 oh, okay... right, doesn't include DMSO
cmc5[hitc.new == -1, .N, by = "hitc.org"]
# hitc.org N
# 1:        0 2 # so 2 compounds that were previously no hits are now not-runable

cmc5[hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N/nrow(cmc5)] # 0.89
# most differences
cmc5[hitc.org == 1 & hitc.new == 0, .N] # 60
cmc5[hitc.org == 0 & hitc.new == 1, .N] # 100
nrow(cmc5) # 1530

# let's check out an added hitc
cmc5[hitc.org == 0 & hitc.new == 1]

# just for CellTiter blue
cmc5[grepl("AB",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 359
cmc5[grepl("AB",aenm) & hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 19
cmc5[grepl("AB",aenm) & hitc.org == -1, .N, by = "hitc.new"]
unique(cmc5$hitc.org)
# [1] 0 1 oh, okay... right, doesn't include DMSO
cmc5[grepl("AB",aenm) & hitc.new == -1, .N, by = "hitc.org"]
# hitc.org N
# 1:        0 2 # so 2 compounds that were previously no hits are now not-runable

cmc5[grepl("AB",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N/nrow(cmc5[grepl("AB",aenm)])] # 0.9497
# most differences are...
cmc5[grepl("AB",aenm) & hitc.org == 1 & hitc.new == 0, .N] # 18
cmc5[grepl("AB",aenm) & hitc.org == 0 & hitc.new == 1, .N] # 1
nrow(cmc5) # 1530

# let's check out an added hitc
cmc5[grepl("AB",aenm) & hitc.org == 0 & hitc.new == 1]

# removed hits:
cmc5[grepl("AB",aenm) & hitc.org == 1 & hitc.new == 0]


# just for LDH
cmc5[grepl("LDH",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 356
cmc5[grepl("LDH",aenm) & hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 28
cmc5[grepl("LDH",aenm) & hitc.org == -1, .N, by = "hitc.new"]
unique(cmc5$hitc.org)
# [1] 0 1 oh, okay... right, doesn't include DMSO
cmc5[grepl("LDH",aenm) & hitc.new == -1, .N, by = "hitc.org"]

cmc5[grepl("LDH",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N/nrow(cmc5[grepl("LDH",aenm)])] # 0.927
# most differences are...
cmc5[grepl("LDH",aenm) & hitc.org == 1 & hitc.new == 0, .N] # 23 removed
cmc5[grepl("LDH",aenm) & hitc.org == 0 & hitc.new == 1, .N] # 5 added
nrow(cmc5) # 1530

# let's check out an added hitc
cmc5[grepl("LDH",aenm) & hitc.org == 0 & hitc.new == 1]

# removed hits:
cmc5[grepl("LDH",aenm) & hitc.org == 1 & hitc.new == 0]

# total hit break down
mc5_mc6[grepl("LDH",aenm), .N, by = "hitc"]
# hitc   N
# 1:   -1   7
# 2:    0 493
# 3:    1  15

mc5_mc6.2 <- mc5_mc6

# see the previous run again
getwd()
load("sbox_dat/sbox_dat_2020-06-10.RData")
mc5_mc6[grepl("LDH",aenm), .N, by = "hitc"]
# hitc   N
# 1:   -1   7
# 2:    0 483
# 3:    1  25
# okay, so we definitely lost hit calls by doing the zero-centering...
cmc5.1 <- merge(mc5_mc6[, ..usecols], mc5_org[,..usecols], by = setdiff(usecols,c("hitc","aeid")), suffixes = c(".new",".org"))

cmc5.1[grepl("LDH",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N/nrow(cmc5[grepl("LDH",aenm)])] # 95.8%
cmc5.1[grepl("LDH",aenm) & hitc.org == 1 & hitc.new == 0, .N] # 11 removed
cmc5.1[grepl("LDH",aenm) & hitc.org == 0 & hitc.new == 1, .N] # 5 added

# hmm... what are most LDH bval's?
mc3.1 <- mc3
mc5_mc6.1 <- mc5_mc6

load("sbox_dat/sbox_dat_2020-06-11.RData")
mc3.2 <- mc3
mc3.2[aeid == 2439, summary(unique(bval))]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.688   2.841   4.816   4.067   6.249  10.600 

# find where zero-centering removes hits
allhits.nochange <- mc5_mc6.1[grepl("LDH",aenm) & hitc == 1, unique(spid)]
mc5_mc6.2[spid %in% allhits.nochange & hitc == 0 & grepl("LDH",aenm), .(spid, hitc)]

# are there any added hit calls from non-zero centered to zero-centered?
allnohits.nochange <- mc5_mc6.1[grepl("LDH",aenm) & hitc == 0, unique(spid)]
mc5_mc6.2[spid %in% allnohits.nochange & hitc == 1 & grepl("LDH",aenm), .(spid, hitc)]
# 6 added hits

# what is bmad?
mc5_mc6.2[grepl("LDH",aenm), unique(coff)]
mc5_mc6.2[, unique(coff), by = "aenm"]
mc3.2[, range(bval), by = "aeid"]
