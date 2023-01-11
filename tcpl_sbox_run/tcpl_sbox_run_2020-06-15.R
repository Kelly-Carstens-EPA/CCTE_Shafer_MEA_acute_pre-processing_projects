# 06/15/2020
# trying out new LDH methods: resp := (cval - bval) / (pval - bval)
library(tcpl)
library(data.table)
library(RMySQL)
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R')
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList()
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")

# save extra id info for lvl 3 data, before re-write over it
load(file = "sbox_dat/sbox_dat_2020-06-11.RData")
mc3[, unique(acid)] # 2432 2446 2447
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = c(2432, 2446, 2447), type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, srcf)], all.x = T)
save(mc3_mthds, mc4_mthds, mc3, mc5_mc6, file = "sbox_dat/sbox_dat_2020-06-11.RData")
rm(list = ls())

# PREPARE MC0 ------------------------------------------------------
# want to make sure have correct "p" wells for each apid
dat3 <- get_latest_dat(lvl = "dat3")
# [1] "Getting data from folders APCRA2019, DNT2019, ToxCast2016"
# [1] "APCRA2019_dat3_2020-06-15.RData"
# Loading objects:
#   dat3
# [1] "DNT2019_dat3_2020-06-10.RData"
# Loading objects:
#   dat3
# [1] "ToxCast2016_dat3_2020-06-10.RData"
# Loading objects:
#   dat3
ldh.dat3 <- dat3[grepl("LDH",acsn)]
ldh.dat4 <- dat4[grepl("LDH",acsn)]
ldh.dat <- merge(ldh.dat3[, .(experiment.date, plate.id, apid, rowi, coli, acsn, srcf, bcval = rval)], ldh.dat4, suffixes = c(".dat3",".dat4"))
apid_pwells <- ldh.dat[wllt == "p", .(pwells = paste0(unique(treatment),collapse=",")), by = "apid"]

# for apid with 1/2 Lysis wells, multiply these by 2, then remove LYSIS wells
ldh.dat[apid %in% apid_pwells[pwells == "Lysis,½ Lysis", apid] & treatment == "½ Lysis", `:=`(treatment = "2 * ½ Lysis", bcval = 2*bcval)]
ldh.dat <- ldh.dat[!(apid %in% apid_pwells[pwells == "Lysis,½ Lysis", apid] & treatment == "Lysis")]

# for apid with only LYSIS pwells, keep these wells as p
ldh.dat[, rval := bcval]
# new break down:
ldh.dat[wllt == "p", .(pwells = paste0(unique(treatment),collapse=",")), by = "apid"]

# Finalize this for mc0 data
mc0 <- ldh.dat[, .(acsn, spid, conc, wllt, wllq, rval, apid, rowi, coli, srcf)]
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

# Need to remove these, since not present in sbox
mc0 <- mc0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01"))]
tcplWriteLvl0(mc0, type = "mc") # June 15, 2020 3:13pm
#Completed delete cascade for 1 ids (110.88 secs)
# [1] TRUE


# REGISTER/COnfRIM METHODS -----------------------------------------
tcplLoadAcid(fld = "acnm", val= "NHEERL_MEA_acute_LDH") # 2446
tcplMthdLoad(lvl = 2L, id = 2446) # none, check
tcplLoadAeid(fld = "acnm", val= "NHEERL_MEA_acute_LDH")
# acnm aeid                    aenm
# 1: NHEERL_MEA_acute_LDH 2439 NHEERL_MEA_acute_LDH_up

for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = c(2439)))
}

tcplMthdClear(lvl = 3L, id = 2439, type = "mc")
tcplMthdList(lvl = 3L, type = "mc")
# bval of n and low cndx t wells - 17
# pval as apid median of p wells - 13              pval.apid.pwlls.med
# resp.pc = (cval - bval) / (pval - bval) - 5
tcplMthdAssign(lvl = 3L, id = 2439, mthd_id = c(17,13,5), ordr = c(1:3), type = "mc")
tcplMthdLoad(lvl = 3L, id = 2439, type = "mc")
# aeid                       mthd mthd_id ordr
# 1: 2439 bval.apid.nwllslowconc.med      17    1
# 2: 2439        pval.apid.pwlls.med      13    2
# 3: 2439                    resp.pc       5    3

# lvl 4, all are
# bmad.aeid.lowconc.nwells       2

# lvl 5, all are
# bmad3       1

# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "aeid", val=c(2439), add.fld = "acid")

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# June 15, 2020 3:29pm 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")
# success!

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# ----------------------------

# save the data, see some plots
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/")
for (i in unique(assay.list$aeid)){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
dbGetQuery(con, "DESC mc3")
mc3 <- dbGetQuery(con, "SELEct m0id, aeid, acid, bval, pval, logc, resp FROM mc3 WHERE aeid IN (2439)")
setDT(mc3)
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = c(2446), type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, srcf)], all.x = T)
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=c(2439), type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=c(2439), type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = c(2439), "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = c(2439), "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = c(2439), "mc")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
save(mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))


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
cmc5[hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 356
cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 28

cmc5[hitc.org != -1 & hitc.new != -1, .N] # 384
# 92.7% agreement
# 7.26% different hit calls

# total hitc break down
mc5_mc6[, .N, by = "hitc"]
# hitc   N
# 1:   -1   7
# 2:    0 493
# 3:    1  15
# exactly the as when normalize by resp.fc cval - bval!
# looking at plots, they look very similar. But not identical

# removed hits
cmc5[hitc.org == 1 & hitc.new == 0] # 23 cases
# aenm         spid dsstox_substance_id        casn hitc.new aeid.new hitc.org aeid.org
# 1: NHEERL_MEA_acute_LDH_up TP0001411A03       DTXSID4047339 181640-09-5        0     2439        1     2540
# 2: NHEERL_MEA_acute_LDH_up TP0001411A06       DTXSID3020336    637-07-0        0     2439        1     2540
# 3: NHEERL_MEA_acute_LDH_up TP0001411A07       DTXSID2021575     75-99-0        0     2439        1     2540
# 4: NHEERL_MEA_acute_LDH_up TP0001411A08       DTXSID7047358 439687-69-1        0     2439        1     2540
# 5: NHEERL_MEA_acute_LDH_up TP0001411A09       DTXSID8024151  35554-44-0        0     2439        1     2540
# 6: NHEERL_MEA_acute_LDH_up TP0001411A11       DTXSID6020147     95-14-7        0     2439        1     2540
# ...

# added hits
cmc5[hitc.org == 0 & hitc.new == 1] # 5 cases
# aenm         spid dsstox_substance_id       casn hitc.new aeid.new hitc.org aeid.org
# 1: NHEERL_MEA_acute_LDH_up TP0001411F02       DTXSID8026193    87-61-6        1     2439        0     2540
# 2: NHEERL_MEA_acute_LDH_up TP0001411G01       DTXSID6025486    51-30-9        1     2439        0     2540
# 3: NHEERL_MEA_acute_LDH_up TP0001412G02       DTXSID0021464   137-30-4        1     2439        0     2540
# 4: NHEERL_MEA_acute_LDH_up TP0001412G08       DTXSID0023581 79902-63-9        1     2439        0     2540
# 5: NHEERL_MEA_acute_LDH_up TP0001413G11       DTXSID0028038 55406-53-6        1     2439        0     2540
