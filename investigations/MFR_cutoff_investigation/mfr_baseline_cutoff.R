# script to determine best mfr cutoff
# library(tcpl)
library(data.table)
library(tcpl)
library(RMySQL)
setwd(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/"))

# COLLECTING THE DATA - ONLY RUN ONCE--------------------------------------------------------
load("ToxCast2016/output/ToxCast2016_alldat4_2020-05-21.RData")
load("DNT2019/output/DNT2019_alldat4_2020-05-21.RData")
load("APCRA2019/output/APCRA2019_alldat4_2020-05-21.RData")

# get the data that I want:
# wllt, conc, rval, spid, acsn - mfr only, srcf just for fun
mdat <- ToxCast2016_alldat4[grepl("firing_rate",acsn), .(spid, apid, rowi, coli, conc, wllt, rval, srcf, wllq)]
mdat <- rbind(mdat, DNT2019_alldat4[grepl("firing_rate",acsn), .(spid, apid, rowi, coli, conc, wllt, rval, srcf, wllq)])
mdat <- rbind(mdat, APCRA2019_alldat4[grepl("firing_rate",acsn), .(spid, apid, rowi, coli, conc, wllt, rval, srcf, wllq)])
rm(list = c("APCRA2019_alldat4","DNT2019_alldat4","ToxCast2016_alldat4"))

# get the baseline and treated separate data
load("ToxCast2016/output/ToxCast2016_alldat1_2020-05-19.RData")
dat1 <- alldat1[grepl("firing_rate",tcpl_acsn), .(well, activity_value, apid, coli, rowi, run_type, wllq)]
rm(alldat1)
load("DNT2019/output/DNT2019_alldat1_2020-05-19.RData")
dat1 <- rbind(dat1, alldat1[grepl("firing_rate",tcpl_acsn), .(well, activity_value, apid, coli, rowi, run_type, wllq)])
rm(alldat1)
load("APCRA2019/output/APCRA2019_alldat1_2020-05-21.RData")
dat1 <- rbind(dat1, alldat1[grepl("firing_rate",tcpl_acsn), .(well, activity_value, apid, coli, rowi, run_type, wllq)])
rm(alldat1)

# save this data for reproducibility, in case older files get deleted
save(dat1, file = "MFR_cutoff_investigation/data/dat1_toxcast2016_dnt2019_aprca2019_2020-05-21.RData")
dat4 <- mdat
save(dat4, file = "MFR_cutoff_investigation/data/dat4_toxcast2016_dnt2019_aprca2019_2020-05-21.RData")
rm(list = c("mdat","dat4","dat1"))


# WRITE LVL0 DATA --------------------------------------------------------------------------------------

setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/MFR_cutoff_investigation")
load("data/dat4_toxcast2016_dnt2019_aprca2019_2020-05-21.RData")

# realized I need to address where conc=0
sort(unique(dat4$conc))
# oh no! there is a zero
dat4[conc == "0", unique(apid)] # oh duh! this is from the DNT data,where some conc's were all 0
# [1] "20190618_MW69-0106" "20190618_MW69-0107" "20190618_MW69-0108"
# I really want to include this culture, bc it was going to be excluded because of low activity
# and low activity is exactly what I want to screen for!
# I am just going to assign a conc of 1 for all of these
dat4[conc == "0" & grepl("20190618",apid), conc := "1.0"]


# ----------------------- Register the spids in sbox
# see how many are currently registered in sbox
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplLoadChem(field = "spid", val = unique(dat4$spid)) # 384 are registered
length(unique(dat4$spid)) # 525 total
new.spids <- setdiff(unique(dat4$spid), tcplLoadChem(field = "spid", val = unique(dat4$spid))$spid)

# register the casn / chid's
## get casn's from invitrodb
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db="invitrodb", drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
invitrodb.chem <- tcplLoadChem(field = "spid", val = new.spids)

## see how many chem/casn are already registered in sbox
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
sbox.chem <- tcplLoadChem(field = "casn", val = unique(invitrodb.chem$casn))
casn.to.reg <- setdiff(invitrodb.chem$casn, sbox.chem$casn) # these have not been registered:
# [1] "157212-55-0"  "5124-25-4"    "17865-07-5"   "1820573-27-0" "6106-04-3"  
invitrodb.chem$casn <- as.character(invitrodb.chem$casn)
chdat <- invitrodb.chem[casn %in% casn.to.reg]
tcplRegister(what = "chid", flds = chdat[, .(casn, chnm)])
# Error: Duplicate entry '0' for key 'PRIMARY' [1062]
# not sure what is happenning here... I will just remove these 5 chem for now
unregistered.casn.chems.spids <- invitrodb.chem[casn%in% casn.to.reg,spid]
dat4 <- dat4[!(spid %in% unregistered.casn.chems.spids)]
invitrodb.chem <- invitrodb.chem[!(casn %in% casn.to.reg)]

## register spid's in sbox
spid.to.reg <- setdiff(invitrodb.chem$spid, sbox.chem$spid) # 130 to register
## confirming that sbox chid are the same as invitrodb chid
# merge.test <- merge(invitrodb.chem[spid %in% spid.to.reg], sbox.chem[, .(chid, casn, chnm, dsstox_substance_id)], by = c("casn","chnm","dsstox_substance_id"))
# merge.test[, length(which(chid.x != chid.y))] # 0! okay, so I guess these are the same

tcplRegister(what = "spid", flds = invitrodb.chem[spid %in% spid.to.reg, .(spid, chid)])
tcplLoadChem(field = "spid", val = unique(mc0$spid)) # all 514 are present! (525 - 5 dropped bc of errors - controls)

# ----------------------- Assign acid's
# just doing mean firing rate, for now
mfr_acid <- tcplLoadAcid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")$acid
dat4[, acid := mfr_acid]
str(dat4)

# ----------------------- Write lvl 0 data!
# close any open connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

mc0 <- dat4[, list(acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)]

# got an error bc probably doesn't like the wllt of "media"
mc0[nchar(wllt)>1,unique(wllt)] # all media
mc0[wllt == "media", wllt:="a"]

# trying again:
tcplConfList() # I am on sbox
tcplWriteLvl0(mc0, type = "mc") # May 22, 2020 2:07pm
# Completed delete cascade for 2 ids (53.31 secs)
# [1] TRUE
# Warning messages:
#   1: In for (i in seq_len(n)) { :
#       call dbDisconnect() when finished working with a connection
#     2: In for (i in seq_len(n)) { :
#         call dbDisconnect() when finished working with a connection

# ----------------------- Confirm assigned methods
# lvl2
tcplMthdLoad(lvl=2L, id = mfr_acid, type = "mc")
# acid mthd mthd_id ordr
# 1: 2432 none       1    1

# lvl3
mfr_aeid_info <- tcplLoadAeid(fld = "acid", val = mfr_acid)
mfr.aeid.up <- mfr_aeid_info[grepl("_up",aenm),aeid]
mfr.aeid.dn <- mfr_aeid_info[grepl("_dn",aenm),aeid]
tcplMthdLoad(lvl=3L, id=mfr.aeid.up,"mc") #right
# aeid                       mthd mthd_id ordr
# 1: 2425 bval.apid.nwllslowconc.med      17    1
# 2: 2425                  pval.zero      32    2
# 3: 2425                 resp.logfc      35    3
tcplMthdLoad(lvl=3L, id=mfr.aeid.dn,"mc") # yep

# lvl 4-6
tcplMthdLoad(lvl=4L, id = mfr_aeid_info$aeid)
# aeid                     mthd mthd_id
# 1: 2425 bmad.aeid.lowconc.nwells       2
# 2: 2442 bmad.aeid.lowconc.nwells       2
tcplMthdLoad(lvl=5L, id = mfr_aeid_info$aeid)
# aeid  mthd mthd_id
# 1: 2425 bmad3       1
# 2: 2442 bmad3       1
tcplMthdLoad(lvl=6L, id = mfr_aeid_info$aeid)

# ----------------------------------- tcpl run with hack function
# close any open connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())
rm(list = ls())

# assay.list <- tcplLoadAeid(fld = "asid", val=20, add.fld = "acid")
mfr_acid <- tcplLoadAcid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R', echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList() # sbox confirmed

# May 22, 2020 5:19pm 
tcplRun(slvl = 1L, elvl = 6L, id = mfr_acid$acid, type = "mc")

# Loaded L0 ACID2432 (13938 rows; 7.13 secs) ...
# Total processing time: 11.06 mins 
# 
# $l1
# ACID2432 
# TRUE 
# 
# $l1_failed
# character(0)
# 
# $l2
# ACID2432 
# TRUE 
# 
# $l2_failed
# character(0)
# 
# $l3
# ACID2432 
# TRUE 
# 
# $l3_failed
# character(0)
# 
# $l4
# AEID2425 AEID2442 
# TRUE     TRUE 
# 
# $l4_failed
# character(0)
# 
# $l5
# AEID2425 AEID2442 
# TRUE     TRUE 
# 
# $l5_failed
# character(0)
# 
# $l6
# AEID2425 AEID2442 
# TRUE     TRUE 
# 
# $l6_failed
# character(0)


# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")




# DO MY ANALYSIS------------------------------------------------------
rm(list=ls())
tcplListFlds(tbl ="mc4")
tcplLoadAcid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")
# acnm acid
# 1: NHEERL_MEA_acute_firing_rate_mean 2432
con <- dbConnect(drv = RMySQL::MySQL(), dbname = "sbox_invitrodb_v3_2", user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'))
mc3 <- dbGetQuery(con, "SELECT 
                  resp, bval, aeid, acid, logc, m0id FROM mc3 WHERE acid = 2432")

# get the spid's via m0id's
mc0 <- dbGetQuery(con, "SELECT m0id, spid, rowi, coli,apid FROM mc0 WHERE acid = 2432")

mc3 <- merge(mc3, mc0, by = "m0id", all.x=T)
unique(mc3$aeid)
# [1] 2442 2425

# get bmad
bmad <- dbGetQuery(con, "SELECT DISTINCT bmad FROM mc4 WHERE aeid IN (2442,2425)")

dat <- as.data.table(mc3)
dat[, bmad := bmad]


# add in my alldat1
load("data/dat1_toxcast2016_dnt2019_aprca2019_2020-05-21.RData")
baseline_dat1 <- dat1[run_type=="baseline"]
treated_dat1 <- dat1[run_type=="treated"]
wdat1 <- merge(baseline_dat1, treated_dat1, by = c("well","apid","coli","rowi"), suffixes = c(".baseline",".treated"))

# use the wllq from baseline
wdat1[, wllq.treated := NULL]
setnames(wdat1, old = "wllq.baseline", new = "wllq")

# calculated the raw difference
wdat1[, activity_value.diff := activity_value.treated - activity_value.baseline]

# let's just use the "up" endpoints, for now (2425). Dn is 2442
nrow(dat[aeid==2425]) # 13466
wdat1[wllq==1,.N] #  13604
# this diff in nrows is probs dues to the 5 compounds that I dropped
wdat1$coli <- as.numeric(wdat1$coli)
wdat1 <- merge(dat[aeid==2425], wdat1[wllq==1], by = c("apid","coli","rowi"))

# all of these points should NOT have a significant response (i.e., should be within bmad)
plot(wdat1[activity_value.diff < 5/60, .(activity_value.baseline, resp)])
abline(h = bmad)
abline(h= -1*bmad)
title(main = paste0("Resp versus baseline mean firing rate (Hz)\nwhere MFRt - MFRb < 5*60 (Hz)"))

# is there noise because of the bval's?
# is it possible to choose a cutoff that would fix this?

# hmm, stuff to think about. I am going to save what I have
save(mc3, file = "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/test_output/mfr_cutoff_investigation/mc3_meanfiringrate_sbox_2020-02-22.RData")
save(wdat1, file = "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/test_output/mfr_cutoff_investigation/wdat1_meanfiringrate_sbox_2020-02-22.RData")
save(mc3, file = "data/mc3_meanfiringrate_sbox_2020-02-22.RData")
save(wdat1, file = "data/wdat1_meanfiringrate_sbox_2020-02-22.RData")

wdat1[,test_val := activity_value.treated*100/(100 + 3*bmad - bval)]
wdat1[activity_value.diff < 5/60 & activity_value.baseline < test_val]

# for dn resp
wdat1[,test_val2 := activity_value.treated*100/(100 - 3*bmad + bval)]
wdat1[activity_value.diff < 5/60 & activity_value.baseline > test_val2]
summary(wdat1$test_val2) # oof, got some neg
max_baseline_with_high_resp <- wdat1[activity_value.diff < 5/60 & activity_value.baseline > test_val2, max(activity_value.baseline)]
min_baseline_with_safe_resp <- wdat1[activity_value.diff < 5/60 & activity_value.baseline < test_val2, min(activity_value.baseline)]
max_baseline_with_high_resp
# [1] 5.946412
min_baseline_with_safe_resp
# [1] 0.1240756
# aw man! since there is overlap in the baseline values for these groups, I can't draw a clear cutoff this way...
# to be continued...
# will def need to look into the bval noise a bit. Might try again by apid

# Previous musings::

# wow. I don't understand how the values can get this big when baseline is greater than 1!
# mc4 mthd_id=2 from bitbucket:
# bmad.aeid.lowconc.nwells = function() {
#   
#   e1 <- bquote(dat[ , bmad := mad(resp[wllt == "n"], na.rm = TRUE)])
#   list(e1)
#   
# }

dbDisconnect(con)

# then I can:
# get alldat1, and extract the baseline data for mean firing rate
# get table with columns baseline_mfr, treated_mfr, diff, resp, bmad
# table[diff <= 5*60 & resp < bmad, min(baseline_mfr)] OR (I think I want this top one)
# table[diff <= 5*60 & resp > bmad, max(baseline_mfr)] 
# where diff <= 5*60, does resp increase mostly monotonically as baseline_mfr decrease?

# plot to answer that:
plot(table[diff <= 5*60, .(baseline_mfr, resp)])
abline(h = table[, unique(bmad)])
# find baseline_mfr_cutoff such that 
abline(v = baseline_mfr_cutoff)
# all points to the right of this line are below bmad

