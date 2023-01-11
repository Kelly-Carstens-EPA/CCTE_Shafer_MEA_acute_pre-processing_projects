# trying out a different normalization method
# with (cval - bval)/(-100 - bval)*100 for dn,
# (cval - bval)/(100 - bval)*100 for up

# script to run MEA acute lvl 0 data
library(tcpl)
library(data.table)
library(RMySQL)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# REVIEW REGISTRATION>UPDATE --------------------------------------------------------------------------------

tcplLoadAcid(fld = 'asid', val = 20)

# get the names of just the 15 components that we care about
org_15_ascn_map <- as.data.table(read.csv('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/deprecated_acsn_map.csv',stringsAsFactors=F))


# REGISTER NEW ACID/AEID --------------------------------------------------------------------------------


# PREPARE MC0 ------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
load("lvl0_snapshots/mea_acute_lvl0_2020-07-29.RData")

# Assign acid's
acsns <- sub('NHEERL_','CCTE_Shafer_',org_15_ascn_map$tcpl_acsn)
acid.acnm <- tcplLoadAcid(fld = "acnm", val = acsns)
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm, by = "acnm")
mea_acute_lvl0[, acnm := NULL]

# I have to remove a few compounds that are not registered
mc0 <- mea_acute_lvl0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01",
                                    "EX000371", "EX000372", "EX000373", "EX000374", "EX000408", "EX000411"))]

# so, should I pre-normalize, or use tcpl methods?
tcplMthdList(lvl=2L,type = 'mc')
tcplMthdList(lvl=3L, type = 'mc')

# options:
# for dn, resp.multineg1
# Use pval.maxp - pval is the maximum p-well value for the entire aeid.
# this should be 100... I think. 
# then, resp.pc = (cval - bval) / (pval - bval)
# for up, 
# resp.multineg1
# pval.maxp (should be 100)
# resp.multineg1 - make everything postive again
# resp.pc = (cval - bval) / (pval - bval)

# option b:
# calculate bval myself
# 

# option c
# remove all 'p' wells, add my own dummy p wells at -100 for all.

# I really have to pre-normalize, I see no other way
rm(mea_acute_lvl0)

# TAKING From mc1 code


## Set conc to three significant figures
mc0[ , conc := signif(conc, 3)]

## Define replicate id
# Order by the following columns
setkeyv(mc0, c('acid', 'srcf', 'apid', 'coli', 'rowi', 'spid', 'conc')) 
# Define rpid column for test compound wells
nconc <- mc0[wllt == "t" , 
             list(n = lu(conc)), 
             by = list(acid, apid, spid)][ , list(nconc = min(n)), by = acid]
mc0[wllt == "t" & acid %in% nconc[nconc > 1, acid],
    rpid := paste(acid, spid, wllt, srcf, apid, "rep1", conc, sep = "_")]
mc0[wllt == "t" & acid %in% nconc[nconc == 1, acid],
    rpid := paste(acid, spid, wllt, srcf, "rep1", conc, sep = "_")]
# Define rpid column for non-test compound wells
mc0[wllt != "t", 
    rpid := paste(acid, spid, wllt, srcf, apid, "rep1", conc, sep = "_")] 
# Increment rpid 
mc0_rpid <- mc0[ , rpid]
j = 2L
while (any(duplicated(mc0_rpid))) {
  ind <- duplicated(mc0_rpid)
  mc0_rpid[ind] <- sub("_rep[0-9]+", paste0("_rep", j), mc0_rpid[ind])
  j <- j + 1
}
mc0[ , rpid := mc0_rpid]
rm(mc0_rpid)
# Remove conc values from rpid
mc0[ , rpid := sub("_([^_]+)$", "", rpid, useBytes = TRUE)]

## Define concentration index
indexfunc <- function(x) as.integer(rank(unique(x))[match(x, unique(x))])
mc0[ , cndx := indexfunc(conc), by = list(rpid)]

## Define replicate index
# Create temporary table containing the unique replicate ids

trdt <- unique(mc0[wllt %in% c("t", "c") , list(acid, spid, wllt, rpid)])
trdt_rpid <- trdt[ , rpid]
trdt[ , rpid := NULL]
trdt[ , repi := 1]
# Increment repi
while (any(duplicated(trdt))) {
  trdt[duplicated(trdt), repi := repi + 1]
}
trdt[ , rpid := trdt_rpid]
rm(trdt_rpid)
# Map replicate index back to mc0
setkey(mc0, rpid)
setkey(trdt, rpid)
mc0[ , repi := trdt[mc0, repi]]

## Remove rpid column
mc0[, rpid := NULL]

mc0 <- mc0[wllq == 1]

# LEVEL 3 methods:
mc0[, bval := median(rval[(cndx %in% 1:2 & wllt == "t") | 
                            wllt == "n"],
                     na.rm = TRUE),
    by = list(acid, apid)]
mc0[, rval2 := (rval - bval) / (-100 - bval) * 100]
mc0[, summary(rval2)]
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -17427.771    -11.499      0.618     -6.135     17.380    100.000
mc0[, rval := NULL]
mc0[, c('bval','cndx','repi') := NULL]
setnames(mc0, old = 'rval2', new = 'rval')

nrow(mc0) # 162428

# close any connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList() # confirming db
tcplWriteLvl0(mc0, type = "mc") # Dec 3, 2020 1:41am


# REGISTER/CONFIRM METHODS -----------------------------------------
acids <- unique(mea_acute_lvl0$acid)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # none for all


# lvl 3
dn.mea <- aeid.info[grepl("_dn",aenm), aeid]
tcplMthdLoad(lvl=3, id = dn.mea)
tcplMthdClear(lvl=3, id = dn.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = dn.mea, mthd_id = c(1), ordr = c(1), type = "mc") # none for all
tcplMthdLoad(lvl=3, id = dn.mea)

up.mea <- aeid.info[grepl("_up",aenm) & !(grepl("(LDH)|(AB)",aenm)), aeid]
tcplMthdLoad(lvl=3, id = up.mea)
tcplMthdClear(lvl=3, id = up.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = up.mea, mthd_id = c(1, 6), ordr = c(1:2), type = "mc") # initialize resp, then multiply by -1
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
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

x <- '2431
2432
2433
2434
2435
2436
2437
2438
2439
2440
2442
2443
2444
2445'
cat(stri_replace_all_regex(x,'\n',"','"))
acids <- c('2431','2432','2433','2434','2435','2436','2437','2438','2439','2440','2442','2443','2444','2445')
assay.list <- tcplLoadAcid(fld = "acid", val= acids)

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# Dec 3, 2020 2:08am
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# yay!

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# Save data and plots ----------------------------

assay.list <- tcplLoadAeid(fld = "acid", val=assay.list$acid)
# save the data, see some plots
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/")
for (i in unique(assay.list$aeid)){
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
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)

# get the methods
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = assay.list$aeid, "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = assay.list$aeid, "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = assay.list$aeid, "mc")

# save the data
sbox_run_summary <- paste0("Pre-normalizing with rval := (rval - bval)/(-100 - bval)*100. Mostly testing for dn endpoints, ran up with resp*-1. Using mea_acute_lvl0_2020-07-29.Rdata. Date ran: 2020-12-03.")
save(sbox_run_summary, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, 
     file = file.path(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData")))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))


# ---------------------------
load(file.path(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData")))
mc5_mc6[, .(sum(hitc==1)), by = .(aenm)][order(V1)]

# compare with previous:
ndat <- as.data.table(read.csv('L:/Lab/Toxcast_Data/toxcast_data/files/ccte_shafer/ccte_shafer_mea_acute/mc5_mc6_CCTE_SHAFER_MEA_ACUTE_05AUG2020.csv',stringsAsFactors = F))

comp <- merge(mc5_mc6[, .(spid, chnm, aenm, hitc, modl_ga, flag_ids)], ndat[, .(spid, chnm, aenm, hitc, modl_ga, flag_ids = mc6_flags)], by = c('spid','chnm','aenm'), suffixes = c('.n','.p'))
comp[, .N, by = .(hitc.n, hitc.p)]
# hitc.n hitc.p     N
# 1:     -1     -1   240
# 2:      1      1  2552
# 3:      0      0 10711
# 4:      0      1   108
# 5:      1      0   333
# 108 removed hits, 333 added hits for all aeid

# characterizing the added hits
comp[hitc.n==1 & hitc.p == 0, .N, by = .(aenm)][order(-N)]
comp[grepl('firing_rate_mean_dn',aenm) & hitc.n==1 & hitc.p == 0, .N, by = .(flag_ids.n)][order(-N)]

# how many added hits where no flag n, or borderline miss p?
comp[hitc.n==1 & hitc.p == 0 & (grepl('12',flag_ids.p) | grepl('11',flag_ids.n)), .N, by = .(flag_ids.n)][order(-N)]
comp[hitc.n==1 & hitc.p == 0, .N, by = .(prev_border_miss = grepl('12',flag_ids.p), new_border_hit = grepl('11',flag_ids.n))]

comp[grepl('firing_rate_mean_dn',aenm), .N, by = .(hitc.n, hitc.p)]

plotdat <- plotdat[hitc.n != -1]

comp[, range(modl_ga.p,na.rm=T)]
comp[, range(modl_ga.n,na.rm=T)] # both max at 2.1
comp[hitc.p == 0, modl_ga.p := 3]
comp[hitc.n == 0, modl_ga.n := 3]

plot(modl_ga.n ~ modl_ga.p, comp)
abline(0,1)
title(main = 'New log10(AC50) vs previous. Set to 3 where hitc==0')
#  I'm really more concerned about the change in hit call than anything else.


# comapre results with what was previously pipelined
mc5_org <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/ccte_shafer/nheerl_mea_acute/output/01APR2020/mc5_mc6_nheerl_mea_acute.csv", stringsAsFactors = FALSE)
setDT(mc5_org)

mc5_org[, .(sum(hitc==1)), by = .(aenm)][order(V1)]
# wow, sevearl endpoints have 50-80 more hits... woah.

# checking out nalist compounds...
load('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/my_project/Positives/positives_list/data/nalist_with_references_2020-11-20.RData')
head(nalist)
na_dtxsid <- intersect(nalist$DTXSID, mc5_mc6$dsstox_substance_id)
mc5_mc6[dsstox_substance_id %in% na_dtxsid, .(.N, min(modl_ga, na.rm=T)), by = .(hitc, chnm)]
na_spid <- mc5_mc6[dsstox_substance_id %in% na_dtxsid, unique(spid)]
mc5_mc6[grepl('firing_rate_mean_dn',aenm) & spid %in% na_spid, .(hitc, modl_ga, flags, chnm)][order(hitc)]
View(mc5_mc6[grepl('firing_rate_mean_dn',aenm) & spid %in% na_spid, .(hitc, modl_ga, flags, chnm)][order(hitc)])
     
     # questions:
    # - does the new method pick up all of the 'missing_expected_hits' from before?
    # - let's again tabulate how many compounds I would have expected to be a strong hit, but were not
dat4[, med_rval := median(rval[wllq == 1]), by = .(spid, acnm, conc)]
dat4[, min_med_rval := min(med_rval, na.rm = T), by = .(spid, acnm)] # 6ish no med_rval
expected_mfr_dn_hits <- dat4[wllt == 't' & grepl('firing_rate_mean$',acnm) & min_med_rval < -95 & !is.na(bval), unique(spid)]
(missed_expected_mfr_dn_hits <- ndat[grepl('firing_rate_mean_dn',aenm) & hitc == 1, setdiff(expected_mfr_dn_hits, spid)])
missed_expected_mfr_dn_hits <- ndat[grepl('firing_rate_mean_dn',aenm) & spid %in% missed_expected_mfr_dn_hits & is.na(mc6_flags), unique(spid)]
missed_expected_mfr_dn_hits # 26
mc5_mc6[spid %in% missed_expected_mfr_dn_hits & grepl('firing_rate_mean_dn',aenm), .N, by = .(hitc, chnm, spid, flag_ids)][order(hitc, flag_ids)]
setdiff(missed_expected_mfr_dn_hits, mc5_mc6$spid) # "EPAPLT0154A05" - this was 1 that I couldn't register in the sbox
# of the 26: 1 not in sbox, 1 hitc=-1, 7 hitc==0, 17 hitc == 1 now!

(new_missed_mfr_dn_hits <- mc5_mc6[spid %in% expected_mfr_dn_hits & grepl('firing_rate_mean_dn',aenm) & hitc==0, unique(spid)])
# 9 missed expected hits
setdiff(new_missed_mfr_dn_hits, missed_expected_mfr_dn_hits) # 7 same as earlier, 2 were hit previous but now are not

spidi <- 'EPAPLT0154D10'
plot_raw_resp <- function(spidi) {
  apidi <- dat4[spid == spidi, unique(apid)]
  usedat <- dat4[grepl('firing_rate_mean',acnm) & apid %in% apidi]
  usedat[, xval := ifelse(wllt =='t',logc,wllt)]
  usedat$xval <- factor(usedat$xval, levels = c(sort(unique(grep("^[a-z]",usedat$xval,val=T))), sort(as.numeric(unique(grep("[0-9]",usedat$xval,val=T))))), ordered = T)
  stripchart(rval ~ xval, usedat[spid %in% spidi | wllt == 'n'], pch = 1,cex=1.5,vertical=T,method='jitter',ylab = '(treated - baseline)/basline * 100')
  title(main = c(paste0('% Change in MFR ',mc5_mc6[spid == spidi, unique(chnm)],' ',spidi)))
}

plot_raw_resp <- function(spidi) {
  apidi <- dat4[spid == spidi, unique(apid)]
  usedat <- dat4[grepl('firing_rate_mean',acnm) & apid %in% apidi]
  usedat[, xval := ifelse(wllt =='t',cndx,wllt)]
  usedat$xval <- factor(usedat$xval, levels = c(sort(unique(grep("^[a-z]",usedat$xval,val=T))), sort(as.numeric(unique(grep("[0-9]",usedat$xval,val=T))))), ordered = T)
  stripchart(rval ~ xval, usedat[spid %in% spidi | (wllt == 'n' | (wllt == 't' & cndx %in% c(1,2)))], pch = "",cex=1.5,vertical=T,method='jitter',ylab = '(treated - baseline)/basline * 100')
  stripchart(rval ~ xval, usedat[(wllt == 'n' | (wllt == 't' & spid != spidi & cndx %in% c(1,2)))], pch = 1,col='gray50',cex=1.5,vertical=T,add=T)
  stripchart(rval ~ xval, usedat[spid %in% spidi], col = 'blue', pch = 1,cex=1.5,vertical=T,add=T)
  title(main = c(paste0('% Change in MFR ',mc5_mc6[spid == spidi, unique(chnm)],' ',spidi)))
  legend(x = 'topright',bg = 'transparent', legend =  c(paste0('MFR dn hitc=',ndat[spid==spidi & grepl('firing_rate_mean_dn',aenm),unique(hitc)]),
                                                        paste0('MFR dn hitc new = ',mc5_mc6[spid==spidi & grepl('firing_rate_mean_dn',aenm),unique(hitc)]),
                                                        'all apid wllt==n or cndx 1 or 2',
                                                        'treated'),
         pch = c('','','o','o'), col = c('black','black','gray50','blue'),cex=0.6)
}

graphics.off()
pdf(file = 'missed_expected_mfr_hits_2020-12-03.pdf')
for (spidi in missed_expected_mfr_dn_hits) {
  plot_raw_resp(spidi)
}
graphics.off()
# this doesn't work for abamectin -- cndx goes back to 1 for second apid

# If bval < coff - 100, then nothing can be a hit on those apid
check.apid <- dat4[grepl('firing_rate_mean',acnm) & bval < (49-100), unique(apid)]
check.spid <- dat4[apid %in% check.apid, unique(spid)]
check.spid <- dat4[spid %in% check.spid, .(length(unique(apid))), by = .(spid)][V1 == 1, c(spid)]
ndat[spid %in% check.spid & grepl('firing_rate_mean_dn',aenm), .N, by = .(hitc)]
# hitc  N
# 1:    0 63
# 2:   -1  4


# names(mc5_org)
# length(unique(mc5_org$spid))*32
# nrow(mc5_org)
# usecols <- c("aenm","spid","dsstox_substance_id","casn","hitc","aeid")
# 
# # can only combine for compounds that were ran previously, the toxcast compounds
# cmc5 <- merge(mc5_mc6[, ..usecols], mc5_org[,..usecols], by = setdiff(usecols,c("hitc","aeid")), suffixes = c(".new",".org"))
# cmc5[hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 2775
# cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 285
# 
# # just comparing LDH
# cmc5[grepl("LDH",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 355 same hitc
# cmc5[grepl("LDH",aenm) & hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 29 diff hitc
# 
# # added LDH hits
# cmc5[grepl("LDH",aenm) & hitc.org == 0 & hitc.new == 1, .N] # 6
# cmc5[grepl("LDH",aenm) & hitc.org == 1 & hitc.new == 0, .N] # 23 removed hits
# 
# # percent agreement
# total_hitcs <- cmc5[grepl("LDH",aenm) & hitc.org != -1 & hitc.new != -1, .N]
# cmc5[grepl("LDH",aenm) & hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N/total_hitcs] # 92.44% percent agreement
# 
# 
# # compare to cval - bval "zero-centering" method
# mc5_mc6.21 <- mc5_mc6
# mc3.21 <- mc3
# load("sbox_dat/sbox_dat_2020-06-11.RData")
# mc3_mthds # yep, cval - bval methods
# # 2: 2439 bval.apid.nwllslowconc.med      17    1
# # 3: 2439                  pval.zero      32    2
# # 4: 2439                 resp.logfc      35    3
# 
# cmc5.2 <- merge(mc5_mc6.21[, ..usecols], mc5_mc6[,..usecols], by = setdiff(usecols,c("hitc")), suffixes = c(".21",".11"))
# cmc5.2[grepl("LDH",aenm) & hitc.11 == hitc.21, .N] # 514 agreeing hitc's
# cmc5.2[grepl("LDH",aenm) & hitc.11 != hitc.21, .N] # 1 different hitc
# # cmc5.2[grepl("LDH",aenm) & hitc.11 != hitc.21]
# # aenm         spid dsstox_substance_id       casn aeid hitc.21 hitc.11
# # 1: NHEERL_MEA_acute_LDH_up TP0001413B06       DTXSID9023881 74115-24-5 2439       1       0
