# 06/23/2020
# what to compare results of finding bval by DMSO + cndx 1 onlye
# versus DMSO + cndx1&2
# since there is no lvl3 method for this, 
# I am going to add in the cndx 1 t compounds as exra "n" wells
# while keeping the original cndx 1 data in the mc0 table
# this time with more endpoints
library(tcpl)
library(data.table)
library(RMySQL)
# tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList()
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")


# PREPARE MC0 ------------------------------------------------------
load("lvl0_snapshots/mea_acute_lvl0_2020-06-19.RData")

# Assign acid's from sbox
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.acnm.idb <- tcplLoadAcid(fld = "acid", val = unique(mea_acute_lvl0$acid))
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm.idb, all.x = T)
mea_acute_lvl0[, acid := NULL]

tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
acid.info <- tcplLoadAcid(fld = "asid", val = 20)
(mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.info, by = "acnm"))
mea_acute_lvl0[, c("acnm","asid") := list(NULL, NULL)]

# get all things that were indexed at cndx1
load("sbox_dat/sbox_dat_2020-06-21.RData") # 6-19 data pipelined for all endpoints
unique(mc3$acid) # just 5 endpoints.

# get cdnx1 data as wllt "n"
cndx1.twells <- mc3[cndx == 1 & wllt == 't']
setnames(cndx1.twells, old = "cval", new = "rval") # I added cval from lvl 2, but it's the same as rval for our none methods
cndx1.twells[, wllt := "n"]
cndx1.twells[, spid := "test_cndx1"]
usecols <- setdiff(names(mea_acute_lvl0),c("m0id"))
cndx1.twells <- cndx1.twells[, ..usecols]
mc0 <- rbind(mea_acute_lvl0[, ..usecols], cndx1.twells)
rm(list = c("mc5_mc6","mc6_mthds","mc4_mthds","mc3"))

# to speed things up, let's remove CTB, since we already looked at that
tcplLoadAcid(fld = "acid", val = unique(mc0$acid)) # AB = 2447
mc0 <- mc0[acid != 2447]

# write it!
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConfList()

# Need to remove these, since not present in sbox
mc0 <- mc0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01"))]
mc0 <- mc0[, .(acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)] # getting rid of the extra "attributes" stuff
tcplConfList() # sbox confirmed
tcplWriteLvl0(mc0, type = "mc") # June 23, 2020 11:34pm
# Completed delete cascade for 31 ids (68.42 secs)
# [1] TRUE

# REGISTER/COnfRIM METHODS -----------------------------------------
# just realized that I wrote data for all acid's except 2447, even though I only added the cdnx 1 wells for the 5 acids:
mc0[acid %in% acids & wllt == "n", unique(spid), by = "acid"]
# acid         V1
# 1: 2446       DMSO
# 2: 2446 test_cndx1
# 3: 2433       DMSO
# 4: 2433 test_cndx1
# 5: 2432       DMSO
# 6: 2432 test_cndx1
# 7: 2445       DMSO
# 8: 2445 test_cndx1
mc0[!(acid %in% acids) & wllt == "n", unique(spid), by = "acid"] # only DMSO
acids <- c(2432, 2433, 2445, 2446) # plus 2447
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # all none for lvl 2

# lvl 3 - just use "n" wells, with the cheat
tcplMthdClear(lvl = 3L, id = aeid.info$aeid, type = "mc")
# LDH acid = 2446, aeid = 2439
tcplMthdAssign(lvl = 3L, id = 2439, mthd_id = c(11, 13, 5), ordr = c(1:3), type = "mc") # bval of n wells, pval, resp.pc

dn.mea <- c(2442, 2443, 2455)
tcplMthdAssign(lvl = 3, id = dn.mea, mthd_id = c(11, 35, 6), ordr = c(1:3), type = "mc") # set bval of n wells, resp.logfc, resp.multneg1

up.mea <- c(2425, 2426, 2438)
tcplMthdAssign(lvl = 3, id = up.mea, mthd_id = c(11,35), ordr = c(1:2), type = "mc")

# view 3 - 6
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = aeid.info$aeid, type = "mc"))
}

# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "acid", val=c(2432, 2433, 2445, 2446))

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# June 23, 2020 12:01pm 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")
# done!

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
description <- paste0("Comparining bval by 'n' and cndx 1 only, versus 'n' and cndx 1&2. For ",paste0(assay.list$aenm,collapse=", "),".")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
save(description, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))

# Checking out the Results ---------------------------
load("sbox_dat/sbox_dat_2020-06-23.RData")

# comapre results with results when used cndx 1 and 2
mc5_mc6.1 <- mc5_mc6
mc3.1 <- mc3
load("sbox_dat/sbox_dat_2020-06-21.RData") # created in script dated 6-19. Ran to test updated LDH wllt, NA conc's.
# so all data is as it should be, with the usual methods
mc3_mthds # from previously pipelined data
# mea endponts:
# 13: 2442 bval.apid.nwllslowconc.med      17    1
# 14: 2442                 resp.logfc      35    2
# 15: 2442              resp.multneg1       6    3 (only if dn)
# LDH:
# 7: 2439 bval.apid.nwllslowconc.med      17    1
# 8: 2439        pval.apid.pwlls.med      13    2
# 9: 2439                    resp.pc       5    3
# yes, bval is by n + cndx1&2

# compare hit calls!
nrow(mc5_mc6.1) # 3612
usecols <- c("aenm","spid","dsstox_substance_id","casn","chnm","hitc","aeid","modl_ga","flags","flag_ids")
cmc5 <- merge(mc5_mc6.1[, ..usecols], mc5_mc6[,..usecols], by = setdiff(usecols,c("hitc","modl_ga","flags","flag_ids")), suffixes = c(".new",".org"))
nrow(cmc5) # 3605 - my dummy spid "test_cndx1" got dropped for 7 aeid

cmc5[hitc.org == hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 3441
cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N] # 107

# oof, okay...
cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N, by = "aenm"]
# huh, fairly even spread
# aenm  N
# 1:              NHEERL_MEA_acute_LDH_up 21
# 2:     NHEERL_MEA_acute_burst_number_dn 24
# 3:     NHEERL_MEA_acute_burst_number_up  9
# 4: NHEERL_MEA_acute_firing_rate_mean_dn 20
# 5: NHEERL_MEA_acute_firing_rate_mean_up  8
# 6:  NHEERL_MEA_acute_synchrony_index_dn 15
# 7:  NHEERL_MEA_acute_synchrony_index_up 10
by.aeid <- cmc5[, .(added_hits = sum(hitc.new==1 & hitc.org==0), removed_hits = sum(hitc.new==0 & hitc.org == 1)), by = "aenm"]
for (i in 1:nrow(by.aeid)) {
  cat(unlist(by.aeid[i, 3]))
  cat("\n")
}

cmc5[hitc.org != hitc.new & hitc.org != -1 & hitc.new != -1, .N, by = "spid"] # spread across several spids, most only 1 change

# let's start with mfr
cmc5[grepl("firing",aenm) & hitc.org == 0 & hitc.new == 1] # 11 added hits - all have flag, either flags.new borderline active, or flags.org borderlien inactive
cmc5[grepl("firing",aenm) & hitc.org == 1 & hitc.new == 0] # 17 removed hits - literally all of these flags.org say "borderline active"
mc5_mc6[grepl("firing",aenm), unique(coff)] # original coff: 50.90747
mc5_mc6.1[grepl("firing",aenm), unique(coff)] # new coff: 52.91983

# let quantify the flags for all changes in hit call
# for added hit calls
cmc5[hitc.org == 0 & hitc.new == 1, .N, by = "flag_ids.new"]

# want to ask: are there any compounds with changed hits that don't have any borderline flags?
# which flags are "borderline"?
unique(cmc5$flags.new)
setdiff(unique(cmc5$flags.org),unique(cmc5$flags.new))

# deciding whether "ac50 < lowest conc" itself can be a borderline flag
cmc5[flags.new == "AC50 less than lowest concentration tested" | flags.org  == "AC50 less than lowest concentration tested"]
# no, I don't think that flag makes it a borderline hit
cmc5[flags.new == "Only one conc above baseline, active" | flags.org  == "Only one conc above baseline, active"]

# note differences in coff's by aeid:
compare.coff <- merge( mc5_mc6.1[, .(cndx1_coff = unique(coff)), by = "aenm"], mc5_mc6[, .(cndx12_coff = unique(coff)), by = "aenm"])
compare.coff[, diff := cndx1_coff - cndx12_coff]
compare.coff

# for added hit, if see these in flags.new, then the hit is less surprising
borderline_active_flags <- c(
"Borderline active"
)

# for added hit, if see these in flags.org, then the hit is less surprising
borderline_inactive_flags <- c(
"Borderline inactive",
"Multiple points above baseline, inactive"
)

# other potentially interesting flags
"Hit-call potentially confounded by overfitting"
"Noisy data"
"Gain AC50 < lowest conc & loss AC50 < mean conc"
"Less than 50% efficacy"
"Only highest conc above baseline, active" # we do have some legitimate hits like this, I think
"AC50 less than lowest concentration tested"
"Only one conc above baseline, active"

# assigne flags
cmc5[, any_ba_flags.new := any(unlist(lapply(borderline_active_flags, function(x) grepl(x, flags.new)))), by = c("spid","aeid")]
cmc5[, any_bi_flags.new := any(unlist(lapply(borderline_inactive_flags, function(x) grepl(x, flags.new)))), by = c("spid","aeid")]
cmc5[, any_ba_flags.org := any(unlist(lapply(borderline_active_flags, function(x) grepl(x, flags.org)))), by = c("spid","aeid")]
cmc5[, any_bi_flags.org := any(unlist(lapply(borderline_inactive_flags, function(x) grepl(x, flags.org)))), by = c("spid","aeid")]

# for added hit calls
cmc5[hitc.org == 0 & hitc.new == 1, .SD, .SDcols = c("spid","aenm",grep("any_",names(cmc5), value = T))] # 72 cases total, for all 7 aeids
cmc5[hitc.org == 0 & hitc.new == 1 & any_ba_flags.new == FALSE & any_bi_flags.org == FALSE]
# 14 cases
# syn index curve is just super noisy
# LDH TP0001411A06 -> oof, that really became a hit. On this scale, again crazy that the coff decreases with cndx1! But still, hill was a fit\
# LDH TP0001413A06 -> still a bit borderline, but no flags...

# see changes in bval for these comopunds
added.hits <- cmc5[hitc.org == 0 & hitc.new == 1]
added.hits <- merge(added.hits, mc3[, .(aeid = unique(aeid)), by = c("spid","apid")], by = c("aeid","spid")) # get the corresponding apid for each spid/aeid
(added.hits <- merge(added.hits, compare.bval[, .(aeid = unique(aeid)), by = c("bval.new","bval.org","apid")], by = c("aeid","apid")))
added.hits[, summary(bval.new - bval.org)]
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -50.8981  -3.0041   0.0002  -3.0076   4.9281  20.4484
# some are significant, others are not

# of the 14 w/o borderline flags, are bval's significantly different?
added.hits[any_ba_flags.new == FALSE & any_bi_flags.org == FALSE, .(aenm, signif(bval.new - bval.org,3), spid, chnm, apid)]
added.hits[any_ba_flags.new == FALSE & any_bi_flags.org == FALSE &!grepl("LDH",aenm), .(bval.new - bval.org)]


# for removed hit calls
cmc5[hitc.org == 1 & hitc.new == 0, .SD, .SDcols = c("spid","aenm",grep("any_",names(cmc5), value = T))] # 35 cases total, for all 7 aeids
removed.hits.noflags <- cmc5[hitc.org == 1 & hitc.new == 0 & any_bi_flags.new == FALSE & any_ba_flags.org == FALSE]
# 3 cases
removed.hits.noflags <- merge(removed.hits.noflags, mc3[, .(aeid = unique(aeid)), by = c("spid","apid")], by = c("aeid","spid")) # get the corresponding apid for each spid/aeid
(removed.hits.noflags <- merge(removed.hits.noflags, compare.bval[, .(aeid = unique(aeid)), by = c("bval.new","bval.org","apid")], by = c("aeid","apid")))


# can I point to any instances where an added hit call is due to larger in magnitude bval?
spidi <- "TP0001414C08" # added hit in burst number dn, aeid = 2443
mc3.1[spid == spidi & aeid == 2443, unique(bval)] # -12.45504
mc3.1[aeid == 2443, summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -96.386 -49.351 -33.425 -34.579 -17.552   6.678 
stripchart(mc3.1[aeid == 2443, unique(bval)], vertical = T, pch = 1, method = "jitter")
abline(h = -12.455) # yes, this is above the median... but there are lots of other at this level. Not a stand-out

# I guess this is the q:
# for plates where a compound was tested that was very potent/had effect at every conc, 
# did that affect the hit call for the other compounds on those plates?
# how do i define the prior?
med.resp.summ <- mc3.1[aeid == 2443 & wllt == 't' & cndx %in% c(1,2,3), .(med_resp_cndx = median(resp)), by = c("spid","cndx","apid")]
mc5_mc6[aeid == 2443, unique(coff)] # 56.5
med.resp.summ[med_resp_cndx > 56.5 & cndx == 1][order(apid)]
# spid cndx     apid med_resp_cndx
# 1: EPAPLT0154C11    1 20190411      68.99637
med.resp.summ[med_resp_cndx > 56.5 & cndx %in% c(1,2)][order(apid)]
# a few mroe cases
# so, on these plates, what is bval?
check.plates <- med.resp.summ[med_resp_cndx > 56.5 & cndx %in% c(1,2), apid]
# 8 instances. These look like very messy curves though.
mc3.1[apid %in% check.plates & aeid == 2443, .(bval = unique(bval)), by = "apid"][order(bval)]
# okay, so for burst number dn, for compounds that have a response at cndx1 or 2 (a median resp), 
# overall, the bval on those apid does not appear to be abnormal

# let's just compare bval for a sec
compare.bval <- merge(mc3.1[, .(bval.new = unique(bval)), by = c("aeid","apid")], mc3[, .(bval.org = unique(bval)), by = c("aeid","apid")])
plot(bval.new ~ bval.org, compare.bval, main = "bval as median of DMSO+cndx1 vs. bval as median of DMSO+cndx1&2\nfor Burst Number, Mean Firing Rate, Synchrony Index, and LDH", 
     ylab = "bval as median of DMSO+cndx1", xlab = "bval as median of DMSO+cndx1&2")
points(bval.new ~ bval.org, compare.bval[aeid == 2425 & apid == "20190411"],  pch = 1, cex = 2, lwd = 2, col = "blue")
points(bval.new ~ bval.org, compare.bval[aeid == 2426 & apid == "20190411"],  pch = 1, cex = 2, lwd = 2, col = "red")
points(bval.new ~ bval.org, compare.bval[aeid == 2426 & apid == "20160309"],  pch = 1, cex = 2, lwd = 2, col = "black")
points(bval.new ~ bval.org, compare.bval[aeid == 2455 & apid == "20190528"],  pch = 1, cex = 2, lwd = 2, col = "green")
points(bval.new ~ bval.org, compare.bval[aeid == 2443 & apid == "20190328"],  pch = 1, cex = 2, lwd = 2, col = "purple")
points(bval.new ~ bval.org, compare.bval[aeid == 2443 & apid == "20160419"],  pch = 1, cex = 2, lwd = 2, col = "orange")
points(bval.new ~ bval.org, compare.bval[aeid == 2443 & apid == "20190502"],  pch = 1, cex = 2, lwd = 2, col = "gray")
points(bval.new ~ bval.org, compare.bval[aeid == 2425 & apid == "20160309"],  pch = 1, cex = 2, lwd = 2, col = "pink")
abline(0,1)
# what's going on where bval is very different?
compare.bval[bval.org > 10]
# aeid     apid   bval.new bval.org
# 1: 2425 20190411 -11.046886 30.28784
# 2: 2426 20190411  -1.289768 49.45127
# 3: 2442 20190411 -11.046886 30.28784
# 4: 2443 20190411  -1.289768 49.45127
compare.bval[abs(bval.org - bval.new) > 15][order(abs(bval.org - bval.new))]

# for this plate, let's see a boxplot by cndx. mc3 has the original cval's from lvl 2, unaffected by bval
aeidi <- 2425
apidi <- "20190411"
apid.check <- mc3[apid == apidi] # getting previous mc3, where wllt == "n" only includes DMSO
apid.check[wllt == "n", cndx := 0]
boxplot(cval ~ cndx, apid.check[aeid == aeidi], main = paste0(mc5_mc6[aeid == aeidi, unique(aenm)], " Changes in bval for apid ",apidi))
abline(h = compare.bval[apid == apidi & aeid == aeidi, bval.new], col = "blue") # bval with cndx 1 only
abline(h = compare.bval[apid == apidi & aeid == aeidi, bval.org], col = "red") # bval wiht cndx 1&2
abline(h = compare.bval[aeid == aeidi, quantile(bval.new)[2:4]], lty = "dashed", col = "blue") # bval with cndx 1 only
abline(h = compare.bval[aeid == aeidi, quantile(bval.org)[2:4]], lty = "dashed", col = "red") # bval wiht cndx 1&2
legend(x = "topleft", bg = "transparent", legend = c("bval with DMSO+cndx1","bval with DMSO+cndx1&2"), pch = c(19,19), col = c("blue","red"))

# what was tested on these plates?
spids.tested <- mc3[apid == apidi & aeid == aeidi, unique(spid)]
cmc5[spid %in% spids.tested & aeid == aeidi & spid != "DMSO", .(spid, chnm, hitc.org, hitc.new, aeid, flags.new, flags.org, modl_ga.new, modl_ga.org)]
# how many added/removed hits?


# what are the changes in potency?
cmc5[hitc.org == 1 & hitc.new == 1] # 655 shared hits
cmc5[hitc.org == 1 & hitc.new == 1, summary(modl_ga.new - modl_ga.org)]
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -3.031846 -0.021176  0.000000 -0.019214  0.007458  0.715316
# correlation plot:
plot(modl_ga.new ~ modl_ga.org,cmc5[hitc.org == 1 & hitc.new == 1], ylab = "log10(AC50) - DMSO + 1st lowest conc only used to normalize",
     xlab = "log10(AC50) - DMSO + 1st and 2nd lowest conc's used to normalize", 
     main = "Correlation Plot of log10(AC50) with Different Normalization Approaches\nfor Burst Number, Mean Firing Rate, Synchrony Index, and LDH")
abline(a = 0, b = 1)
abline(a = -1, b = 1, lty = "dashed")
abline(a = 1, b = 1, lty = "dashed")
boxplot(cmc5[hitc.org == 1 & hitc.new == 1, modl_ga.new - modl_ga.org])

# check out the pionts that differ by more than 1 log unit
diff.ac50 <- cmc5[hitc.org == 1 & hitc.new == 1 & abs(modl_ga.new - modl_ga.org) > 1, .(spid, chnm, aeid, aenm, modl_ga.new, modl_ga.org, flags.new, flags.org)]
diff.ac50 <- merge(diff.ac50, mc3[, .(aeid = unique(aeid)), by = c("spid","apid")]) # get the corresponding apid
(diff.ac50 <- merge(diff.ac50, compare.bval[, .(aeid = unique(aeid)), by = c("bval.new","bval.org","apid")], by = c("aeid","apid")))
# aeid     apid          spid                                 aenm modl_ga.new modl_ga.org   bval.new   bval.org
# 1: 2425 20190411 EPAPLT0154D03 NHEERL_MEA_acute_firing_rate_mean_up  -1.5527665    0.192856 -11.046886  30.287837
# 2: 2426 20190326 EPAPLT0154A06     NHEERL_MEA_acute_burst_number_up  -2.5212633   -0.738856 -66.528067 -53.074627
# 3: 2438 20150728  TP0001411D01  NHEERL_MEA_acute_synchrony_index_up  -1.6198767    1.411970  -7.819828  -7.289962
# 4: 2443 20160309  TP0001414C09     NHEERL_MEA_acute_burst_number_dn   0.2138343    1.273610 -12.455036 -30.807096
diff.ac50[, bval.new - bval.org]

# Bis 2... phosphate
stripchart(cval ~ cndx, apid.check[aeid == aeidi, .(apid, cndx, cval)], pch = "", vertical = T, method = "jitter")
stripchart(cval ~ cndx, apid.check[aeid == aeidi & spid != "EPAPLT0154A06", .(apid, cndx, cval)], pch = 1, vertical = T, method = "jitter")
stripchart(cval ~ cndx, apid.check[aeid == aeidi & spid == "EPAPLT0154A06", .(apid, cndx, cval)], pch = 19, vertical = T, method = "jitter", add = T, col = "blue")
# this compound def has the highest values... 


# checking out LDH separately, since bval's would be much smaller order of magnitude
plot(bval.new ~ bval.org, compare.bval[aeid == 2439], main = "bval as median of DMSO+cndx1 vs. bval as median of DMSO+cndx1&2\nfor LDH only", 
     ylab = "bval as median of DMSO+cndx1", xlab = "bval as median of DMSO+cndx1&2")
abline(a = 0, b = 1)
compare.bval[aeid == 2439, summary(abs(bval.new - bval.org))]
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.001012 0.002283 0.003404 0.004112 0.024000 
# very similar!

# aeid                                   V1
# 1: 2425 NHEERL_MEA_acute_firing_rate_mean_up
# 2: 2426     NHEERL_MEA_acute_burst_number_up
# 3: 2438  NHEERL_MEA_acute_synchrony_index_up
# 4: 2439              NHEERL_MEA_acute_LDH_up
# 5: 2440               NHEERL_MEA_acute_AB_up
# 6: 2442 NHEERL_MEA_acute_firing_rate_mean_dn
# 7: 2443     NHEERL_MEA_acute_burst_number_dn
# 8: 2455  NHEERL_MEA_acute_synchrony_index_dn

