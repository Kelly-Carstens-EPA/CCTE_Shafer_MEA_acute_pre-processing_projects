# Verifying up endpoints with 
# pval as 99th percentile of all rvals

# script to run MEA acute lvl 0 data
library(tcpl)
library(data.table)
library(RMySQL)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# REVIEW REGISTRATION>UPDATE --------------------------------------------------------------------------------

tcplLoadAcid(fld = 'asid', val = 20)

# get the names of just the 15 components that we care about
main15_tb <- as.data.table(read.csv('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/mea_acute_main15_acnm_aenm_2020-12-08.csv',stringsAsFactors=F))


# REGISTER NEW ACID/AEID --------------------------------------------------------------------------------


# PREPARE MC0 ------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
load("lvl0_snapshots/mea_acute_lvl0_2020-07-29.RData")

# Assign acid's
acnms <- main15_tb$acnm
acid.acnm <- tcplLoadAcid(fld = "acnm", val = acnms) # I have only registered the top 15 adn LDH/AB
mea_acute_lvl0 <- merge(mea_acute_lvl0, acid.acnm, by = "acnm")
mea_acute_lvl0[, acnm := NULL]

# I have to remove a few compounds that are not registered
mc0 <- mea_acute_lvl0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01",
                                    "EX000371", "EX000372", "EX000373", "EX000374", "EX000408", "EX000411"))]
rm(mea_acute_lvl0)

# Pre-normalize time

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

# define the pval
# pval.twlls.99th = function(aeids) {
#   e1 <- bquote(dat[J(.(aeids)), 
#                    pval := quantile(cval[wllt == "t"], probs = c(0.99), na.rm = TRUE), 
#                    by = list(aeid)])
#   list(e1)
# }
mc0[,
    pval := quantile(rval[wllt == 't'], probs = 0.99, na.rm = TRUE),
    by = list(acid)]

# calculate resp.pc
mc0[, rval2 := (rval - bval) / (pval - bval) * 100]
mc0[, summary(rval2)]
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -1721.501   -12.359    -0.409    -2.313    10.187  4962.708
mc0[is.na(rval2)] # empty, awesome!!
mc0[is.na(bval)] # okay, also empty!!
# right, active electrodes endpoints have already been removed

mc0[, rval := NULL]
mc0[, c('bval','cndx','repi') := NULL]
mc0[, pval := NULL]
setnames(mc0, old = 'rval2', new = 'rval')

nrow(mc0) # 199631
length(unique(mc0$acid)) # 17

# considering LDH/AB...
# I might want to play with that, but I am going to not do that right now

# removing ldh/ab
mc0 <- mc0[acid %in% acid.acnm[!grepl('(AB)|(LDH)',acnm),acid]]
nrow(mc0) #173K

# close any connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplConfList() # confirming db
rm(list = setdiff(ls(), c('mc0','acid.acnm','acnms','main15_tb')))
tcplWriteLvl0(mc0, type = "mc") # Dec 11, 2020, 5:42pm


# REGISTER/CONFIRM METHODS -----------------------------------------
acids <- acid.acnm$acid
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # none for all


# lvl 3
dn_aeids <- aeid.info[grepl('_dn',aenm) & !grepl('(LDH)|(AB)',aenm),aeid]
tcplMthdLoad(lvl=3, id = dn_aeids) # all are none except for 1...
tcplMthdClear(lvl=3, id = 3255, type = "mc")
tcplMthdAssign(lvl = 3, id = 3255, mthd_id = c(1), ordr = c(1), type = "mc") # none for all
tcplMthdLoad(lvl=3, id = dn_aeids)

up.mea <- aeid.info[grepl("_up",aenm) & !(grepl("(LDH)|(AB)",aenm)), aeid]
tcplMthdClear(lvl=3, id = up.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = up.mea, mthd_id = c(1), ordr = c(1), type = "mc") # initialize resp
tcplMthdLoad(lvl=3, id = up.mea)

# lvl 4
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
# bmad.aeid.lowconc.nwells

# lvl 5
tcplMthdLoad(lvl=5L, id = aeid.info$aeid)
# bmad3

# lvl 6
mc6.mthds <- tcplMthdLoad(lvl=6L, id = aeid.info$aeid)
mc6.mthds[, .N, by = .(aeid)] # 10 for all except for the 2 viability endpoints have 11
tcplMthdList(lvl=6L, 'mc')
tcplMthdClear(lvl = 6L, id = aeid.info$aeid, type = "mc")

# removed "efficacy.50" flag - actually, I changed my mind, I'm going to leave it in
lvl6_mthds <- tcplMthdList(lvl=6L, 'mc')
tcplMthdAssign(lvl = 6L, id = aeid.info[grepl('(LDH)|(AB)',aenm),aeid], mthd_id = lvl6_mthds$mc6_mthd_id, ordr = c(1:nrow(lvl6_mthds)), type = "mc")
lvl6_mthds <- lvl6_mthds[!(mc6_mthd_id %in% c(19))] # actually, ...
tcplMthdAssign(lvl = 6L, id = aeid.info[!grepl('(LDH)|(AB)',aenm),aeid], mthd_id = lvl6_mthds$mc6_mthd_id, ordr = c(1:nrow(lvl6_mthds)), type = "mc")
# tcplMthdAssign(lvl = 6L, id = up.mea, mthd_id = lvl6_mthds$mc6_mthd_id, ordr = c(1:nrow(lvl6_mthds)), type = "mc")
tcplMthdLoad(lvl=6L, id = aeid.info[,aeid])

# view all 
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = aeid.info$aeid))
}


# ----------------------------------- tcpl run with hack function
rm(list = setdiff(ls(),c('acids','aeid.info','acid.acnm')))

# quick test: do I still need to use tcplLoadData?
# test <- tcplLoadData(lvl=3L, fld = 'aeid', val = 2440, type = 'mc')
# nope, this is still taking hours!

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

acids <- acid.acnm[!grepl('(LDH)|(AB)',acnm),acid]
up_aeids <- aeid.info[grepl('_up',aenm) & !grepl('(LDH)|(AB)',aenm),aeid]
dn_aeids <- aeid.info[grepl('_dn',aenm) & !grepl('(LDH)|(AB)',aenm),aeid]
tcplMthdLoad(lvl=3L, id = up_aeids, type = 'mc') # all none
tcplMthdLoad(lvl=3L, id = dn_aeids, type = 'mc') # all none

# Dec 14, 10:48 am
(res <- tcplRun(slvl = 1L, elvl = 3L, id = acids, type = "mc"))
tcplRun(slvl = 4L, elvl = 6L, id = up_aeids, type = "mc")
# yay!

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# Save data and plots ----------------------------

load("lvl0_snapshots/dat4_2020-07-29.RData")

# I'm just going to save the plots that I know I want to see, for now.
check.chem <- c('Chlordane','Dieldrin','Endosulfan','Endrin','Glufosinates',
                'Heptachlor','Heptachlor epoxide B','Lindane','Kepone','Methoxychlor',
                'Permethrin','p,p`-DDT','Mirex','Nicotine','Clofenotane')
# I don't think we tested p,p'-DDT, only p,p'-DDD
dat4[grepl('p,p',treatment), unique(treatment)] # empty...
tested.chem <- tcplLoadChem(field = 'spid', val = dat4[wllt == 't', unique(spid)])
tested.chem[chnm %in% check.chem]

gf_spids <- dat4[apid == '20190924' & wllt == 't', unique(spid)]
tested.chem[spid %in% gf_spids] #empty...?
# ah, right, these are spids that  I had to remove because I had troubles registering them in the sandbox
# So I just won't have any GF data

use.spid <- tested.chem[chnm %in% check.chem, unique(spid)]


# Get pdf plots of each aeid but only with the specified use.spid
aeids <- up_aeids

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
mc0_id_table <- dbGetQuery(con, paste0("SELECT acid, m0id FROM mc0 WHERE acid IN(",paste0(acid.acnm[!grepl('(LDH)|(AB)',acnm),acid],collapse=','),") AND spid IN('",paste0(use.spid,collapse="','"),"');"))
dbDisconnect(con)
setDT(mc0_id_table)

odir <- 'L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/figs/sbox_run_2020-12-14'

for (aeidi in aeids) {

  # m0ids <- mc0[spid == chem_spid & is.element(apid, chemsetplates), m0id]
  # agg <- tcplLoadData(lvl = "agg", fld = c("m0id"), val = m0ids, type = "mc")
  # dat <- tcplLoadData(lvl = 5L, fld = c("aeid","m4id"), val = list(aeids, agg[, unique(m4id)]), type = "mc") # always lvl 4 or 5
  # flgs <- tcplLoadData(lvl = 6L, fld = "m4id", val = agg[, unique(m4id)], type = "mc")
  
  aeidi.info <- tcplLoadAeid(fld = 'aeid', val = aeidi, add.fld = 'acid')
  # m0ids <- dbGetQuery(con, paste0("SELECT m0id FROM mc0 WHERE acid='",unique(aeidi.info$acid),"' AND spid IN('",paste0(use.spid,collapse="','"),"');"))
  m0ids <- mc0_id_table[acid == unique(aeidi.info$acid), m0id]
  agg <- tcplLoadData(lvl = "agg", fld = c("m0id"), val = m0ids, type = "mc")
  dat <- tcplLoadData(lvl = 5L, fld = c("m4id"), val = list(agg$m4id), type = "mc") # always lvl 4 or 5
  flgs <- tcplLoadData(lvl = 6L, fld = "m4id", val = agg$m4id, type = "mc")

  # make fname
  fname <- file.path(odir, paste(aeidi.info$aenm, "AEID",aeidi, "select_up_positives", as.character.Date(Sys.Date()), "plots.pdf", sep = "_"))

  graphics.off()
  pdf(file = fname, height = 6, width = 10, pointsize = 8.5)
  par(mfrow=c(1,1))
  tcplPlotFits(dat = dat, agg = agg, flg = flgs, ordr.fitc = TRUE)
  graphics.off()
  cat(fname, "complete\n")
}


con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))

# get mc3, add spid, apid, rowi, coli, etc from mc0, add cndx, repi, cval from mc1, add cval from mc2
mc3 <- dbGetQuery(con, paste0("SELEct m0id, aeid, acid, bval, pval, logc, resp FROM mc3 WHERE aeid IN (",paste0(up_aeids,collapse=","),")"))
setDT(mc3)
mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(acids,collapse=","),")"))
mc3 <- merge(mc3, mc1, all.x = T)
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = acids, type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc,rval)], all.x = T)

# get mc5_mc6 data
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=up_aeids, type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=up_aeids, type = "mc")
mc6_collapsed <- mc6[, .(flag_length = length(unique(flag)), mc6_mthd = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)

# get the methods
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = up_aeids, "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = up_aeids, "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = up_aeids, "mc")

# save the data
dbDisconnect(con)
sbox_run_summary <- paste0("Pre-normalizing mea acute up endpoints with pval as 99th percentiles of *all* rval's where wllt=='t', rval := (rval - bval)/(pval - bval)*100.\nLvl0: mea_acute_lvl0_2020-07-29.Rdata.\nDate ran: 2020-12-14.")
save(sbox_run_summary, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, 
     file = file.path(paste0("C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/normalization_ideas_Dec2020/sbox_run/sbox_run_",as.character.Date(Sys.Date()),".RData")))
rm(list = c("mc5","mc6","mc6_collapsed"))

# ---------------------------

# let's check out the results!
