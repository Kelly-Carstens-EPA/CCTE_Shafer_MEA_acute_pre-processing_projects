#-----------------------------------------------------------------------------------#
# Suggested methods for MEA Acute Update
# Jan 26, 2022
#-----------------------------------------------------------------------------------#
library(tcpl)
library(data.table)
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')


#-----------------------------------------------------------------------------------#
# Update methods
#-----------------------------------------------------------------------------------#
aeid.tb <- tcplLoadAeid(fld = 'asid', val = 20, add.fld = c("acid","acnm"))
aeid.tb <- aeid.tb[grepl('_acute_',acnm)]
aeid.tb <- aeid.tb[!grepl('(LDH)|(AB)',acnm)] # no changes needed to methods for LDH and AB endpoints

# Excluding these 2 endpoints. Suggest deleting them from invitrodb entirely
aeid.tb <- aeid.tb[!(aenm %in% c('CCTE_Shafer_MEA_acute_active_electrodes_number_up','CCTE_Shafer_MEA_acute_bursting_electrodes_number_up'))]


# Level 2 - no adjustment
tcplMthdLoad(lvl = 2L, id = aeid.tb$acid)
# yep, all are none


# Level 3 
tcplMthdClear(lvl = 3L, id = aeid.tb$aeid, type = 'mc')

# DN endpoints methods:
# bval as the median of DMSO + cndx 1 and 2
# pval = -100
# resp.pc
pval.neg.100 = function(aeids) {
  
  e1 <- bquote(dat[J(.(aeids)), 
                   pval := -100, 
                   by = list(aeid)])
  list(e1)
  
}
dn.aeids <- aeid.tb[grepl('_dn',aenm), aeid]
tcplMthdList(lvl = 3L)
tcplMthdAssign(lvl = 3L, id = dn.aeids, mthd_id = c(17, # bval.apid.nwllslowconc.med,
                                                    '(fill with new mthd_id)', # pval = -100
                                                    5   # resp.pc
                                                    ), ordr = c(1:3), type = 'mc')

# UP endpoints methods:
# bval as the median of DMSO + cndx 1 and 2
# pval  = 99%ile of all rvals from wllt t
# resp.pc
pval.twlls.99percentile = function(aeids) {
  
  e1 <- bquote(dat[J(.(aeids)), 
                   pval := quantile(cval[wllt == "t"], probs = 0.99, na.rm = TRUE), 
                   by = list(aeid)])
  list(e1)
  
}
up.aeids <- aeid.tb[grepl('_up',aenm), aeid]
tcplMthdAssign(lvl = 3L, id = up.aeids, mthd_id = c(17, # bval.apid.nwllslowconc.med,
                                                    '(fill with new mthd_id)', # pval = 99%ile of rvals
                                                    5  # resp.pc
                                                    ),  ordr = c(1:3), type = 'mc')


# Level 4 - bmad.aeid.lowconc.nwells for all
tcplMthdLoad(lvl = 4L, id = aeid.tb$aeid)


# Level 5
# for active electrodes and actively bursting electrodes in the down direction, use pc30 
# (bmad is 0, so can't use a multiple of bmad as the coff)
set.pc30.coff.aeids <- aeid.tb[aenm %in% c('CCTE_Shafer_MEA_acute_active_electrodes_number_dn',
                                          'CCTE_Shafer_MEA_acute_bursting_electrodes_number_dn'),aeid]
tcplMthdClear(lvl = 5L, id = set.pc30.coff.aeids, type = 'mc')
tcplMthdList(lvl = 5L)
tcplMthdAssign(lvl = 5L, id = set.pc30.coff.aeids, mthd_id = c(19), ordr = 1, type = 'mc')
# For all other MEA endpoints, use bmad3 for coff (should already be set)
tcplMthdLoad(lvl = 5L, id = setdiff(aeid.tb$aeid, set.pc30.coff.aeids))


# Level 6
# can use existing flags
# or possibly remove flag 17, for all or just for up?


#-----------------------------------------------------------------------------------#
# Run tcpl
#-----------------------------------------------------------------------------------#

tcplRun(slvl = 1L, elvl = 6L, id = unique(aeid.tb$acid), type = 'mc')


#-----------------------------------------------------------------------------------#
# Which endpoints to release
#-----------------------------------------------------------------------------------#
# See mea_acute_status_by_aenm_2022-01-27.csv

