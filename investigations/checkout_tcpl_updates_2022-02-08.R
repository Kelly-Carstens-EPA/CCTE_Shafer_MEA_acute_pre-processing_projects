# ----------------------------------------------------------------------- #
# Checking out results of MEA Acute update!
# (just changed from normalized with cval - bval to 
# (cval - bval)/(-100 - bval) for down, and (cval - bval)/(99th%ild - bval) for up)
# Feb 8, 2022
#
# Things I could check
# - Are the methods what I think they should be
# - How closely do the resutls agree with what I predicted in the sandbox? (for Method 3)
# - Check out some positive and negative controls
# - Confirm the endpoints Madison listed are good to do (confirming sc only for the key endpoints, not nAE nABE, generally agrees with my list)
# ----------------------------------------------------------------------- #

library(data.table)
library(tcpl)
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')
library(stringi)
library(ggplot2)

# Checkout the methods ----------------------------------------------------

aeid.tb <- tcplLoadAeid(fld = 'asid', val = 20, add.fld = c('anm','acnm','acid'))
aeid.tb[, dir := stri_extract(aenm, regex = '.{2}$')]
acute.tb <- aeid.tb[anm == 'CCTE_Shafer_MEA_acute']
tcplMthdLoad(lvl = 2L, id = unique(acute.tb$acid), type = 'mc')
# none for all
acute.mc.aeids <- aeid.tb[anm == 'CCTE_Shafer_MEA_acute' & grepl('MEA_acute',aenm),aeid]
mc3.mthds <- tcplMthdLoad(lvl = 3L, id = acute.mc.aeids, type = 'mc')
mc3.mthds.cond <- merge(mc3.mthds[, .(mthds = paste0(mthd, collapse = ',')), by = .(aeid)], 
                        aeid.tb, by = 'aeid', all.x = T)
mc3.mthds.cond[, .N, by = .(mthds, dir)]
#                                                     mthds dir  N
# 1:        bval.apid.nwllslowconc.med,pval.neg.100,resp.pc  dn 43
# 2:    bval.apid.nwllslowconc.med,pval.twlls.99pct,resp.pc  up 41
# 3: bval.apid.nwllslowconc.med,pval.apid.pwlls.med,resp.pc  up  1
# 4:           bval.apid.nwllslowconc.med,pval.zero,resp.pc  dn  1
# 5:                  bval.apid.nwllslowconc.med,resp.logfc  up  2
# dn adn up main cases look correct!
# cases 3 & 4 are going to be the LDH adn AB
# What are these 2 up endpoints that still have logfc for up?
mc3.mthds.cond[mthds == 'bval.apid.nwllslowconc.med,resp.logfc', .(aenm, aeid)]
# aenm aeid
# 1:   CCTE_Shafer_MEA_acute_active_electrodes_number_up 2961
# 2: CCTE_Shafer_MEA_acute_bursting_electrodes_number_up 2973
# ah, yes, the endpoints that will be deleted

# Do the new methods look correct in BitBucket/github?
# Hmm, not seeing teh updates in either
tcpl:::mc3_mthds
# Hmm, not seeing the new methods here either
# have the updates been made?
# Or, maybe I woudl need to re-download tcpl?

tcplMthdList(lvl=3L, type = 'mc')
# 46:          48                 pval.twlls.99pct                                                     pval = 99%-ile of all cvals with wllt of t
# 47:          49                     pval.neg.100                                                pval = -100 for endpoints in the down direction
# Okay, cool!

# I'm okay with trusting these functions implemented in a way that makes sense
# they were not super complicated



# Compare results with what I predicted in sandbox ------------------------

# load localbox
load('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/normalization_ideas_Dec2020/sbox_run/localbox_dat_2021-11-08.RData')
cat(localbox_run_summary)
# Used negshift followed by resp.pc for dn, same with multneg1 for up.
# For active_electrodes_number_dn and bursting_electrodes_number_dn, set coff to 20% (wanted 30%, but not an option in tcplLite).
# For active_electrodes_number_up and bursting_electrodes_number_up, set coff := 70% (i.e., I set the coff so high that there should not be any hits)
# Did not run AB nor LDH
# Normalized at level 3 with bval of n and 2 low, pval = 0, resp.pc.
# Excluded HESI Convulsant
# Date ran: 2022-01-23
# Date saved:2022-01-24.
localbox_mc5_mc6 <- mc5_mc6

# Load tcpl results
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=acute.mc.aeids, type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=acute.mc.aeids, type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), mc6_mthd = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
rm(mc5, mc6, mc6_collapsed)

# Compare hits, ac50s!
setdiff(localbox_mc5_mc6$spid, mc5_mc6$spid) # empty
setdiff(mc5_mc6$spid, localbox_mc5_mc6$spid) # empty
setdiff(localbox_mc5_mc6$aenm, mc5_mc6$aenm) # empty
setdiff(mc5_mc6$aenm, localbox_mc5_mc6$aenm) # [1] "CCTE_Shafer_MEA_acute_LDH_up" "CCTE_Shafer_MEA_acute_AB_dn" 
usecols <- c('spid','chnm','dsstox_substance_id','aeid','aenm','hitc','fitc','flags','mc6_mthd','modl_ga','max_med','modl_tp','coff')
comp.tb <- merge(localbox_mc5_mc6[!aenm %in% c('CCTE_Shafer_MEA_acute_bursting_electrodes_number_up',
                                               'CCTE_Shafer_MEA_acute_active_electrodes_number_up'), ..usecols], 
                 mc5_mc6[!grepl('(LDH)|(AB)',aenm) & !aenm %in% c('CCTE_Shafer_MEA_acute_bursting_electrodes_number_up',
                                                                  'CCTE_Shafer_MEA_acute_active_electrodes_number_up'), ..usecols], by = c('aenm','spid'), 
                 all = T, suffixes = c('.l','.t'))
comp.tb[, dir := stri_extract(aenm, regex = '.{2}$')]

# Confirm the coff's generally agree
ggplot(comp.tb, aes(x = coff.l, y = coff.t))+
  geom_point(aes(color = factor(dir)))+
  geom_abline(slope = 1, intercept = 0)
# the 2 deviating dn points are nAE, nABE - which is okay, Iknow these are different
# Some deviation i the up direction, with slightly higher coffs in tcpl
# we'll see how this plays out

# Compare the results
comp.tb[, .N, by = .(hitc.l, hitc.t)][order(hitc.l, hitc.t)]
#    hitc.l hitc.t     N
# 1:     -1     -1   820
# 2:      0      0 36632
# 3:      0      1    80
# 4:      1      0    96
# 5:      1      1  6640
# Okay, so we have some differences...
# Let's check it out

# Checking out hits in tcpl, not in localbox
comp.tb[hitc.l == 0 & hitc.t == 1, .N, by = .(aenm, coff.l, coff.t)][order(-N)]
#                                                                         aenm    coff.l    coff.t  N
# 1:                         CCTE_Shafer_MEA_acute_active_electrodes_number_dn 50.000000 30.000000 18
# 2:                       CCTE_Shafer_MEA_acute_bursting_electrodes_number_dn 50.000000 30.000000 11
#  I know I changed the coffs here, so this makes sense
# all others endpoints 4 or fewer cases
comp.tb[hitc.l == 0 & hitc.t == 1, .N, by = .(spid)][order(-N)]
# 62 different spid
# how many are borderline?
comp.tb[hitc.l == 0 & hitc.t == 1, .N, by = .(borderline = grepl('12',mc6_mthd.l) | grepl('11',mc6_mthd.t),
                                              nAE_or_nABE = aenm %in% c('CCTE_Shafer_MEA_acute_active_electrodes_number_dn',
                                                                        'CCTE_Shafer_MEA_acute_bursting_electrodes_number_dn'))]
#    borderline nAE_or_nABE  N
# 1:       TRUE        TRUE 26
# 2:       TRUE       FALSE 49
# 3:      FALSE        TRUE  3
# 4:      FALSE       FALSE  2
# so just 2/90 cases are not borderline and are not in nABE, nAE endpoints..
comp.tb[hitc.l == 0 & hitc.t == 1 & !(grepl('12',mc6_mthd.l) | grepl('11',mc6_mthd.t)) & !aenm %in% c('CCTE_Shafer_MEA_acute_active_electrodes_number_dn','CCTE_Shafer_MEA_acute_bursting_electrodes_number_dn')]
# coff's are quite similar, as well as max_med's 
# case 1: modl_tp in localbox didn't get above coff, but did in tcpl. TCPL has flag "only one conc above baseline"
# case 2: localbox has flag "noisy data", looks like it fit the cnst model. TCPL has flag "only one conc above baseline"
# neither endpoint looks like one that we are releasing
# I think these are just 2 fluke cases, not a deal breaker to me

# Checking out hits in localbox, not in tcpl
# Any endpoints or spids seem to be the main culprit?
comp.tb[hitc.l == 1 & hitc.t == 0, .N, by = .(aenm, coff.l, coff.t)][order(-N)]
# spread out over several endpoints
# the top 5 are all dn, so coffs are quite similar
comp.tb[hitc.l == 1 & hitc.t == 0, .N, by = .(spid, chnm.t)][order(-N)]
# 84 different spid
# nope

# how many are borderline?
comp.tb[hitc.l == 1 & hitc.t == 0, .N, by = .(grepl('11',mc6_mthd.l) | grepl('12',mc6_mthd.t))]
#    grepl  N
# 1:  TRUE 92
# 2: FALSE  4
# oh wow, all but a handful are borderline
comp.tb[hitc.l == 1 & hitc.t == 0 & !(grepl('11',mc6_mthd.l) | grepl('12',mc6_mthd.t))]
# in all 4 cases, had 2-3 flags in localbox
# in every case, max_med still gets above coff in tcpl, just fits cnst model best

# Plot the 4 cases where not borderline hit in localbox, not in tcpl
plot.spid.aenms <- comp.tb[hitc.l == 1 & hitc.t == 0 & !(grepl('11',mc6_mthd.l) | grepl('12',mc6_mthd.t)), .(spid, aenm)]
setkey(mc5_mc6, aenm, spid) # super weird, I had to switch the order on these...
plot.m4ids <- mc5_mc6[.(plot.spid.aenms), m4id]
for (m4id in plot.m4ids) {
  tcplPlotM4ID(m4id, lvl = 6L)
}
# I mean, yeah, these might have some real activity
# but these are 4 one-off cases
# I'm not seeing anything systemic

# Compare AC50s
comp.tb[, plot_val.l := ifelse(hitc.l == 1, modl_ga.l, 4)]
comp.tb[, plot_val.t := ifelse(hitc.t == 1, modl_ga.t, 4)]
ggplot(comp.tb, aes(x = plot_val.l, y = plot_val.t)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  xlab('AC50 (log10) localbox')+
  ylab('AC50 (log10) tcpl')+
  ggtitle('Comparison of AC50 values in Localbox vs TCPL')
# Pretty good
# but there are some definite outliers
# But again, I think this is just noise 
# -> I don't think this points to any systemic issues

# Check out where more than a log order of difference
comp.tb[hitc.l == 1 & hitc.t == 1 & abs(modl_ga.l - modl_ga.t) > 1]
comp.tb[hitc.l == 1 & hitc.t == 1 & abs(modl_ga.l - modl_ga.t) > 1, .N, by = .(aenm)]
# spread over 24 endpoints
comp.tb[hitc.l == 1 & hitc.t == 1 & abs(modl_ga.l - modl_ga.t) > 1, .N, by = .(spid)]
# and 25 spid
# really just 30 cases of this - I don't think it's a big deal


# Confirm with positive and negative controls -----------------------------

plot.m4ids <- mc5_mc6[chnm == 'Emamectin benzoate' & grepl('firing_rate_mean_dn',aenm), m4id]
for (m4id in plot.m4ids) {
  tcplPlotM4ID(m4id, lvl = 6L)
}
# both are solid hits, sweet!

plot.m4ids <- mc5_mc6[chnm == 'Heptachlor epoxide B' & grepl('firing_rate_mean_up',aenm), m4id]
for (m4id in plot.m4ids) {
  tcplPlotM4ID(m4id, lvl = 6L)
}
# interesting gnls structure, but looks pretty good to me!

plot.m4ids <- mc5_mc6[chnm == 'Saccharin' & grepl('firing_rate_mean_up',aenm), m4id]
for (m4id in plot.m4ids) {
  tcplPlotM4ID(m4id, lvl = 6L)
}
# still a hit, sadly
plot.m4ids <- mc5_mc6[chnm == 'Sucralose' & grepl('firing_rate_mean_.{2}$',aenm), m4id]
for (m4id in plot.m4ids) {
  tcplPlotM4ID(m4id, lvl = 6L)
}
# not a hit, at least for this endpoint
mc5_mc6[, dir := stri_extract(aenm, regex = '.{2}$')]
mc5_mc6[chnm %in% c('Saccharin','Sucralose') & hitc == 1, .N, by = .(chnm, dir)]
#         chnm dir N
# 1: Saccharin  up 6
# 2: Sucralose  up 2
# but this is not unexpected

plot.m4ids <- mc5_mc6[grepl('Glufosinate',chnm) & grepl('firing_rate_mean_.{2}$',aenm), m4id]
for (m4id in plot.m4ids) {
  tcplPlotM4ID(m4id, lvl = 6L)
}
# a couple hits, but most don't quite get above the coffs


# Confirm the endpoints to select -----------------------------------------

# From madison: 
# "I will update the export ready status for the MEA_acute endpoints (aeids 2454-2483, 2540-2541) after you review the Katie’s reprocessing. "
my.acute.tb <- as.data.table(read.csv('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/normalization_ideas_Dec2020/update_materials_sent_Jan2022/mea_acute_status_by_aenm_2022-01-27.csv'))
my.acute.tb <- merge(my.acute.tb, aeid.tb, all.x = T, by = 'aenm')
my.acute.tb[aeid.x != aeid.y] # empty, good
my.acute.tb[can_be_released == 1, sort(aeid.x)]
# [1] 2033 2034 2035 2036 
# 2454 2455 2456 2457 2458 2459 2460 2461 2462 2463 2464 2465 2466 2467 2468
# 2469 2470 2471 2472 2473 2474 2475 2476 2477 2478 2479 2480 2481 2482 2483 
# 2540 2541
# 2033-2036 (these are the sc only endpoints)
# 2454 - 2483 (these are the mc endpoints!)
# 2540-2541 - these are LDH, AB
# cool beans!

# triple confirm does not include nAE up, nABE dn
aeid.tb[aenm %in% c('CCTE_Shafer_MEA_acute_bursting_electrodes_number_up',
                    'CCTE_Shafer_MEA_acute_active_electrodes_number_up'), .(aenm, aeid)]
# yep, these are not in the range of aeid's to release



# Conclusion --------------------------------------------------------------

# Yes there are some differences with what I pipelined in tcplLite,
# But the vast majority seem borderline, adn those that aren't seem spurious (rare, not affecting known positives)
# Of course, I do still have concerns about e.g. the noise level in the data,
# But that's not what I'm checkign here - just that nothing hugely unexpected
