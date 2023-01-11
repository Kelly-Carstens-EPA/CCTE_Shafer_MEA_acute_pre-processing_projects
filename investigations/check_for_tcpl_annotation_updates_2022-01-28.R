# ----------------------------------------------------------------------- #
# Checking on some details for the assay annotation updates
# Particularly for MEA Acute
# Jan 28, 2022
# ----------------------------------------------------------------------- #

library(data.table)
library(tcpl)
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')
library(stringi)
library(ggplot2)
library(RMySQL)

# Load data ---------------------------------------------------------------

# This is lovely, but better if fload from tcpl
# # MEA acute
# load('lvl0_snapshots/dat4_2020-07-29.RData')
# 
# # mea nfa
# load('../pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_extra_cols_2021-05-05.RData')
# load('../pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_sc0_extra_cols_2021-06-09.RData')
# mea_nfa_lvl0[, type := 'mc']
# mea_nfa_sc0[, type := 'sc']
# setdiff(names(mea_nfa_lvl0), names(mea_nfa_sc0))
# mea_nfa <- rbind(mea_nfa_lvl0, mea_nfa_sc0, fill = T)
# rm(mea_nfa_lvl0, mea_nfa_sc0)
# 
# # hci
# load('../pre-process_hci_for_tcpl/lvl0_snapshots/hci_mc0_2021-10-15_allcols.RData')
# load('../pre-process_hci_for_tcpl/lvl0_snapshots/hci_sc0_2021-10-15_allcols.RData')
# hci_mc0_Oct2021_allcols[, type := 'mc']
# hci_sc0_Oct2021_allcols[, type := 'sc']
# hci <- rbind(hci_mc0_Oct2021_allcols, hci_sc0_Oct2021_allcols)
# rm(hci_mc0_Oct2021_allcols, hci_sc0_Oct2021_allcols)

# Load data from tcpl
acid.tb <- tcplLoadAcid(fld = 'asid', val = c(20,31), add.fld = c('aid'))
acid.tb # looks like CDI is aid 857
acid.tb <- acid.tb[asid == 20 | aid == 867]
mc0 <- tcplLoadData(lvl = 0L, fld = 'acid', val = unique(acid.tb$acid), type = 'mc')
sc0 <- tcplLoadData(lvl = 0L, fld = 'acid', val = unique(acid.tb$acid), type = 'sc')
mc0[, type := 'mc']
sc0[, type := 'sc']
dat <- rbind(mc0, sc0, fill =T)
rm(sc0, mc0)
dat <- merge(dat, acid.tb, by = 'acid')
dat <- tcplPrepOtpt(dat)

# annotate the assay
dat[grepl('CCTE_Shafer_MEA_dev',acnm), my_anm := 'MEA_dev']
dat[grepl('CCTE_Shafer_MEA_acute',acnm), my_anm := 'MEA_acute']
dat[grepl('CCTE_Mundy_HCI_CDI',acnm), my_anm := 'CDI']
dat[is.na(my_anm),.N, by = .(acnm)]
# acnm     N
# 1: CCTE_Shafer_MEA_MFR 14298
# 2: CCTE_Shafer_MEA_LDH 14298
# 3:  CCTE_Shafer_MEA_AB 14298
dat[is.na(my_anm), my_anm := 'MEA_acute']

# add well id
dat[, well_id := paste(rowi,coli,sep=",")]

# note the acnm's/screening types I want to remove
dat[, keeping_data := 1]
dat[acnm %in% c('CCTE_Shafer_MEA_MFR','CCTE_Shafer_MEA_LDH','CCTE_Shafer_MEA_AB') & type == 'mc',
    keeping_data := 0]


# Checkout sovlents used --------------------------------------------------

# # MEA Acute
# dat4[wllt == 'n', .N, by = .(spid, treatment)]
# #     spid treatment     N
# # 1:  DMSO      DMSO 39021
# # 2: Water     Water   405
# 
# # MEA dev
# mea_nfa[wllt == 'n', .N, by = .(spid, treatment)]
# #            spid    treatment      N
# # 1:         DMSO         DMSO 135175
# # 2:        Water        Water  11623
# # 3: DMSO/Ethanol DMSO/Ethanol    261
# # 4:      Ethanol      Ethanol    261
# # 5:        Media        Media    348
# # ah, yes. I think Katie corrected the wwlt for the Media to not be n for the sc
# 
# # HCI
# hci[wllt == 'n', .N, by = .(spid)]
# # spid    N
# # 1:  DMSO 5420
# # 2: Media  648
# # AGain, I think Katie corrected the wllt to Media to be 'b'

dat[wllt == 'n', .N, by = .(my_anm, spid)]
#       my_anm         spid     N
# 1: MEA_acute         DMSO 41739
# 2: MEA_acute        Water   405
# 3:   MEA_dev         DMSO 56329
# 4:   MEA_dev DMSO/Ethanol   108
# 5:   MEA_dev      Ethanol   108
# 6:   MEA_dev        Water  4789
# 7:   MEA_dev        Media   144
# 8:       CDI         DMSO  1952

# okay, where is wllt n with media?
dat[wllt == 'n' & spid == 'Media' & my_anm == 'MEA_dev', .N, by = .(type)]
# all mc..
dat[wllt == 'n' & spid == 'Media' & my_anm == 'MEA_dev', .N, by = .(apid, srcf)]
# PFAS 2019...
# from my notes, all should be DMSO and 0.1% for this chem set
# Are these the only control wells on these plates?
check.apids <- dat[wllt == 'n' & spid == 'Media' & my_anm == 'MEA_dev', unique(apid)]
dat[apid %in% check.apids & wllt != 't', .N, by = .(wllt, spid, apid)][order(apid)]
#    wllt  spid               apid   N
# 1:    n  DMSO 20210127_MW75-5620 180
# 2:    b Media 20210127_MW75-5620 252
# 3:    n Media 20210127_MW75-5620  36
# 4:    n  DMSO 20210331_MW75-8102 180
# 5:    b Media 20210331_MW75-8102 252
# 6:    n Media 20210331_MW75-8102  36
# 7:    n  DMSO 20210331_MW75-8103 180
# 8:    b Media 20210331_MW75-8103 252
# 9:    n Media 20210331_MW75-8103  36
# 10:    n  DMSO 20210331_MW75-8104 180
# 11:    b Media 20210331_MW75-8104 252
# 12:    n Media 20210331_MW75-8104  36
dat[apid %in% check.apids & wllt != 't', .(length(unique(well_id))), by = .(wllt, spid, apid)][order(apid)]
#    wllt  spid               apid V1
# 1:    n  DMSO 20210127_MW75-5620  5
# 2:    b Media 20210127_MW75-5620  7
# 3:    n Media 20210127_MW75-5620  1
# 4:    n  DMSO 20210331_MW75-8102  5
# 5:    b Media 20210331_MW75-8102  7
# 6:    n Media 20210331_MW75-8102  1
# 7:    n  DMSO 20210331_MW75-8103  5
# 8:    b Media 20210331_MW75-8103  7
# 9:    n Media 20210331_MW75-8103  1
# 10:    n  DMSO 20210331_MW75-8104  5
# 11:    b Media 20210331_MW75-8104  7
# 12:    n Media 20210331_MW75-8104  1
# Okay, just opened up a source file - 
# one of the entire rows was Media for these plates
# So in order to keep 6 control wells instead of 5, 
# I let the Media well in column 2 be another control well
# while the rest of the columns containing media I left as wllt 'b'
# In retrospect, I would have just had 5 control wells,
# But I don't think it's a big deal



# Solvent percent max --------------------------------------------------

# I can use wllt 'n' as an indication of the dilution solvent percent max
# bc almost always they run some controls with the samve solvent as used for every treatment
dat[wllt == 'n', .N, by = .(my_anm, spid, conc)]
#       my_anm         spid     conc     N
# 1: MEA_acute         DMSO       NA  2439
# 2: MEA_acute         DMSO 0.100000   279
# 3: MEA_acute         DMSO 0.002000 27945
# 4: MEA_acute         DMSO 0.001500  5811
# 5: MEA_acute         DMSO 0.001000  5265
# 6: MEA_acute        Water 0.001000   405
# 7:   MEA_dev         DMSO 0.001000 53953
# 8:   MEA_dev DMSO/Ethanol 0.001000   108
# 9:   MEA_dev      Ethanol 0.001000   108
# 10:   MEA_dev        Water 0.001000  4789
# 11:   MEA_dev         DMSO 0.001460  2376
# 12:   MEA_dev        Media 0.000001   144
# 13:       CDI         DMSO 0.100000  1808
# 14:       CDI         DMSO 0.150000   144

# MEA Acute - I'm pretty sure that my "0.002" I meant "0.2%"
# MEA dev - again, I think 0.146% is the max
dat[wllt == 'n' & my_anm == 'MEA_dev' & conc > 0.001, .N, by = .(apid, srcf)]
# yeah, I have a memory of them using a slighlty higher DMSO % in the DNTGF dataset
# CDI - I'm pretty sure this should be 0.15%, not 15%. Yeah, that would be ridiculous
# I'm going to leave teh MEA dev dilution solvent percent max as 0.2% instead of reducing to 0.146%
# Because it coudl go up that high?



# Understanding the mea acute sc data -------------------------------------

# Where was the sc data taken from (srcf?) Could I use that as a clue as to whether this is wMFR or MFR?
dat[my_anm == 'MEA_acute' & type == 'sc', .N, by = .(srcf)]
# srcf     N
# 1: NHEERL_MEA_SS_SOURCE_RAW_DATA_150422.csv 12654

# Anohter idea: is there an acsn registered with this component?
tcplLoadAcid(fld = 'acid', val = acid.tb[acnm == 'CCTE_Shafer_MEA_MFR',acid], add.fld = 'acsn')
# acid                acnm           acsn
# 1: 1916 CCTE_Shafer_MEA_MFR NHEERL_MEA_MFR
# Ah, okay. Not really helpful

# Okay, I don't know where this source data is coming from

# wait, here's a thought:
# might this be coming from the darb results?
# Let's do some spot checks:
dat[my_anm == 'MEA_acute' & type == 'sc', unique(apid)]
dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & grepl('1007-46',apid)][order(rowi, coli), .(apid, rowi, coli, rval)]

# (If the values to match, I can see that the darb results are the wMFR, so that would give me confirmation)
darb.results.bas <- read.table('L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Screening/Experiment Date (Culture Date)/Phase I/8-12-14 (7-30-14)/darb results/TC_MW1007-46_2014730_20140812_14_0(000)_spike_countsresults.csv', 
                               sep = ",", skip = 4, header = TRUE, stringsAsFactors = F)
darb.results.trt <- read.table('L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Screening/Experiment Date (Culture Date)/Phase I/8-12-14 (7-30-14)/darb results/TC_MW1007-46_2014730_20140812_14_1(000)_spike_countsresults.csv', 
                               sep = ",", skip = 4, header = TRUE, stringsAsFactors = F)
bas.tb <- melt(darb.results.bas, id.vars = 'Well.name.', variable.name = 'well_char', value.name = 'activity_value', variable.factor = F)
setnames(bas.tb, old = 'Well.name.', new = 'acsn')
trt.tb <- melt(darb.results.trt, id.vars = 'Well.name.', variable.name = 'well_char', value.name = 'activity_value', variable.factor = F)
setnames(trt.tb, old = 'Well.name.', new = 'acsn')
test.vals <- merge(bas.tb, trt.tb, by = c('acsn','well_char'), suffixes = c('.b','.t'))
setDT(test.vals)
test.vals[, darb_rval := (activity_value.t - activity_value.b)/activity_value.b*100]
test.vals[, rowi := match(stri_sub(well_char, from = 1, to = 1), LETTERS)]
test.vals[, coli := as.numeric(sub('[A-Z]','',well_char))]

# Merge with dat from same plate to comapre rvals
comp.dat <- merge(dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & grepl('1007-46',apid)],
                  test.vals[acsn == 'Mean spike rate of active electrodes:'], by = c('rowi','coli'), all = T)
comp.dat[is.na(darb_rval)]
nrow(comp.dat) # 96.. wait, what?
nrow(bas.tb) # 96 -> but that's bc it includes 2 endpoints
dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & grepl('1007-46',apid)][, .N, by = .(apid, rowi, coli)]
# umm.. how are there 2 data points per well?
# ARe the rval's different, or is this just duplicate rows?
dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & grepl('1007-46',apid)][, .(num_unique_rval = length(unique(signif(rval,3)))), by = .(apid, rowi, coli)][, .N, by = .(num_unique_rval)]
#    num_unique_rval  N
# 1:               2 46
# 2:               1  2
# So all but 2 of the wells have 2 distinct rval's associated with them.. 
# How different are they?
ggplot(dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & grepl('1007-46',apid)],
       aes(x = well_id, y = rval))+
  geom_jitter(aes(color = wllt), height = 0, width = 0.05)
# Many of htese are quite a bit different. sad.
# Oh wait!! Ya know what - they probably just re-used a plate!!

names(dat)
dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & grepl('1007-46',apid), .N, by = .(apid, spid)]
# Yeah, each spid only appears once on this apid!! (since it's a SPS)

# OOOOKay
# So let's choose a plate that wasn't reused, since the experiment date has been scrubbed
dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR', .N, by = .(apid)][N == 48][order(apid)]
use.apid <- 'MW 1007-63'

# Get darb results fro new plates to compare
darb.results.bas <- read.table('L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Screening/Experiment Date (Culture Date)/Phase I/8-19-14 (8-6-14)/darb results/TC_MW 1007-63_20140806_20140819_13_0(000)_spike_countsresults.csv', 
                               sep = ",", skip = 4, header = TRUE, stringsAsFactors = F)
darb.results.trt <- read.table('L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Screening/Experiment Date (Culture Date)/Phase I/8-19-14 (8-6-14)/darb results/TC_MW 1007-63_20140806_20140819_13_1(000)_spike_countsresults.csv', 
                               sep = ",", skip = 4, header = TRUE, stringsAsFactors = F)
bas.tb <- melt(darb.results.bas, id.vars = 'Well.name.', variable.name = 'well_char', value.name = 'activity_value', variable.factor = F)
setnames(bas.tb, old = 'Well.name.', new = 'acsn')
trt.tb <- melt(darb.results.trt, id.vars = 'Well.name.', variable.name = 'well_char', value.name = 'activity_value', variable.factor = F)
setnames(trt.tb, old = 'Well.name.', new = 'acsn')
test.vals <- merge(bas.tb, trt.tb, by = c('acsn','well_char'), suffixes = c('.b','.t'))
setDT(test.vals)
test.vals[, darb_rval := (activity_value.t - activity_value.b)/activity_value.b*100]
test.vals[, rowi := match(stri_sub(well_char, from = 1, to = 1), LETTERS)]
test.vals[, coli := as.numeric(sub('[A-Z]','',well_char))]
test.vals[acsn == "Number of active electrodes:" , summary(activity_value.t)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000  11.000   8.312  14.000  16.000 
# The whole first quartile is at 0! the Median is significantly < 16
# So, the mean firing rate will be different than the wieghted mean firing rate (averaged over AE only)
# So if the values in dat are the mean firign rate, they will appear different than these darb results

# Merge with dat from same plate to comapre rvals
comp.dat <- merge(dat[my_anm == 'MEA_acute' & type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & apid == use.apid],
                  test.vals[acsn == 'Mean spike rate of active electrodes:'], by = c('rowi','coli'), all = T)
comp.dat[is.na(darb_rval)]
nrow(comp.dat) # 48 - yay!

ggplot(comp.dat, aes(x = darb_rval, y = rval)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0)
# Heeey!!! They look identical!
comp.dat[, max(abs(darb_rval-rval))]
# 2.273737e-13
# ayyyy!!!
# So I 'm still not 100% certain that the values came from the DARB,
# BUT, I am confident that these rvals represent the weighted mean firing rate
# not the generic mean firing rate

# Summary:
# - I forgot that plate can be reused, and since the apid is just the plate.id for the mea acute sc,
#   some of these apid will be associated with more than 48 wells
# - Based on the fact that rvals (% of baseline) derived from the DARB MFR results (which is the wMFR) are iddentical to the rval's in the mea acute sc for a plate,
#   I am confident that these rvals represent the weighted mean firing rate (and not the generic MFR).
# not the generic mean firing rate
# - I also feel more confident that these rval's are calculated as % of baseline in the same way I do for the mea acute mc!



# Check current LDH/AB methods for mea acute sc ------------------------------

sc.ldh.acid <- acid.tb[acnm == 'CCTE_Shafer_MEA_LDH', acid]
sc.ldh.aeid <- tcplLoadAeid(fld = 'acid', val = sc.ldh.acid)$aeid
tcplMthdLoad(lvl = 1L, id = sc.ldh.aeid, type = 'sc')
#    aeid mthd mthd_id ordr
# 1: 2035 none      14    1
tcplMthdLoad(lvl = 2L, id = sc.ldh.aeid, type = 'sc')
#    aeid           mthd mthd_id
# 1: 2035          bmad3       1
# 2: 2035 ow_bmad_nwells      20
# Okay, so they apply no level 1 correction/normalization,
# then calculate bmad of n wells only,
# then set the coff to 3*bmad
# So this means that the rvals must be pre-normalized

# So the raw data exists on the l drive so that i could standardize the rval's 
# with the format of the rvals in the mea acute endpoint (just blank-corrected, but not % of control)
# But i don't think that is worth it right now.

# Similar for AB?
sc.ab.acid <- acid.tb[acnm == 'CCTE_Shafer_MEA_AB', acid]
sc.ab.aeid <- tcplLoadAeid(fld = 'acid', val = sc.ab.acid)$aeid
tcplMthdLoad(lvl = 1L, id = sc.ab.aeid, type = 'sc')
#    aeid mthd mthd_id ordr
# 1: 2035 none      14    1
tcplMthdLoad(lvl = 2L, id = sc.ab.aeid, type = 'sc')
#    aeid           mthd mthd_id
# 1: 2035          bmad3       1
# 2: 2035 ow_bmad_nwells      20
# Yep, same.

# What do I do again with the acute annotated version?
my.ab.acid <- acid.tb[acnm == 'CCTE_Shafer_MEA_acute_AB', acid]
my.ab.aeid <- tcplLoadAeid(fld = 'acid', val = my.ab.acid)$aeid
tcplMthdLoad(lvl = 2L, id = my.ab.aeid, type = 'mc')
# none
tcplMthdLoad(lvl = 3L, id = my.ab.aeid, type = 'mc')
#    aeid                       mthd mthd_id ordr
# 1: 2541 bval.apid.nwllslowconc.med      17    1
# 2: 2541                  pval.zero      32    2
# 3: 2541                    resp.pc       5    3
# classic, percent of control
# So once again, the rvals under 'CCTE_Shafer_MEA_acute_AB' are truly raw values,
# while the values rvals under 'CCTE_Shafer_MEA_AB' must have been pre-normalized
tcplMthdLoad(lvl = 5L, id = my.ab.aeid, type = 'mc')
# okay, and I do use bmad3 as well for the coff


# reviewing dev LDH, AB methods
nfa.aeid.tb <- tcplLoadAeid(fld = 'asid', val = 20)
nfa.aeid.tb <- nfa.aeid.tb[grepl('_dev_',aenm)]
tcplMthdLoad(lvl = 3L, id = nfa.aeid.tb[grepl('LDH',aenm),aeid], type = 'mc')
#    aeid                mthd mthd_id ordr
# 1: 2529 bval.apid.nwlls.med      11    1
# 2: 2529           pval.zero      32    2
# 3: 2529             resp.pc       5    3
# pval is 0
tcplMthdLoad(lvl = 3L, id = nfa.aeid.tb[grepl('AB',aenm),aeid], type = 'mc')
# aeid                mthd mthd_id ordr
# 1: 2530 bval.apid.nwlls.med      11    1
# 2: 2530           pval.zero      32    2
# 3: 2530             resp.pc       5    3



# What are the current acute sc MFR methods? ------------------------------

sc.mfr.acid <- acid.tb[acnm == 'CCTE_Shafer_MEA_MFR', acid]
sc.mfr.aeid <- tcplLoadAeid(fld = 'acid', val = sc.mfr.acid)$aeid
tcplMthdLoad(lvl = 1L, id = sc.mfr.aeid, type = 'sc')
#    aeid          mthd mthd_id ordr
# 1: 2033          none      14    1
# 2: 2034          none      14    1
# 3: 2034 resp.multneg1      16    2
tcplMthdLoad(lvl = 2L, id = sc.mfr.aeid, type = 'sc')
#    aeid           mthd mthd_id
# 1: 2034          bmad3       1
# 2: 2033 ow_bmad_nwells      20
# 3: 2034 ow_bmad_nwells      20
# 4: 2033          bmad1      22
# Ah, okay!!

# okay things are coming back to me;
# I remember arguing that useing 3bmad in teh dn direction and 1bmad in the up
# was essentially the same as 2bmad in both directions if we were to normalize by subtracting 1bmad?
# yeah...
# So this is really quite different than how I am proposing that we normalize the data
# (in fact, I think what is done for the SC is quite fine, perhaps better bc it doesn't factor in the bval!
# just the bmad of ALL DMSO controls!
# So I"m not going to propose that we 





# Signal direction? -------------------------------------------------------

aeid.tb <- tcplLoadAeid(fld = 'acid', val = acid.tb$acid, add.fld = 'aenm')
aeid.tb[grepl('LDH',aenm)]
#    acid aeid                         aenm
# 1: 1917 2035          CCTE_Shafer_MEA_LDH
# 2: 2488 2529   CCTE_Shafer_MEA_dev_LDH_dn
# 3: 2495 2540 CCTE_Shafer_MEA_acute_LDH_up
# Okay, so in the acute, it is up ->
# makes sense, because we are looking for an increase in extracellular LDH
# This would apply to aeid 2035 as well, bc it is associated with teh acute assay

aeid.tb[grepl('AB',aenm)]
#    acid aeid                        aenm
# 1: 1918 2036          CCTE_Shafer_MEA_AB
# 2: 2489 2530   CCTE_Shafer_MEA_dev_AB_dn
# 3: 2496 2541 CCTE_Shafer_MEA_acute_AB_up
# really, it's up for acute??

# Let's check out some rvals
ggplot(dat[acnm == 'CCTE_Shafer_MEA_acute_AB' & wllq == 1], aes(x = conc, y = rval))+
  geom_jitter(height = 0, width = 0.1)


# Understanding analysis fitting direction --------------------------------

# Isn't everythign in tcpl supposed to be fitted in the positive direction?
# Then why some MFR dn says negative direction?

all.aeid.tb <- tcplLoadAeid(add.fld = c('analysis_direction','assay_component_endpoint_desc','signal_direction','assay_function_type'))
all.aeid.tb[, .N, by = .(analysis_direction, signal_direction)][order(analysis_direction)]
# analysis_direction    N
# 1:           positive 1695
# 2:           negative  880
# 3:               <NA>  248
# 4:                 NA    2
# okay, several are "negative"!

# common langauge in desc, relative to anything other than baseline?
all.aeid.tb[, .N, by = grepl('relative',assay_component_endpoint_desc)]
all.aeid.tb[, .N, by = .(grepl('relative',assay_component_endpoint_desc), grepl('baseline of activity',assay_component_endpoint_desc))]
View(all.aeid.tb[grepl('relative',assay_component_endpoint_desc) & grepl('baseline of activity',assay_component_endpoint_desc)])

# viability reporter more common for cell viability assay?
all.aeid.tb[, .N, by = .(assay_function_type)]
all.aeid.tb[, reporter_type := stri_extract(assay_component_endpoint_desc, regex = 'Using a type of [^ ]* reporter')]
all.aeid.tb[assay_function_type %in% c('viability','cell viability','cytotoxicity'), .N, by = .(reporter_type)]
# reporter_type   N
# 1:  Using a type of viability reporter  77
# 2:                                <NA> 127
# 3:     Using a type of growth reporter   6
# 4: Using a type of functional reporter   2
all.aeid.tb[assay_function_type %in% c('viability','cell viability','cytotoxicity') & reporter_type == 'Using a type of functional reporter']
# yep, this is just the dev assays!
# where the reporter type is defined all elsewhere, the say growth or viability
# that makes mroe sense to me



# Key positive control ----------------------------------------------------

# check it out for ldh, AB
all.aeid.tb <- tcplLoadAeid(add.fld = c('assay_function_type','key_positive_control'))
all.aeid.tb[assay_function_type %in% c('viability','cell viability','cytotoxicity'), .N, by = .(key_positive_control)]
# "Lysed cells" is oen option
# generally these look like actual test chemicals, e.g. colchicine, cadmium chloride
# not assay components, like pure LDH
# From tcpl sop: "tested chemical sample expected to produce activity; used to assess assay validity"
# Okay, so this should be something with wllt == 't' preferably
# I could maybe say something like methylmercurcy, but we would need to discuss that
# So I'm leaving it NA for AB and LDH

# well, let's look at some options here;
dat[keeping_data == 1 & type == 'mc' & my_anm == 'MEA_acute' & wllt != 't' & grepl('LDH',acnm), .N, by = .(spid, chnm, wllt)]
# spid chnm wllt   N
# 1:  Bicuculline <NA>    z 414
# 2:         DMSO <NA>    n 870
# 3:        Media <NA>    b  60
# 4:   Picrotoxin <NA>    z  96
# 5: Tetrodotoxin <NA>    x  96
# 6:   Tritonx100 <NA>    p 474
# 7:   Tritonx100 <NA>    x 318
# 8:        Water <NA>    n   9
# So I could use tritonx100...?
# it is on every plate, ya?- > actually, that doesn't matter for this question
# I think this is a good eough answer

# Now for AB
dat[keeping_data == 1 & type == 'mc' & my_anm == 'MEA_acute' & wllt != 't' & grepl('AB',acnm), .N, by = .(spid, chnm, wllt)]
# even though tritonx100 is wllt p, because it totally will kill the cells
# I feel weird making this a positive control bc it's not really part of the assay
# It's just there bc ran in tandom with LDH
# so I"m goign to lreave this as NA

# What about for the sc?
dat[keeping_data == 1 & type == 'sc' & my_anm == 'MEA_acute' & wllt != 't' & grepl('LDH',acnm), .N, by = .(spid, chnm, wllt)]
#          spid chnm wllt   N
# 1: BICUCULLINE <NA>    p 186
# 2:        DMSO <NA>    n 279
# 3:       LYSIS <NA>    v  93
# I'm prettttty sure that the LYSIS = tritonx100...
# I'm going to say tritonx100, because they wrote out bicuculline methoidide even thogh that conflicts with the spid as it appears here
dat[keeping_data == 1 & type == 'sc' & my_anm == 'MEA_acute' & wllt != 't' & grepl('AB',acnm), .N, by = .(spid, chnm, wllt)]
# spid chnm wllt   N
# 1: BICUCULLINE <NA>    p 186
# 2:        DMSO <NA>    n 279
# 3:       LYSIS <NA>    v  93
# for same reason as with the mc, I'm going to leave this NA



# For MEA acute endpoints
# sc - is BIC legitimately a good positive control?
dat[type == 'sc' & my_anm == 'MEA_acute' & grepl('bic',tolower(chnm)), .N, by = .(spid, chnm, wllt)]
# spid                                                      chnm wllt N
# 1: TP0001187F11                          Octylbicycloheptenedicarboximide    t 9
# 2: TP0001239G06 4-Bromophenyl 1,4-diazabicyclo(3.2.2)nonane-4-carboxylate    t 9
# huh?
dat[type == 'sc' & my_anm == 'MEA_acute' & grepl('bic',tolower(spid)), .N, by = .(spid, chnm, wllt)]
# spid chnm wllt   N
# 1: BICUCULLINE <NA>    p 558
# ah, I see
# I'm going to leave this - because if we ever test/tested BIC, that would be the intended positive control
# (well, at least in the up direction, yeah? Or was it down?)
dat[type == 'sc' & acnm == 'CCTE_Shafer_MEA_MFR' & wllt == 'p', unique(spid)] # jsut bic
plotdat <- dat[acnm == 'CCTE_Shafer_MEA_MFR']
ggplot(plotdat[wllq == 1], aes(x = wllt, y = rval)) +
  geom_boxplot()
# Okay, so BIC is definitely up relative to DMSO
# but it's right on par with the b wells!
# But if it's one or the other, i'd say it's up
# So I"m going to remove this as a positive control for down
# I don't think TTX or other dn positive control was tested in the sc...
dat[type == 'sc' & my_anm == 'MEA_acute' & (tolower(chnm) %in% c('tetrodotoxin','ttx') | tolower(spid) %in% c('tetrodotoxin','ttx') ), .N, by = .(spid, chnm, wllt)]
# empty


# MC
dat[type == 'mc' & my_anm == 'MEA_acute' & wllt != 't' & !grepl('(LDH)|(AB)',acnm), .N, by = .(wllt, spid)]
#    wllt         spid     N
# 1:    b                  3
# 2:    p          BIC   417
# 3:    p  Bicuculline 17802
# 4:    n         DMSO 37908
# 5:    v        LYSIS    84
# 6:    v        Lysis   129
# 7:    b        Media  3870
# 8:    p   Picrotoxin  3569
# 9:    p Tetrodotoxin  3569
# 10:    v   Tritonx100 10793
# 11:    n        Water   387
# For the up direction, which is more common - PICRO or BIC?
dat[type == 'mc' & my_anm == 'MEA_acute' & wllt != 't' & !grepl('(LDH)|(AB)',acnm) & spid %in% c('BIC','Bicuculline','Picrotoxin'), .(length(unique(apid))), by = .(spid)]
#           spid  V1
# 1:         BIC 169
# 2: Bicuculline  69
# 3:  Picrotoxin  28
# REally?? I expected it to be Picro
# I guess Picro is newer?
# I'm going to say Picro regardless, because going forward I believe they plan ot use picro, bc it's better/more consistent
# But will check with the group
# Oh wait, why not both?

# dn options
# TTX is the clear option



# intended target family --------------------------------------------------

# Is cell function a thing?
all.aeid.tb <- tcplLoadAeid(add.fld = c('assay_function_type','intended_target_family','cell_viability_assay'))
all.aeid.tb[grepl('function',intended_target_family)]
# cardiac function...
all.aeid.tb[, sort(unique(intended_target_family))]
# so cell function is def not an option..
# they def need to do a lot of work to really make these annotations consistent..
# oofta
# could so neuronal function ?
# or neuron function?
# eh, we're not measure directly are these cells functioning propertly (well, that's the goal)
# But generally we just looking at their activity

# cyto assays
all.aeid.tb[assay_function_type %in% c('viability','cell viability','cytotoxicity'), .N, by = .(intended_target_family)]
# intended_target_family   N
# 1:             cell cycle 205
# 2:                   <NA>   2
# 3:     cell proliferation   1
# 4:               protease   4
# okay, most are lumped under cell cycle!
# so let's go with that


# Understand cell viability
# my intiution contradicts the defn in tcpl sop
# is cell_viability_Assay = 1 when the assay itself measure cell viability,
# or if the endpoint might be confounded by cell viability?
all.aeid.tb[cell_viability_assay == 1, .N, by = .(assay_function_type)]
# assay_function_type   N
# 1:           viability 150
# 2:      cell viability   2
# Okay, so everywhere that the cell_viability_Assay == 1,
# it seems like the assay funciton itself is to measure cell viability
# So I think this col matches option #1



# Create plots to convince tim that acute ab is down ----------------------

plotdat <- dat[keeping_data == 1 & wllq == 1 & my_anm == 'MEA_acute' & type == 'mc' & grepl('AB',acnm) & wllt != 't']
ggplot(plotdat, aes(x = spid, y = rval))+
  geom_boxplot()
#  oaky, yes this shows that tritonx100 is low,
# but I feel like the other trends we are seeing might be distracting

plotdat <- dat[keeping_data == 1 & wllq == 1 & my_anm == 'MEA_acute' & type == 'mc' & grepl('AB',acnm) & wllt == 't']
plotdat[, conc_round := signif(conc, 1)]
ggplot(plotdat, aes(x = conc_round, y = rval))+
  geom_boxplot(aes(group = conc_round))+
  scale_x_log10()
# okay, not convincing either...

# I think I'll stick with the summary figures if he asks (bc at least these broken up for epochs)
# so factors in some varbility that might be making the TTX, picro look low

# Now for the sc... the data is pre-normalized...
plotdat <- dat[keeping_data == 1 & wllq == 1 & my_anm == 'MEA_acute' & type == 'sc' & grepl('AB',acnm) & wllt == 't']
plotdat[, conc_round := signif(conc, 1)]
ggplot(plotdat, aes(x = conc_round, y = rval))+
  geom_boxplot(aes(group = conc_round))+
  scale_x_log10()
# wait a sec... does this make any sense?
# Now I'm starting to question whether this was pre-normalized ocrrectly
# because the rvals seem to be decreasing at higher conc's...
# and I would assume that if there is any trend, it should be toward cytotoxicity!

# Are any chem a hit with this endpiont?
# Does it make any sense?
sc2 <- tcplLoadData(lvl = 2L, fld = 'aeid', val = sc.ab.aeid, type = 'sc')
sc2 <- tcplPrepOtpt(sc2)
sc2[, .N, by = .(hitc)]
sc2[hitc == 1]
# LYSIS is a hit, that's a good sign
sc2[, summary(max_med)]
sc2[hitc == 1]
# okay, this includes things like mercuric chloride, tributyltin chloride

# Oh , wait, DUH! We're dealing with single conc, so higher conc's are completely diff compounds!
# I'm just going to trust that whoever did the normalization before did it correctly
# and that the assay format has not changed (i.e., we still lookign for a decrease in the response)



# analysis direction and signal direction ---------------------------------
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'prod_internal_invitrodb_v3_4')
all.aeid.tb <- tcplLoadAeid(add.fld = c('assay_function_type','analysis_direction','signal_direction','acid','acnm'))
all.aeid.tb[, .N, by = .(analysis_direction, signal_direction)][order(analysis_direction)]
#    analysis_direction signal_direction   N
# 1:                 NA               NA   1
# 2:           negative             loss 509
# 3:           negative             gain 157
# 4:           positive             gain 775
# 5:           positive             loss 614
# wow, so these are definitely not the same!

# Let's try to understand some cases
res3 <- tcplMthdLoad(lvl = 3L, id = all.aeid.tb[analysis_direction == 'positive' & signal_direction == 'loss',aeid][1:10], type = 'mc')
res3[, .(unique(mthd)), by = .(aeid)]
res3 <- lapply(all.aeid.tb[analysis_direction == 'positive' & signal_direction == 'loss',aeid],
               function(i) tcplMthdLoad(lvl = 3L, id = i, type = 'mc')[order(ordr),mthd])
length(res3) # 614
length(unique(res3)) # 11
res3[[1]]
# resp.blineshift.50.repi - does some kind of vertical zero centering. resp := resp - low_med
unique(res3)[[2]]
# "none"          "resp.multneg1"
unique(res3)
# hmm, this looks like a fairly typical hodgepodge of methods.


# okay - are there cases where the 2 endpoints for a given component are in oppositie directions?
# (i.e., what i want to do)
all.aeid.tb[, dir := stri_extract(aenm, regex = '[^_]*$')]
all.aeid.tb[, .N, by = .(dir)]
all.aeid.tb[, dir2 := dir]
all.aeid.tb[dir2 %in% c('dn','down','loss'), dir2 := 'down']
all.aeid.tb[dir2 %in% c('up','gain'), dir2 := 'up']
all.aeid.tb[, .N, by = .(dir2)][order(-N)][1:20]
check.acids <- all.aeid.tb[, .(length(unique(aeid))), by = .(acid)][V1 == 2, acid]
all.aeid.tb[acid %in% check.acids, .N, by = .(dir2)][order(-N)][1:20]
View(all.aeid.tb[acid %in% check.acids][order(acid)])
all.aeid.tb[acid %in% check.acids, .N, by = .(dir2, signal_direction, analysis_direction)][order(-N)]
#         dir2 signal_direction analysis_direction   N
# 1:        up             gain           positive 516
# 2:      down             loss           negative 499
# 3: Activator             gain           negative 155
# 4:      down             loss           positive  18
# plus other 1-off cases...
# oh ,insight!!
# th eonly/main case where signal direction is gain and analysis direciton is negative is for "Activator" endpoints
# otherwise, where the signal direction is gain, the endpoint suffix is "up" and analysis direciton is positive

# Here's an example:
all.aeid.tb[dir2 == 'Activator']
#    aeid                        aenm assay_function_type analysis_direction signal_direction acid              acnm       dir      dir2
# 1:  319           NVS_ADME_hCYP19A1  enzymatic activity           positive             loss  201 NVS_ADME_hCYP19A1  hCYP19A1  hCYP19A1
# 2:  320 NVS_ADME_hCYP19A1_Activator  enzymatic activity           negative             gain  201 NVS_ADME_hCYP19A1 Activator Activator
# reading more about this endpoint... I'm still so confused
# I'm going to go with what makes sense to me,
# KNow that there is a lot of standardization needed for inivtrodb in the future



# assay design type for LDH and AB ----------------------------------------
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')
all.acid.tb <- tcplLoadAcid(add.fld = c('assay_design_type','assay_design_type_sub','detection_technology','acnm','key_assay_reagent','key_assay_reagent_type'))
all.acid.tb[grepl('LDH',acnm)]
# acid                         acnm  assay_design_type                assay_design_type_sub detection_technology key_assay_reagent key_assay_reagent_type
# 1:  560          OT_HEK293T_LDH_2880 viability reporter dehydrogenase activity determination      Enzyme activity  tetrazolium salt              substrate
# 2:  561       OT_Hepatocyte_LDH_2880 viability reporter dehydrogenase activity determination      Enzyme activity  tetrazolium salt              substrate
# 3:  722 LTEA_HepaRG_LDH_cytotoxicity viability reporter dehydrogenase activity determination      Enzyme activity              <NA>              substrate
# 4: 1917          CCTE_Shafer_MEA_LDH viability reporter dehydrogenase activity determination      Enzyme activity              <NA>              substrate
# 5: 2488      CCTE_Shafer_MEA_dev_LDH viability reporter       lactate dehydrogenase activity      enzyme activity                NA              substrate
# 6: 2495    CCTE_Shafer_MEA_acute_LDH               <NA>                                 <NA>                 <NA>              <NA>                   <NA>
all.acid.tb[grepl('_AB_',acnm) | grepl('_AB$',acnm)]
# just the MEAs, what i already have

tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')
all.aeid.tb <- tcplLoadAeid(add.fld = c('assay_function_type','assay_component_endpoint_desc','signal_direction','acid','acnm'))
View(all.aeid.tb[grepl('LDH',aenm)])
# well that's not helpful



