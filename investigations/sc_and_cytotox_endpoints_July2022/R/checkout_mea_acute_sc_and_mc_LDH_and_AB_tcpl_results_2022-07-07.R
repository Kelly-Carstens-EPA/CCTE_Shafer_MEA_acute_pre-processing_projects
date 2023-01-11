# ---
# title: "MEA Acute SC TCPL Evaluation of LDH and AB results"
# author: "Amy Carpenter"
# date: "July 7, 2022"
# output: 
#   html_document:
#     code_folding: hide
#     toc: true
#     toc_float: true
#     df_print: paged
# ---

#+ include = FALSE
knitr::opts_chunk$set(out.width = '100%')

library(data.table)
library(openxlsx)
library(stringi)
library(ggplot2)

## # Purpose ----

## To confirm that the current MEA Acute TCPL sc results are as expected for the LDH and AB endpoints<br>
## Motivation: I noticed that Acetaminophen is a hit in a cyto assay... that didn't seem right. So we shall investigate.<br>
## From Strickland et al, 2018: "Further, changes in MFR occurred in the absence of cytotoxicity, as only 8 compounds decreased cell viability"<br>
## from results section: "Compounds were deemed cytotoxic if they induced greater than 20% increase in LDH release and/or decreased mitochondrial activity by more than 80%"
##
# knitr::spin('investigations/sc_and_cytotox_endpoints_July2022/R/checkout_mea_acute_sc_and_mc_LDH_and_AB_tcpl_results_2022-07-07.R', precious  = T, doc = '^##\\s*')

## # Prepare data ----

# Load tcpl results
# load('../../mea_nfa_vs_acute/data/mea_acute_and_dev_sc2_2022-02-10.RData')
# sc2 <- sc2[assay_type == 'acute']
load('investigations/sc_and_cytotox_endpoints_July2022/data/sc0_and_sc2_invitrodb_05042022_2022-08-03.RData')
##

## # Check out activity of LDH/AB ----

tcpl.cyto.res <- sc2[, .(LDH_hitc = hitc[grepl('LDH',aenm)],
                         AB_hitc = hitc[grepl('AB',aenm)]), by = .(dsstox_substance_id, chnm, casn)]
tcpl.cyto.res[, .N, by = .(LDH_hitc, AB_hitc)][order(LDH_hitc, AB_hitc)]
#    LDH_hitc AB_hitc   N
# 1:        0       0 870
# 2:        0       1  40
# 3:        1       0 175
# 4:        1       1  19
## Wait a sec... There are 175 hits in the LDH??

## What were the cutoffs?
sc2[grepl('(LDH)|(AB)',aenm), .N, by = .(aenm, bmad, coff)]
#                   aenm     bmad      coff    N
# 1: CCTE_Shafer_MEA_LDH 2.547671  7.643012 1104
# 2:  CCTE_Shafer_MEA_AB 6.666138 19.998414 1104
## Looks like a 3*bmad cutoff was used<br>
## No wonder there are 175 hits for the LDH - the cutoff is SO low!!

## 
## From results section, Jenna used a 20% cutoff for the LDH (in the up direciton) and 20% for AB (in the dn direction)<br>
## It doesn't look like she published those results, but what if I applied those hit calls?<br>
sc2[grepl('LDH',aenm), strickland_equiv_coff := 20]
sc2[grepl('AB',aenm), strickland_equiv_coff := 80]
sc2[, strickland_equiv_hitc := as.numeric(max_med >= strickland_equiv_coff)]

## But still, results say that only 8 compounds were a hit
## (compound names that had a cyto hit copied from manuscript text, I don't see where else the cyto results were published)
strickland.cyto.chnms <- c('UK-337312', 'Tributyltin chloride', 'Tributyltin methacrylate', 'Phenylmercuric acetate',
                           '9-Phenanthrol', 'Gentian Violet', 'Mercuric chloride', 'Ketoconazole')
setdiff(strickland.cyto.chnms,
        sc2$chnm)
# all present, cool
sc2[, strickland_cyto_hit := as.numeric(chnm %in% strickland.cyto.chnms)]

## Check out updated LDH results
sc2[grepl('LDH',aenm), .(num_samples = .N), by = .(aenm, bmad, coff, hitc, strickland_equiv_coff, strickland_equiv_hitc, strickland_cyto_hit)][order(hitc, strickland_equiv_hitc)]
## So there are 184 samples that are currently a hit for LDH, but probably shoudn't be<br>
## Note that strickland cyto hit could have been for LDH or AB (paper did not say)

## And updated AB results 
sc2[grepl('AB',aenm), .(num_samples = .N), by = .(aenm, bmad, coff, hitc, strickland_equiv_coff, strickland_equiv_hitc, strickland_cyto_hit)][order(hitc, strickland_equiv_hitc)]
## And at least 50 AB hits currently that probably should not be a hit

## Visualize hit call situation
sc2[, hit_desc := paste0('tcpl:',hitc,', strickland:',strickland_cyto_hit)]
sc2$hit_desc <- factor(sc2$hit_desc)
plotdat <- sc2[grepl('(LDH)|(AB)',aenm)]
plotdat[, plot_name := sub('CCTE_Shafer_MEA_','',aenm)]
ggplot(plotdat, aes(x = plot_name, y = max_med))+
  geom_jitter(aes(color =hit_desc), width = 0.1, height = 0)+
  geom_jitter(aes(y = coff), col = 'black', width = 0.2, height = 0.25, pch = '-', size = 2)+
  geom_jitter(aes(y = strickland_equiv_coff), col = 'red', width = 0.2, height = 0.25, pch = '-', size = 2)+
  scale_color_discrete(name = 'current TCPL\nhit call')+
  ylab('median response')+
  xlab('assay endpoint')+
  # scale_x_discrete(expand = expand_scale(add = c(0.5,1)))+
  # annotate(geom = 'text', x = 2.5, y = 23, label = 'Strickland2018 cutoff')+ # I don't think its 20% for both anymore
  # geom_hline(yintercept = 20)+
  # annotate(geom = 'text', x = 2.5, y = 33, label = '30% cutoff')+
  # geom_hline(yintercept = 30)+
  ggtitle('Current Hit Calls for MEA Acute SC Cytotoxicity\nblack line = current cutoff (3xBMAD), red line = strickland 2018 cutoff\n(each point = 1 sample)')+
  theme_bw()

# ggplot(sc2[grepl('(LDH)',aenm)], aes(x = max_med))+
#   geom_histogram()+
#   geom_vline(aes(xintercept = coff), col = 'orange')+
#   geom_vline(aes(xintercept = strickland_equiv_coff))+
#   ggtitle('Current Hit Calls for MEA Acute SC ')


## Compounds that are currently a cyto hit in tcpl, but not in strickland 2018
sc2[grepl('(LDH)|(AB)',aenm) & hitc == 1 & strickland_cyto_hit == 0 & !is.na(chnm), .(aenm = sub('CCTE_Shafer_MEA_','',aenm), 
                                                                                      chnm = stri_sub(chnm,1,30), max_med, coff, hitc, strickland_equiv_coff, strickland_equiv_hitc, strickland_cyto_hit)][order(hitc, strickland_equiv_hitc)][order(aenm, chnm)]

## **I'm still slightly confused on where these values came from… (I'm just guessing 80% corresponds to 80% decrease in viability, but I don't actually know how these values were normalized)<br>
## There are no normalization method in TCPL<br>
## I haven't been able to find the srcf (NHEERL_MEA_SS_SOURCE_RAW_DATA_150422.csv), so I haven't been able to do much forensics<br>
## But I think reasonable to assume 80% is applicable to this data?<br>
##

## # Confirming normalization direction of AB and LDH values ----

## I don't know how these respone values were normalizes/what they represent<br>
## How were these values normalized in tcpl?
# NOT RUN. Just showing output as of JUly 7, 2022
# library(tcpl)
# tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb_05042022')
# 
# shafer.aeids <- tcplLoadAeid(fld = 'asid', val = 20, add.fld = c('export_ready','internal_ready'))
# sc.cyto.aeids <- shafer.aeids[grepl('MEA_LDH',aenm) | grepl('MEA_AB',aenm), aeid]
# tcplMthdLoad(lvl = 1L, id = sc.cyto.aeids, type = 'sc')
# none
# tcplMthdLoad(lvl = 2L, id = sc.cyto.aeids, type = 'sc')
# aeid           mthd mthd_id
# 1: 2035          bmad3       1
# 2: 2036          bmad3       1
# 3: 2035 ow_bmad_nwells      20
# 4: 2036 ow_bmad_nwells      20

# sc.acid.tb <- tcplLoadAeid(fld = 'aeid', val = sc.cyto.aeids, add.fld = c('acnm','acid'))
# sc0 <- tcplPrepOtpt(tcplLoadData(lvl = 0L, fld = 'acid', type = 'sc', val = sc.acid.tb$acid))
# sc0[, .N, by = .(srcf)]
# srcf    N
# 1: NHEERL_MEA_SS_SOURCE_RAW_DATA_150422.csv 8436

## What directino does it look like the values have been normalized?
ggplot(sc2[grepl('AB',aenm)], aes(x = max_med)) +
  geom_histogram(aes(fill = as.factor(hitc)), binwidth = 5)+
  xlab('median response')+
  ylab('# of samples')+
  scale_fill_manual(values = c('1' = 'orange',
                                '0' = 'black'),
                     name = 'current TCPL\nhit call')+
  geom_vline(xintercept = sc2[grepl('AB',aenm), unique(coff)], col = 'red', lwd = 2)+
  geom_vline(xintercept = sc2[spid == 'DMSO' & grepl('AB',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[spid == 'LYSIS' & grepl('AB',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[chnm == 'Phenylmercuric acetate' & grepl('AB',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[chnm == 'Acetaminophen' & grepl('AB',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[chnm == 'Sodium saccharin hydrate' & grepl('AB',aenm), max_med], col = 'gray50')+
  annotate(geom = 'label', y = 200, x = sc2[spid == 'DMSO' & grepl('AB',aenm), max_med], label = 'DMSO', hjust = 1)+
  annotate(geom = 'label', y = 200, x = sc2[spid == 'LYSIS' & grepl('AB',aenm), max_med], label = 'LYSIS', hjust = 1)+
  annotate(geom = 'label', y = 250, x = sc2[chnm == 'Phenylmercuric acetate' & grepl('AB',aenm), max_med], label = 'Phenylmercuric\nacetate', hjust = 1)+
  annotate(geom = 'label', y = 250, x = sc2[chnm == 'Acetaminophen' & grepl('AB',aenm), max_med], label = 'Acetaminophen', hjust = 1)+
  annotate(geom = 'label', y = 225, x = sc2[chnm == 'Sodium saccharin hydrate' & grepl('AB',aenm), max_med], label = 'Sodium saccharin hydrate', hjust = 1)+
  ggtitle('Distribution of AB sc median response values\nred line = current cutoff (3xBMAD)')+
  theme_bw()

## So it really looks like the values are currently normalized such that a larger value corresponds to a greater decrease in viability
## 


ggplot(sc2[grepl('LDH',aenm)], aes(x = max_med)) +
  geom_histogram(aes(fill = as.factor(hitc)))+
  xlab('median response')+
  ylab('# of samples')+
  scale_fill_manual(values = c('1' = 'orange',
                               '0' = 'black'),
                    name = 'current TCPL\nhit call')+
  geom_vline(xintercept = sc2[grepl('LDH',aenm), unique(coff)], col = 'red', lwd = 2)+
  geom_vline(xintercept = sc2[spid == 'DMSO' & grepl('LDH',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[spid == 'LYSIS' & grepl('LDH',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[chnm == 'Phenylmercuric acetate' & grepl('LDH',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[chnm == 'Acetaminophen' & grepl('LDH',aenm), max_med], col = 'gray50')+
  geom_vline(xintercept = sc2[chnm == 'Sodium saccharin hydrate' & grepl('LDH',aenm), max_med], col = 'gray50')+
  annotate(geom = 'label', y = 200+500, x = sc2[spid == 'DMSO' & grepl('LDH',aenm), max_med], label = 'DMSO', hjust = 1)+
  annotate(geom = 'label', y = 200+500, x = sc2[spid == 'LYSIS' & grepl('LDH',aenm), max_med], label = 'LYSIS', hjust = 1)+
  annotate(geom = 'label', y = 250+500, x = sc2[chnm == 'Phenylmercuric acetate' & grepl('LDH',aenm), max_med], label = 'Phenylmercuric\nacetate', hjust = 1)+
  # annotate(geom = 'label', y = 250+500, x = sc2[chnm == 'Acetaminophen' & grepl('LDH',aenm), max_med], label = 'Acetaminophen', hjust = 1)+
  # annotate(geom = 'label', y = 225+500, x = sc2[chnm == 'Sodium saccharin hydrate' & grepl('LDH',aenm), max_med], label = 'Sodium saccharin hydrate', hjust = 1)+
  ggtitle('Distribution of LDH sc median response values\nred line = current cutoff (3xBMAD)')+
  theme_bw()

## Also looks like larger values = greater response
##

## # Confirm current mc LDH/AB methods ----

# mc.cyto.aeids <- shafer.aeids[grepl('MEA_acute_LDH',aenm) | grepl('MEA_acute_AB',aenm), aeid]
# asid aeid                         aenm export_ready internal_ready
# 1:   20 2540 CCTE_Shafer_MEA_acute_LDH_up            1              1
# 2:   20 2541  CCTE_Shafer_MEA_acute_AB_dn            1              1

## All Current mc methods
# NOT RUN. Just showing output as of JUly 7, 2022
# tcplMthdLoad(lvl = 3L, id = mc.cyto.aeids, type = 'mc')
# aeid                       mthd mthd_id ordr
# 1: 2540 bval.apid.nwllslowconc.med      17    1
# 2: 2540        pval.apid.pwlls.med      13    2
# 3: 2540                    resp.pc       5    3
# 4: 2541 bval.apid.nwllslowconc.med      17    1
# 5: 2541                  pval.zero      32    2
# 6: 2541                    resp.pc       5    3
# tcplMthdLoad(lvl = 4L, id = mc.cyto.aeids, type = 'mc')
# bmad lowconcnwells for both
# tcplMthdLoad(lvl = 5L, id = mc.cyto.aeids, type = 'mc')
# aeid  mthd mthd_id
# 1: 2540 bmad3       1
# 2: 2541 bmad3       1
# Woah, 3bmad!!

# What do those cutoff equate to?
load(file.path('investigations','data','cyto_mc5_mc6_invitrodb_2022-02-10.RData'))
# mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = 'aeid', val = mc.cyto.aeids, type = 'mc'))
mc.cyto[, .N, by = .(aenm, aeid, bmad, coff)]
#                            aenm aeid      bmad      coff   N
# 1: CCTE_Shafer_MEA_acute_LDH_up 2540  1.768788  5.306365 527
# 2:  CCTE_Shafer_MEA_acute_AB_dn 2541 13.375532 40.126595 521
# Woah, even the current LDH cutoffs are quite low!

## Current hit counts in LDH adn AB
mc.cyto[grepl("LDH",aenm), .N, by = .(aenm, hitc)]
mc.cyto[grepl("AB",aenm), .N, by = .(aenm, hitc)]
# only 18 and 21 hits - that's not htat many

plotdat <- sc2[grepl('(LDH)|(AB)',aenm)]
mc.cyto[, plot_name := sub('CCTE_Shafer_MEA_','',aenm)]
ggplot(mc.cyto[!is.na(dsstox_substance_id) & hitc %in% c(0,1)], aes(x = plot_name, y = max_med))+
  geom_jitter(aes(color =as.factor(hitc)), width = 0.1, height = 0)+
  geom_jitter(aes(y = coff), col = 'red', width = 0.2, height = 0.25, pch = '-')+
  scale_color_manual(values = c('1' = 'orange',
                                '0' = 'black'),
                     name = 'current TCPL\nhit call')+
  ylab('median response')+
  xlab('assay endpoint')+
  scale_x_discrete(expand = expand_scale(add = c(0.5,1)))+
  annotate(geom = 'text', x = 2.5, y = 23, label = '20% cutoff')+
  geom_hline(yintercept = 20)+
  annotate(geom = 'text', x = 2.5, y = 83, label = '80% cutoff')+
  geom_hline(yintercept = 80)+
  ggtitle('Current Hit Calls for MEA Acute MC Cytotoxicity\nred line = current cutoff (3xBMAD)\n(each point = 1 sample)')

## So i guess with the factor of needing to fit a curve combined with overall lower response values,
## it looks like the current low cutoffs based on 3*bmad aren't terrible for the MC.
##
## Review meaning of values for the cyto assays, as I prepare d the values for the MC:
##
## LDH_up: raw blank-corrected values that correspond to the amount of LDH released (from dead cells). 
## At level 3, the values are normalized to the LDH released from a well in which all the cells have been lysed.
## So larger resp values indicate more LDH released because there were more dead cells in the well.
## So a cutoff of 20% roughly means that the chemical was 20% as cytotoxic as something that totally destroys cells relative to DMSO.


##
## AB_dn: Raw values corresponds to the amount of mitochondrial activity. So lower values indicate less activity, likely more cells are dead.
## Values are zero-centered and made increasing, so resp = 0% means 100% viability.
## resp = 20% would be an 80% decrease viability.
##

## # Current thoughts:
## - As far as # of hits, the multi-conc current AB and LDH cutoffs look okay. But if we are going to base the sc cutoffs on a percent, it might make sense to do the same for the mc
## - We could use a cutoff of 30% or 3bmad, whichever is higher. That way, the cutoff for the AB in the mc would stay at 3bmad
##
## Other thoughtss:<br>
## We probably should change the coff!!<br>
## Is this thinking correct - we woulnd't expect the DMSO wells to cause much of a response at all, so it doesn't make sense to use the DMSO as the threshold for a biologically significant effect in these assays<br>
## I just confirmed that pc20 is a cutoff option in TCPL.<br>
## Would we want to change the cutoff for the MC methods as well?


