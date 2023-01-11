# ---
# title: "MEA Acute SC Evaluation"
# author: "Amy Carpenter"
# date: "June 27, 2022"
# output:
# html_document:
#    code_folding: hide
#    toc: true
#    toc_float: true
#    df_print: paged
# ---

#+ include = FALSE
knitr::opts_chunk$set(out.width = '100%')

library(data.table)
library(openxlsx)
library(ggplot2)

## # Purpose ----
## To decide if we are ready to release the acute sc data in tcpl to the public. Specifically,
## * To understand how the tcpl acute sc results aligned with what was published in Strickland, et al 2018
## * To understand how the tcpl acute sc results align with the samples that were / were not retested in the mc
## * Evaluate how well the acute sc results behave as we expect
##    * If the tcpl results are close enough to what was published in Strickland 2018, use those analysis to validate
##    * Else, identify your own assay controls and evaluate 
##

## # Compare Strickland 2018 and tcpl sc results ----

## ## Prepare tables to compare ----

# Load strickland results
strickland.res <- as.data.table(read.xlsx('investigations/sc_and_cytotox_endpoints_July2022/data/Stricklandetal2018_AcuteSC-Supplement1.xlsx', sheet = 1))

# Load tcpl results
# load('../../mea_nfa_vs_acute/data/mea_acute_and_dev_sc2_2022-02-10.RData')
# sc2 <- sc2[assay_type == 'acute']
load('investigations/sc_and_cytotox_endpoints_July2022/data/sc0_and_sc2_invitrodb_05042022_2022-08-03.RData')

head(strickland.res)
new.colnames <- strickland.res[1,]
setnames(strickland.res, old = names(strickland.res), new = unlist(new.colnames))
strickland.res <- strickland.res[2:nrow(strickland.res)]

## Total chemicals in TCPL sc:
sc2[!is.na(dsstox_substance_id), length(unique(dsstox_substance_id))]

## Total samples in TCPL sc:
sc2[!is.na(spid), length(unique(spid))]

## Total chemicals in strickland 2018
strickland.res[grepl('^DT',DSSTox_Substance_Id), length(unique(DSSTox_Substance_Id))]
# what's different?
strickland.res[!DSSTox_Substance_Id %in% sc2$dsstox_substance_id, .(DSSTox_Substance_Id, Substance_Name, MEA_hit_detail)]
## Ah, yes. We probably just don't have a sample ID for Bicuculline in tcpl, so it wasn't entered with a dtxsid


## Does the MEA hit only refer to the MFR, and not the LDH or AB?<br>
## From abstract: "only 8 compounds decreased cell viability" - so this won't affect very many<br>
## Also from paragraph "Assay Data Analysis", it's clear that in supplementary table 1 the hits refer to the normalized mean firing rate hit calls

# In tcpl, any conflict regarding hit direction?
sc2[grepl('MFR',aenm), .N, by = .(aenm)]
sc2[grepl('MFR',aenm), .(sum(hitc %in% 1)), by = .(spid)][V1 > 1]
# empty, good

# Any cases of multiple spid tested for a given chem?
sc2[, .(length(unique(spid))), by = .(dsstox_substance_id)][V1 > 1]
# 10 cases

# What does strickland table look liek in these cases?
check.chem <- sc2[, .(length(unique(spid))), by = .(dsstox_substance_id)][V1 > 1 & !is.na(dsstox_substance_id), dsstox_substance_id]
strickland.res[DSSTox_Substance_Id %in% check.chem]
# hemm, looks like just 1 sample is shown

# Agreement with Strickland?
# Collapse by sample
tcpl.res <- sc2[grepl('MFR',aenm), .(MEA_hit = ifelse(any(hitc %in% 1), 1, 0),
                                     MEA_hit_detail = ifelse(any(hitc %in% 1), aenm[hitc %in% 1], '0')),
                by = .(dsstox_substance_id, casn, chnm, spid)]
tcpl.res[, MEA_hit_detail := sub('CCTE_Shafer_MEA_MFR_','',MEA_hit_detail)]
tcpl.res[MEA_hit_detail == 'dn', MEA_hit_detail := 'down']

comp.res <- merge(strickland.res, tcpl.res, by.x = 'DSSTox_Substance_Id', by.y = 'dsstox_substance_id', all = T, suffixes = c('.strickland','.tcpl'))
# Note: tcpl.res is broken down by spid
# So rows in stricklnad.res will be duplicated where there are multiple spid mapped to a given dtxsid


# Check for any that didn't map
tcpl.res[is.na(MEA_hit)] 
strickland.res[is.na(MEA_hit)]
# none were NA beforehand
comp.res[is.na(MEA_hit.tcpl)]
# hmm, this is bicuculline
comp.res[is.na(MEA_hit.strickland)]
# all the NA dtxsid's from tcpl
# Let's assign the dtxsid for bic to tcpl.res
tcpl.res[spid == 'BICUCULLINE', dsstox_substance_id := 'DTXSID60873651']
comp.res <- merge(strickland.res, tcpl.res[!is.na(dsstox_substance_id)], by.x = 'DSSTox_Substance_Id', by.y = 'dsstox_substance_id', all = T, suffixes = c('.strickland','.tcpl'))
comp.res[is.na(MEA_hit.tcpl)]
comp.res[is.na(MEA_hit.strickland)]
# cool, empty now

## ## Compare results!

comp.res[, .N, by = .(MEA_hit.strickland, MEA_hit.tcpl)]
comp.res[, .(num_spid = .N), by = .(MEA_hit_detail.strickland, MEA_hit_detail.tcpl)][order(-num_spid)]
## Sweet, the vast majority of cases agree!
##

## Check out the few cases that don't agree
comp.res[, num_spid_tested := .N, by = .(DSSTox_Substance_Id)]
comp.res[MEA_hit_detail.strickland != MEA_hit_detail.tcpl, .(Substance_Name, spid, num_spid_tested, strickland = MEA_hit_detail.strickland, tcpl = MEA_hit_detail.tcpl)]

## in all but 2 cases, there were multiple spid tested...
## so probably some agreed, adn some did not

comp.res[, plot_name := paste0(Substance_Name,' - ',spid)]

plotdat <- melt(comp.res[num_spid_tested > 1], id.vars = c('Substance_Name','spid'), measure.vars = c('MEA_hit_detail.tcpl','MEA_hit_detail.strickland'))
plotdat[, variable := sub("MEA_hit_detail\\.","",variable)]
plotdat[variable == 'strickland', variable := 'strickland\n(spid agnostic)']

#+ fig.height = 11

ggplot(plotdat, aes(x = variable, y = spid)) +
  geom_tile(aes(fill = value), col = 'white')+
  scale_fill_manual(values = c('0' = 'white',
                                 'down' = 'blue',
                                 'up' = 'red'),
                    name = 'MFR hit detail')+
  scale_x_discrete(position = 'top')+
  facet_wrap(vars(Substance_Name), ncol = 1,
             scales = 'free_y')+
  theme_bw()+
  ggtitle('MEA Acute Strickland et al 2018 vs TCPL hit calls\nfor substances with multiple tested samples')+
  theme(axis.text.x = element_text(size = 15))

## Looks like in every case, one of the samples in tcpl isn't active<br>
## 12 of the mismatches in hit can be explained by the fact that for the 2018 manuscript, they probably just used the most active sample
## That leaves only 2 truly conflicting hits - that's pretty minimal!!
##
##
## The 2 spid's with conflicting hit calls
check.spid <- comp.res[MEA_hit_detail.strickland != MEA_hit_detail.tcpl & num_spid_tested == 1, spid]
comp.res[spid %in% check.spid, .(chnm, spid, MEA_hit_detail.strickland, MEA_hit_detail.tcpl)]
sc2[spid %in% check.spid & grepl('MFR',aenm), .(chnm, spid, aenm, max_med, bmad, coff, tcpl_hitc = hitc)]
## Thiamethoxam is pretty close to being an up hit in tcpl<br>
## 3,4-Diaminotoluene looks VEEERY borderline for the down hit<br>
## So these cases that differed from Strickland 2018 are quite borderline

##
## 
## Conclusion: <br>
## Out of the 1055 chemicals and 1104 sample IDs, the tcpl and strickland 2018 acute sc results are VERY comparable<br>
## There are 2 borderline cases where a hit call is added or removed<br>
## And 12 other spid's that were inactive, but at least 1 sample of the given chemical was active, so the chemical was reported as active in Strickland 2018<br>
## (Note that the only acute SC data in tcpl to date comes from the ToxCast library screening that was published in Strickland 2018)<br>
## So at the sample level, 1090/1104 = 98.7% of the samples have the same MFR hit call (whether up, down, or no hit)<br>
## Therefore, I think we can rely on the analyses of the the MEA acute endpoints as previously published (no new analyses are needed)