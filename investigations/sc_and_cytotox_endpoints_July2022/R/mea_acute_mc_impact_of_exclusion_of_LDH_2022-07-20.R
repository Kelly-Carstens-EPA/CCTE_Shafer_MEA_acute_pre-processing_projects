## ---
## title: "MEA Acute MC TCPL Evaluation of LDH and AB results"
## author: "Amy Carpenter"
## date: "July 20, 2022"
## output: 
##   html_document:
##     code_folding: hide
##     toc: true
##     toc_float: true
##     df_print: paged
## ---

#+ include = FALSE
knitr::opts_chunk$set(out.width = '100%')

library(data.table)
library(openxlsx)
library(stringi)
library(ggplot2)

## # Purpose ----

## To check how much the cytotoxicity results will be affected if we exclude the LDH data. We are considering excluding the LDH because that is what was done in Kosnik et al., 2019.
##

## # Prepare data ----

# Load tcpl results
# load('../../mea_nfa_vs_acute/data/mea_acute_and_dev_mc5_mc6_2022-02-10.RData')
mc.dat <- read.xlsx('L:/Lab/NHEERL_MEA/ccte_shafer_mea_acute/ccte_shafer_mea_acute_multiconc_8Feb2022.xlsx', sheet = 'mc5+mc6')
# Have to get the cytotoxicity data from the older table, because the cyto endpoints were not re-pipelined in Feb 2022, since no changes were needed to the normalization methods
mc.dat2 <- as.data.table(read.csv('L:/Lab/NHEERL_MEA/ccte_shafer_mea_acute/mc5_mc6_CCTE_SHAFER_MEA_ACUTE_05AUG2020.csv'))
setDT(mc.dat)
setdiff(names(mc.dat), names(mc.dat2))
setdiff(names(mc.dat2), names(mc.dat))
mc.dat2[, X := NULL] # this is just row.names
mc.dat <- rbind(mc.dat, mc.dat2)
##

## What is the aeid again?
mc.dat[grepl("LDH",aenm), unique(aeid)]

## Recall that this mc data only includes the cytotox and 15 main acnm's from kosnik 2019:
mc.dat[, .N, by = .(aenm)]

## How many sample screened?
mc.dat[grepl("(LDH)|(AB)",aenm), length(unique(spid)), by = .(aenm)]

## #Compare LDh adn AB hit calls ----
mc.dat[, .(ldh_hitc = unique(hitc[grepl('LDH',aenm)]),
                                   ab_hitc = unique(hitc[grepl('AB',aenm)])),
       by = .(dsstox_substance_id, spid, chnm)][, .(num_samples = .N), by = .(ldh_hitc, ab_hitc)][order(ldh_hitc, ab_hitc)]
## Okay, so there are 11 samples that are only a hit in the LDH, but not in AB<br>
## (note that the 6 samples that are NA for ab_hitc just weren't tested in the alamar blue assay)
##
## What were the bmad's and cutoffs?
ggplot(mc.dat[grepl('LDH',aenm)], aes(x = max_med))+
  geom_histogram(aes(fill = as.factor(hitc)))+
  geom_vline(aes(xintercept = coff), lty = 'dashed')

## What percent of substances with a max_med above the coff were not hits?
mc.dat[grepl('LDH',aenm), .N, by = .(max_med_above_coff = max_med >= coff, hitc)]

## From Kosnik et al., 2019: "The LDH assay exhibited small BMAD values, resulting in responses for many chemicals being greater than the 3*BMAD threshold, even though they did not demonstrate a concentration response. This decreased the reliability of this parameter for detecting truly cytotoxic compounds and therefore cytotoxicity was characterized using only the CTB assay. Nineteen of these chemicals were also active in the CTB assay indicating potential cytotoxicity. "<br>
## If 13 samples qualified as "many chemicals", I guess we are still seeign that... hmm.<br>
##
## How flag-y are the 11 LDH hits?
ldh.only.hits <- mc.dat[, .(ldh_hitc = unique(hitc[grepl('LDH',aenm)]),
           ab_hitc = unique(hitc[grepl('AB',aenm)])),
       by = .(dsstox_substance_id, spid, chnm)][ldh_hitc == 1 & ab_hitc == 0, unique(spid)]
mc.dat[spid %in% ldh.only.hits & grepl('LDH',aenm), .(spid, bmad, coff, max_med, max_med_conc, modl, modl_rmse, flag.length, mc6_flags)][order(flag.length)]
## Many have more than 3 flags, but note that the 50% efficacy flag is probably not appropriate for this assay

##<br> Are any of these substances borderline active in the AB?
mc.dat[spid %in% ldh.only.hits & grepl('AB',aenm), .(spid, bmad, coff, max_med, max_med_conc, modl, modl_rmse, flag.length, mc6_flags)][order(max_med)]
## The last 2 are semi-close on the max_med to the cutoff, but all fit a cnst model
##
## Okay, what would be the result/impact of removing LDH?
##
## * If these compounds are only cytotoxic, no other activity -> then they coudl be missed as hits
## * if the activity of these compounds is totally driven by cytotoxicity -> then the hits in the other assay could be mistakenly labelled as selective.
##
## Let's check those 2 scenarios. If neither of these are the case, then I think it's all good?
##
## Actually, I'm more just concerned about scenario one (which seems less likely to me). Let's check it out.
mc.dat[spid %in% ldh.only.hits, .(mea_hit_count = sum(hitc[!grepl('LDH',aenm)] %in% 1)), by = .(spid, dsstox_substance_id)][order(mea_hit_count)]
## Oh wow, 3 of these don't have any other hits! Not what I was expecting.
## Let's look at the curves...

## Were these hits at a really high conc?
mc.dat[, hit_count_no_ldh := sum(hitc[!grepl('LDH',aenm)] %in% 1), by = .(spid, dsstox_substance_id)]
mc.dat[spid %in% ldh.only.hits & grepl('LDH',aenm), .(spid, dsstox_substance_id, max_med, modl_tp, coff, modl_ga, logc_max, hit_count_no_ldh, flag.length)][order(hit_count_no_ldh)]
## ah, all 3 that are only a hit in the LDH have a flag for being borderline.<br>
## Regardless of what we do, we are goign to miss some borderlien chemicals. As Tim keeps emphasizing, I have more important things to do than validating teh LDH acute assay. So I'm okay with dropping it, will miss having.<br>
## But I think doing this helps remind me that we don't expect these assays to be perfect -  so the key is to keep evaluating with assay controls, to understanding our confidence, rather than just hope that we can trust the hit calls and ac50s.


##
## 
## July 20, 2022: Melissa pointed out it could be useful to look at the curves. Tim noted that he could be convinced to change his mind on this decision to remove teh LDH endoints. 
