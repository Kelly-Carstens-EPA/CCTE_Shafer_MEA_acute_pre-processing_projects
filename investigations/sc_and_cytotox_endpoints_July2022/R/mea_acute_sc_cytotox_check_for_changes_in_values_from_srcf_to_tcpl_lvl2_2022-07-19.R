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
library(stringi)
library(ggplot2)

## # Purpose ----------------
## Step 1: Check if the values in NHEERL_MEA_SS_SOURCE_RAW_DATA_150422.csv were manipulated before/in tcpl at all (which would change interpretation)
## 
## Step 2: confirm that applying a hit call of 80% and 20% to the TCPL sc2 aB adn LDH, respectively, would produce similar cytotoxicity hits as seen in STrickland, 2018
## 
## If so, I think I would say we should just chang ethe cutoffs?
##

## # Prepare data ----
srcf.dat <- as.data.table(read.csv('L:/Lab/Toxcast_Data/toxcast_data/files/ccte_shafer/nheerl_mea/NHEERL_MEA/NHEERL_MEA_SS_SOURCE_RAW_DATA_150422.csv'))

# load tcpl data
# load('C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_nfa_vs_acute/data/mea_acute_and_dev_sc2_2022-02-10.RData')
# sc2 <- sc2[grepl('(LDH)|(AB)',aenm)]

# # Actually, let's load the lvl 0 as well
# load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/lvl0_snapshots/dat4_2020-07-29.RData')
# sc0 <- dat4[grepl('(LDH)|(AB)',acnm)]
# oh wait, this is the mc0!

# Get the sc0 from tcpl
scdat_file <- list.files(path = 'investigations/sc_and_cytotox_endpoints_July2022/data',
                         pattern = 'sc0_and_sc2_invitrodb_05042022.*\\.RData', full.names = T)
if (length(scdat_file) > 0) {
  load(scdat_file)
} else {
  library(tcpl)
  # library(RMySQL)
  # con <- dbConnect(drv = MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'))
  # db.res <- dbGetQuery(con, 'show databases')
  # db.res[grepl('invitrodb',db.res$Database),]
  # # [1] "dev_invitrodb_fv"                             "invitrodb"                                   
  # # [3] "invitrodb_05042022"                           "invitrodb_test"                              
  # # [5] "prod_internal_invitrodb_v3_1"                 "prod_internal_invitrodb_v3_2"                
  # # [7] "prod_internal_invitrodb_v3_3"                 "prod_internal_invitrodb_v3_4"                
  # # [9] "prod_internal_invitrodb_v3_5"                 "sbox_jbrown20_invitrodb_base_schema"         
  # # [11] "sbox_jbrown20_invitrodb_mc7"                  "sbox_jbrown20_invitrodb_sample_legacy_backup"
  # # [13] "sbox_jbrown20_invitrodb_v3_1"                 "sbox_sflood_invitrodb" 
  # dbDisconnect(con)
  tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db = 'invitrodb_05042022')
  aeid.tb <- tcplLoadAeid(fld = 'asid', val = 20, add.fld = c('anm','acid'))
  acid.tb <- tcplLoadAcid(fld = 'asid', val = 20, add.fld = 'anm')
  sc0 <- tcplPrepOtpt(tcplLoadData(fld = 'acid', val = acid.tb[anm == 'CCTE_Shafer_MEA_acute', acid], type = 'sc', lvl = 0L))
  sc2 <- tcplPrepOtpt(tcplLoadData(fld = 'aeid', val = aeid.tb[anm == 'CCTE_Shafer_MEA_acute', aeid], type = 'sc', lvl = 2L))
  description <- paste0('CCTE_Shafer_MEA_acute sc0 and sc2 data taken from invitrodb_05042022 on ',Sys.Date(),
                        '\nwith the script mea_acute_sc_cytottox_check_for_changes_from_scrf_to_tcpl_lvl2_2022-07-19.R')
  save(sc0, sc2, description, file = file.path('investigations/sc_and_cytotox_endpoints_July2022/data',
                                               paste0('sc0_and_sc2_invitrodb_05042022_',as.character.Date(Sys.Date()),'.RData')))
}


## # Reviewing TCPL normalization methods ----

## ***NOT RUN*** (because this is just documentation of results as of ~ July 20, 2022 in invitrodb_05042022)
# Are any normalization methods applied?
# aeid.tb[acid %in% sc0$acid]
# merge(tcplMthdLoad(lvl = 1L, id = unique(aeid.tb[acid %in% sc0$acid, aeid]), type = 'sc'),
      # aeid.tb[acid %in% sc0$acid], by = 'aeid')
# aeid          mthd mthd_id ordr asid                   aenm                   anm acid
# 1: 2033          none      14    1   20 CCTE_Shafer_MEA_MFR_up CCTE_Shafer_MEA_acute 1916
# 2: 2034          none      14    1   20 CCTE_Shafer_MEA_MFR_dn CCTE_Shafer_MEA_acute 1916
# 3: 2034 resp.multneg1      16    2   20 CCTE_Shafer_MEA_MFR_dn CCTE_Shafer_MEA_acute 1916
# 4: 2035          none      14    1   20    CCTE_Shafer_MEA_LDH CCTE_Shafer_MEA_acute 1917
# 5: 2036          none      14    1   20     CCTE_Shafer_MEA_AB CCTE_Shafer_MEA_acute 1918
# LDH none, AB none
# empty
# merge(tcplMthdLoad(lvl = 2L, id = unique(aeid.tb[acid %in% sc0$acid, aeid]), type = 'sc'),
      # aeid.tb[acid %in% sc0$acid], by = 'aeid')
# aeid           mthd mthd_id asid                   aenm                   anm acid
# 1: 2033          bmad1      22   20 CCTE_Shafer_MEA_MFR_up CCTE_Shafer_MEA_acute 1916
# 2: 2033 ow_bmad_nwells      20   20 CCTE_Shafer_MEA_MFR_up CCTE_Shafer_MEA_acute 1916
# 3: 2034          bmad3       1   20 CCTE_Shafer_MEA_MFR_dn CCTE_Shafer_MEA_acute 1916
# 4: 2034 ow_bmad_nwells      20   20 CCTE_Shafer_MEA_MFR_dn CCTE_Shafer_MEA_acute 1916
# 5: 2035          bmad3       1   20    CCTE_Shafer_MEA_LDH CCTE_Shafer_MEA_acute 1917
# 6: 2035 ow_bmad_nwells      20   20    CCTE_Shafer_MEA_LDH CCTE_Shafer_MEA_acute 1917
# 7: 2036          bmad3       1   20     CCTE_Shafer_MEA_AB CCTE_Shafer_MEA_acute 1918
# 8: 2036 ow_bmad_nwells      20   20     CCTE_Shafer_MEA_AB CCTE_Shafer_MEA_acute 1918
# so no normalization how been done

# Reviewing available methods...
# tcplMthdList(lvl = 2L, type = 'sc')
#    sc2_mthd_id       sc2_mthd                                                                           desc
# 1:           1          bmad3                                                  Add a cutoff value of 3*bmad.
# 2:           2           pc20                                                      Add a cutoff value of 20.
# 3:           3       log2_1.2                                               Add a cutoff value of log2(1.2).
# 4:           4      log10_1.2                                              Add a cutoff value of log10(1.2).
# 5:           5          bmad5                                                  Add a cutoff value of 5*bmad.
# 6:           6          bmad6                                                  Add a cutoff value of 6*bmad.
# 7:           7         bmad10                                                 Add a cutoff value of 10*bmad.
# 8:           8    pc30orbmad3                  Add a cutoff value of either 30 or 3*bmad, whichever is less.
# 9:           9         pc0.88                                                    Add a cutoff value of 0.88.
# 10:          10       log2_1.5                                               Add a cutoff value of log2(1.5).
# 11:          11           pc25                                                       Add a cutoff value of 25
# 12:          12          bmad2                                                   Add a cutoff value of 2*bmad
# 13:          20 ow_bmad_nwells                         overwrite default bmad with bmad calculated on n wells
# 14:          21      log2_0.76 Add a cutoff value of 0.76 for log2-transformed data, originally for aeid 1691
# 15:          22          bmad1                                                   Add a cutoff value of 1*bmad
# 16:          23           pc30                              Add a cutoff value of 30 for percent control data
# 17:          24        bmad1.5                                                 Add a cutoff value of 1.5*bmad

## # Prepare the LDh and AB data from the srcf ---

# Prepare srcf.dat
ldh.dat <- srcf.dat[, .SD, .SDcols = c(grep('(LDH)|(PLATE)|(EXPERIMENT_DATE)',names(srcf.dat),val = T))]
ldh.dat2 <- rbindlist(lapply(1:3, function(i) {
  dati <- ldh.dat[, .SD, .SDcols = c('LDH_WELL_ID','EXPERIMENT_DATE',grep(paste0('RUN',i),names(ldh.dat), val = T))]
  setnames(dati, old = names(dati), new = sub(paste0('_RUN',i),'',names(dati)))
  dati[, run := paste0('RUN',i)]
  dati
}
))
# ldh.dat2


ab.dat <- srcf.dat[, .SD, .SDcols = c(grep('(AB)|(PLATE)|(EXPERIMENT_DATE)',names(srcf.dat),val = T))]
ab.dat2 <- rbindlist(lapply(1:3, function(i) {
  dati <- ab.dat[, .SD, .SDcols = c('AB_WELL_ID','EXPERIMENT_DATE',grep(paste0('RUN',i),names(ab.dat), val = T))]
  setnames(dati, old = names(dati), new = sub(paste0('_RUN',i),'',names(dati)))
  dati[, run := paste0('RUN',i)]
  dati
}
))
# ab.dat2

ab.dat2[, acnm := 'CCTE_Shafer_MEA_AB']
ab.dat2[, rval_desc := 'AB_percent_viable']
setnames(ab.dat2, old = 'AB_.VIABLE', new = 'rval')
setnames(ab.dat2, old = names(ab.dat2), new = sub('AB_','',names(ab.dat2)))
ldh.dat2[, acnm := 'CCTE_Shafer_MEA_LDH']
ldh.dat2[, rval_desc := 'LDH_percent_dead']
setnames(ldh.dat2, old = 'LDH_.DEAD', new = 'rval')
setnames(ldh.dat2, old = names(ldh.dat2), new = sub('LDH_','',names(ldh.dat2)))

cyto.srcf.dat2 <- rbind(ab.dat2, ldh.dat2)
cyto.srcf.dat2[, coli := as.numeric(stri_extract(WELL_ID, regex = '[1-9]+'))]
cyto.srcf.dat2[, rowi := match(stri_extract(WELL_ID, regex = '[A-Z]*'), LETTERS)]
cyto.srcf.dat2[, plate.id := gsub(' ','',MEA_PLATE_ID)]
cyto.srcf.dat2[, EXPERIMENT_DATE := as.character(EXPERIMENT_DATE)]

## # Compare TCPL sc0 to srcf values -----------

## Prepare to merge and merge, after some cleaning
# Checkign for apparent duplicated data rows
cyto.srcf.dat2[, .N, by = .(acnm, rowi, coli, MEA_PLATE_ID, EXPERIMENT_DATE, rval)][N > 1]
cyto.srcf.dat2 <- cyto.srcf.dat2[, unique(.SD), .SDcols = names(cyto.srcf.dat2)]
cyto.srcf.dat2[, .N, by = .(acnm, rowi, coli, MEA_PLATE_ID, EXPERIMENT_DATE, rval)][N > 1]
# empyt, cool!

# Doe sthis affect sc0 too?
sc0.cyto <- sc0[grepl('(LDH)|(AB)',acnm)]
sc0.cyto[, .N, by = .(acnm, rowi, coli, apid, rval, spid)][N > 1]
# even though theoretically these could have een repeated, it's unlikey that exactly the same rval would have occured on teh same apid and well, tested at different times
sc0.cyto <- sc0.cyto[, unique(.SD), .SDcols = setdiff(names(sc0),'s0id')]
sc0.cyto[, .N, by = .(acnm, rowi, coli, apid, rval, spid, wllq)][N > 1]
# this is empty

nrow(cyto.srcf.dat2) # 8400
nrow(sc0.cyto) # 5 fewer rows - wha tcan ya do
comp.dat <- merge(cyto.srcf.dat2, sc0.cyto, by.x = c('acnm','rowi','coli','MEA_PLATE_ID'),
                  by.y = c('acnm','rowi','coli','apid'),all = T)

# Anything not map?
comp.dat[, .N, by = .(is.na(rval.y), is.na(rval.x))]
# 10 values...
comp.dat[is.na(rval.x)][order(acnm, chnm)]
# so in some cases, it looks liek the wllq is 0 in tcpl regardless.
# Regardless, I'm just looking for general trends in agreement of the concept of the assay values -> I'm okay with missing 10 cases rn
comp.dat <- comp.dat[!is.na(rval.x)]

# Expected number of rows?
nrow(comp.dat)
nrow(cyto.srcf.dat2)
# woah, waaay more!
# Ah, this is probably because where a plate was reused on multiple experiment dates, r is duplicating the merging so that every cobmiantion is present
# let's de-duplicate by choosing the closest matching rval's

## Attempting to de-duplicate mapped data where plates were reused
comp.dat[, rval_diff := rval.y - rval.x]
comp.dat[is.na(rval.x) | is.na(rval.y), rval_diff := 0]

comp.dat2 <- comp.dat[, .(rval.y = unique(rval.y[rval_diff == min(rval_diff)])[1]), by = .(acnm, rowi, coli, WELL_ID, MEA_PLATE_ID, EXPERIMENT_DATE, rval.x)]
nrow(comp.dat2) # just 1 1 more row than in sc0.cyto... that seems close
# maybe i hsould have included spid... but i don't want to get into that rn

comp.dat2[, .N, by = .(acnm, rowi, coli, MEA_PLATE_ID, EXPERIMENT_DATE)][N > 1]
# ugh... what is this??
# does this nullify my conclusion?
# well, it's 42 data points, out of several. I don't know. I'm jsut going to mention this as areason why I'm having some haziness
# 


## Visualize agreement of data points
ggplot(comp.dat2[grepl("LDH",acnm)], aes(x = rval.x, y = rval.y))+
  geom_point()+
  geom_abline(slope = 1, intercept=0)+
  ylab('tcpl rval lvl0')+
  xlab('srcf LDH percent dead')+
  ggtitle('LDH rvals in tcpl vs srcf')

ggplot(comp.dat2[grepl("AB",acnm)], aes(x = rval.x*-1, y = rval.y))+
  geom_point()+
  geom_abline(slope = 1, intercept=0)+
  ylab('tcpl rval lvl0')+
  xlab('srcf AB viablility, * -1')+
  ggtitle('AB rvals in tcpl vs srcf')

# I think I may just need to manipulate...
ggplot(comp.dat2[grepl("AB",acnm)], aes(x = (100-rval.x), y = rval.y))+
  geom_point()+
  geom_abline(slope = 1, intercept=0)+
  ylab('tcpl rval lvl0')+
  xlab('100 - (srcf AB viablity)')+
  ggtitle('AB rvals in tcpl vs srcf, after correction')
# ah, much better agreement!


## Percent of data in agreement?
# ohh... kay. So it seems that there is a subset of data that does agree, and another subset that doesn't
comp.dat2[grepl("LDH",acnm), .N, by = .(rvals_agree = abs(rval.x - rval.y) < 0.0001)]
429/nrow(comp.dat2[grepl("LDH",acnm)])
# so about 90% of the values agree

comp.dat2[grepl("AB",acnm), .N, by = .(rvals_agree_after_correction = abs((rval.x-100)*-1 - rval.y) < 0.0001)]
463/nrow(comp.dat2[grepl("AB",acnm)])
# again, about 89% of the AB values agree AFTER the adjustment

## What's goign on for these rogue values?? I want to conclude that the LDH and AB values in TCPL MUST correspond to % dead and % dead in LDH and AB
##<br>But these rogue poitns are throwing me off...
comp.dat2[grepl("LDH",acnm), rval.x_cor := rval.x]
comp.dat2[grepl("AB",acnm), rval.x_cor := (rval.x-100)*-1]
comp.dat2[, rvals_agree := abs(rval.x_cor - rval.y) < 0.0001]
comp.dat2[rvals_agree == 0, .N, by = .(EXPERIMENT_DATE)]
# 10 different epxeirments, looks like not all points from each experiment
# most are from 2014
comp.dat2[rvals_agree == 0, .N, by = .(rowi, coli)]
comp.dat2[rvals_agree == 0, .N, by = .(WELL_ID)]
# doesn't look like any well in particular is affected

## do these rogue points all come from plates that were used umultple times?
reused.plates <- comp.dat2[, .(length(unique(EXPERIMENT_DATE))), by = .(MEA_PLATE_ID)][V1 > 1, MEA_PLATE_ID]
comp.dat2[, plate_was_reused := as.numeric(MEA_PLATE_ID %in% reused.plates)]
comp.dat2[, .N, by = .(rvals_agree, plate_was_reused)][order(rvals_agree, plate_was_reused)]
# rvals_agree plate_was_reused    N
# 1:       FALSE                0    6
# 2:       FALSE                1  886
# 3:        TRUE                0 6662
# 4:        TRUE                1  842
## ahh!! All but 6 of the cases of the disagreeing rval's come from plates that were reused!! 
## In those cases, I just made my best guess of which rval's to match up, since i don't have the dates in the sc0.
##
## by assay:
comp.dat2[, .N, by = .(acnm, rvals_agree, plate_was_reused)][order(rvals_agree, plate_was_reused)]
## What percent of data rows are affected by plate reuse?
comp.dat2[, .N, by = .(acnm, plate_was_reused)]
3334/(3334+864)

## Let's remake those plots...
ggplot(comp.dat2[grepl("LDH",acnm)], aes(x = rval.x, y = rval.y))+
  geom_point(aes(color = as.factor(plate_was_reused)))+
  geom_abline(slope = 1, intercept=0)+
  ylab('tcpl rval lvl0')+
  xlab('srcf LDH percent dead')+
  ggtitle('LDH rvals in tcpl vs srcf')

ggplot(comp.dat2[grepl("AB",acnm)], aes(x = (rval.x-100)*-1, y = rval.y))+
  geom_point(aes(color = as.factor(plate_was_reused)))+
  geom_abline(slope = 1, intercept=0)+
  ylab('tcpl rval lvl0')+
  xlab('100 - (srcf AB viablity)')+
  ggtitle('AB rvals in tcpl vs srcf, after correction')

## Any explanation for the 6 other cases where rvals disagree but the plate was not reused/
comp.dat2[rvals_agree == FALSE & plate_was_reused == 0]
# looks liek all of these were well A8. Was this the lysis well?
check.ids <- comp.dat2[rvals_agree == FALSE & plate_was_reused == 0, unique(.SD), .SDcols = c('MEA_PLATE_ID','rowi','coli')]
setkey(sc0.cyto, apid, rowi, coli)
setnames(check.ids, old = 'MEA_PLATE_ID', new = 'apid')
sc0.cyto[.(check.ids), .(spid, acnm, apid, rowi, coli, wllt, wllq, rval)]
# nope, not a lysis well.
# part of me wants to investigate why this sample is not lining up... but honestly, I think there may be multiple data mapping issues.
# Adn I don't automatically know if the srcf or the sc0 is correct. I don't think Tim would want me to spend time of this. 
# I'm goign to move forward, knowing tha the dat aisn't perfect, it's just a suggestion.


## # Compare hits in STrickland 2018 to hits in tcpl -----
'only 8 compounds decreased cellular viability following exposure. Compounds inducing cytotoxicity were: UK-337312, tributyltin chloride, tributyltin methacrylate, phenylmercuric acetate, 9-phenanthrol, gentian violet, mercuric chloride, and ketoconazole'

sc2[tolower(chnm) %in% tolower(c('UK-337312', 'tributyltin chloride', 'tributyltin methacrylate', 'phenylmercuric acetate', '9-phenanthrol', 'gentian violet', 'mercuric chloride', 'ketoconazole')),
    .N, by = .(chnm)]
# cool, all 8 substances are here
sc2[, strickland_cyto_hit := as.numeric(tolower(chnm) %in% tolower(c('UK-337312', 'tributyltin chloride', 'tributyltin methacrylate', 'phenylmercuric acetate', '9-phenanthrol', 'gentian violet', 'mercuric chloride', 'ketoconazole')))]

sc2[grepl('AB',aenm), coff_alt := 80]
sc2[grepl('LDH',aenm), coff_alt := 20]

sc2[, hitc_alt := as.numeric(max_med >= coff_alt)]

sc2[, .(current_ldh_hit = hitc[grepl('LDH',aenm)],
        alt_ldh_hit = hitc_alt[grepl('LDH',aenm)],
        current_ab_hit = hitc[grepl('AB',aenm)],
        alt_ab_hit = hitc_alt[grepl('AB',aenm)]),
    by = .(strickland_cyto_hit, dsstox_substance_id)][, 
                                                      .N, by = .(strickland_cyto_hit, current_ldh_hit,alt_ldh_hit,current_ab_hit,alt_ab_hit)]
sc2.sum <- sc2[, .(current_ldh_hit = hitc[grepl('LDH',aenm)],
                   alt_ldh_hit = hitc_alt[grepl('LDH',aenm)],
                   current_ab_hit = hitc[grepl('AB',aenm)],
                   alt_ab_hit = hitc_alt[grepl('AB',aenm)]),
               by = .(strickland_cyto_hit, dsstox_substance_id, chnm, spid)]
sc2.sum[alt_ldh_hit == 1| alt_ab_hit == 1, .N, by = .(chnm, dsstox_substance_id, spid, strickland_cyto_hit, alt_ldh_hit, alt_ab_hit)]

## All Chemicals that would have a cyto hit with new cutoffs 
sc2.sum[alt_ldh_hit == 1| alt_ab_hit == 1, .N, by = .(chnm, dsstox_substance_id, spid, strickland_cyto_hit, alt_ldh_hit, alt_ab_hit)][order(alt_ldh_hit, alt_ab_hit)]

## All chem that should have at least 1 cyto hit according to strickland
sc2.sum[strickland_cyto_hit == 1, .N, by = .(chnm, dsstox_substance_id, spid, strickland_cyto_hit, alt_ldh_hit, alt_ab_hit)][order(alt_ldh_hit, alt_ab_hit)]


## ## Given that we don't have an 80% cutoff currently in tcpl, what if we used 10bmad instead? ----
sc2[grepl('AB',aenm), coff_alt2 := 10*unique(bmad)]
sc2[grepl('LDH',aenm), coff_alt2 := 20]
sc2[, hitc_alt2 := as.numeric(max_med >= coff_alt2)]

sc2.sum <- sc2[, .(current_ldh_hit = hitc[grepl('LDH',aenm)],
                   alt2_ldh_hit = hitc_alt2[grepl('LDH',aenm)],
                   current_ab_hit = hitc[grepl('AB',aenm)],
                   alt2_ab_hit = hitc_alt2[grepl('AB',aenm)]),
               by = .(strickland_cyto_hit, dsstox_substance_id, chnm, spid)]

## All Chemicals that would have a cyto hit with new cutoffs 
sc2.sum[alt2_ldh_hit == 1| alt2_ab_hit == 1, .N, by = .(chnm, dsstox_substance_id, spid, strickland_cyto_hit, alt2_ldh_hit, alt2_ab_hit)][order(alt2_ldh_hit, alt2_ab_hit)]
# So 2 additional substances would be a hit, 1 of which was a hit in strickland et al, 2018
# Ugh, this feels so arbitrary. It isn't based on truthfulness, just guess & check to see what looks good comapred to previous results
# I'm going to ask that Kaatie add a pc80% cutoff. Will circle back if that is an issue

## All chem that should have at least 1 cyto hit according to strickland
sc2.sum[strickland_cyto_hit == 1, .N, by = .(chnm, dsstox_substance_id, spid, strickland_cyto_hit, alt2_ldh_hit, alt2_ab_hit)][order(alt2_ldh_hit, alt2_ab_hit)]


## ## Okay, one last check - what if we used 88%? ----
sc2[grepl('AB',aenm), coff_alt2 := 88]
sc2[grepl('LDH',aenm), coff_alt2 := 20]
sc2[, hitc_alt2 := as.numeric(max_med >= coff_alt2)]

sc2.sum <- sc2[, .(current_ldh_hit = hitc[grepl('LDH',aenm)],
                   alt2_ldh_hit = hitc_alt2[grepl('LDH',aenm)],
                   current_ab_hit = hitc[grepl('AB',aenm)],
                   alt2_ab_hit = hitc_alt2[grepl('AB',aenm)]),
               by = .(strickland_cyto_hit, dsstox_substance_id, chnm, spid)]

## All Chemicals that would have a cyto hit with new cutoffs 
sc2.sum[alt2_ldh_hit == 1| alt2_ab_hit == 1, .N, by = .(chnm, dsstox_substance_id, spid, strickland_cyto_hit, alt2_ldh_hit, alt2_ab_hit)][order(alt2_ldh_hit, alt2_ab_hit)]
# Still have 2 substances that "Shouldn't" be a hit

## All chem that should have at least 1 cyto hit according to strickland
sc2.sum[strickland_cyto_hit == 1, .N, by = .(chnm, dsstox_substance_id, spid, strickland_cyto_hit, alt2_ldh_hit, alt2_ab_hit)][order(alt2_ldh_hit, alt2_ab_hit)]
# 4/8 strickland cyto hits would nto be detected

## How many chem total screened?
sc0[wllt == 't', length(unique(spid))]
# 1101
sc0[wllt == 't', length(unique(dsstox_substance_id))]
# 1055
