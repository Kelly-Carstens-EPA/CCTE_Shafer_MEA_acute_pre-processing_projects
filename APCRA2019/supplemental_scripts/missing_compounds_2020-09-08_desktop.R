library(data.table)
library(tcpl)
library(RMySQL)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

comps <- read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/apcra_pro_list_mea_avail_2020-09-08.csv")
setDT(comps)

# find the spids for these compounds
tcplLoadChem(field = "dsstox_substance_id", val = comps$DTXSID[1])
# oof, that's a lot of spids. hard to tell which one to check

# source all function in folder 'mea-acute-neural-stats-to-mc0-scripts', except for the run_me.R template
scripts <- list.files(path = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts", pattern = "/\.R$", full.names = T)
scripts <- scripts[!grepl("run_me\\.R",scripts) & !grepl("wllt_conc_formalization\\.R",scripts)]
sapply(scripts, source)

dat4 <- get_latest_dat(lvl = "dat4", "APCRA2019")

comps <- read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/apcra_pro_list_mea_avail_2020-09-08.csv")
setDT(comps)

missing_apcra <- setdiff(unique(comps$preferred_name), unique(dat4$treatment))
setdiff(comps[mea.acute == 0, unique(preferred_name)], missing_apcra) # character 0

# for everything in the 84-chem spid map file -> did I collect data for those?
library(openxlsx)
apcra1 <- read.xlsx("L:/Lab/NHEERL_MEA/Project TSCA_APCRA/EPA_18235_EPA-Shafer_84_20181129.xlsx", sheet = 1)
setDT(apcra1)
setdiff(apcra1$EPA_SAMPLE_ID, dat4$spid) # nothing
length(unique(dat4$spid)) # 89
length(unique(apcra1$EPA_SAMPLE_ID)) # 84 (does not include controls)
# So yes, I have inlcuded all 89 compounds as expected from this data set

# check if the list of TSCA spids matching the missing compounds
tsca <- read.xlsx("L:/Lab/NHEERL_MEA/Project TSCA 2019/EPA_25092_EPA-Shafer_339_20190722_key.xlsx", sheet = 1)
setDT(tsca)
not_in_tsca <- setdiff(comps$DTXSID, tsca$DTXSID)
length(not_in_tsca) # 160
intersect(comps$DTXSID, tsca$DTXSID) # 41 compounds that do overlap
intersect(comps[mea.acute == 0, unique(DTXSID)], tsca$DTXSID) # nothing!
# So the missing compounds are not in tsca either
comps[mea.acute == 0]


# checking out this file
all_apcra <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/APCRA Chemicals/APCRA_Prospective_list_revised_tsca_20Aug2018_Acute_MEA.xlsx", sheet = 1))
mea_apcra <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/APCRA Chemicals/APCRA_Prospective_list_revised_tsca_20Aug2018_Acute_MEA.xlsx", sheet = 2))

# are the compounds that katie found are missing in the all_apcra table but not mea_apcra?
missing_apcra <- comps[mea.acute == 0, as.character(unique(DTXSID))]
setdiff(missing_apcra, all_apcra$DTXSID) # character(0)
intersect(missing_apcra, mea_apcra$DTXSID) # "DTXSID6024838"  "DTXSID7046627"  "DTXSID10895020"
intersect(missing_apcra, apcra_tested_info$dsstox_substance_id) # character(0), see below. We did not test these 3
nrow(mea_apcra) # 86
# so there are 3 missing compounds that are on our list of compounds to test.
# could this be a name/salt thing?

# are all of the 84 compounds we did test not in comps table from katie?
apcra_tested_info <- tcplLoadChem(field = "spid", val = unique(dat4$spid))
setdiff(apcra_tested_info$dsstox_substance_id, comps$DTXSID) # one is missing: "DTXSID80873178"
nrow(comps) # 201
# but this one is not one of the 3. So it is probs just a technically apcra compound that Katie is not interested in

compounds_not_included_mea <- setdiff(all_apcra$DTXSID, mea_apcra$DTXSID)
setdiff(comps[mea.acute == 0, unique(DTXSID)], compounds_not_included_mea)
# "DTXSID6024838"  "DTXSID7046627"  "DTXSID10895020"
# So all but 3 of the 68 missign compounds from Katie are in all_apcra but not in mea_apcra
mea_apcra[DTXSID %in% c("DTXSID6024838" , "DTXSID7046627",  "DTXSID10895020"), .(X1, DTXSID, PREFERRED_NAME, CASRN)] # but all 3 are here!!
# X1         DTXSID      PREFERRED_NAME       CASRN
# 1: 142  DTXSID7046627            Bosentan 147536-97-8
# 2: 197  DTXSID6024838 C.I. Solvent Red 80   6358-53-8
# 3:  42 DTXSID10895020   Quintolast sodium 101193-62-8

# So there are 86 compounds from mea_apcra that we should have tested
# But, we did not test these 3 extra (which are also not in all_apcra, for some reason)
# And we did test 1 extra, 
setdiff(apcra_tested_info$dsstox_substance_id, mea_apcra$DTXSID) # "DTXSID80873178"
apcra_tested_info[dsstox_substance_id == "DTXSID80873178", .(dsstox_substance_id, chnm, casn)]
# dsstox_substance_id             chnm        casn
# 1:      DTXSID80873178 Bosentan hydrate 157212-55-0
# So 86 - 3 + 1 = 84, which is how many APCRA compounds we have!

# more vierifiecations
not_teseted <- setdiff(all_apcra$DTXSID, mea_apcra$DTXSID)
length(intersect(not_teseted, missing_apcra))
# [1] 65
setdiff(missing_apcra, not_teseted)
# [1] "DTXSID6024838"  "DTXSID7046627"  "DTXSID10895020"

# okay, just need to work out the math:
# - I have level 0 data for 84 apcra compounds
# - There are 201 apcra compounds total in the table from katie and the all_apcra sheet
# - There are 86 compounds that were selected for testing in the MEAs

# Just verifying that the 84 apcra compounds did make it in the lvl0 data
load("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/lvl0_snapshots/dat4_2020-07-29.RData")
names(dat4)
dat4[origin == "APCRA2019" & wllt == "t", length(unique(spid))] # 84 baby!!

# comparing with the ToxCast list
toxcast_list <- read.xlsx("L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx")
setDT(toxcast_list)
toxcast_list[, dsstox_casrn := sub("'","",dsstox_casrn)]
toxcast_list[, dsstox_casrn := sub("'","",dsstox_casrn)]
intersect(comps[mea.acute == 0, unique(as.character(CASRN))], toxcast_list$dsstox_casrn)
intersect(comps[mea.acute == 1, unique(as.character(CASRN))], toxcast_list$dsstox_casrn) # gut check: several intersect here
length(intersect(comps[mea.acute == 1, unique(as.character(CASRN))], toxcast_list$dsstox_casrn)) # 49 intersect. Got it.
# Ah, so 49 + 84 = 133 APCRA compounds that Katie has!

toxcast_list[grepl("[Pp]irinix",chnm)]

toxcast_list[grepl("[Pp]irin",chnm)]

toxcast_list[grepl("[Tt]hiaben",chnm)]

toxcast_list[grepl("perfluoro",chnm)]

toxcast_list[grepl("lurorotelomer",chnm)]

toxcast_list[grepl("[Pp]hora",chnm)]

toxcast_list[grepl("[Pp]ara",chnm)]
rm(toxcast_list)

# Compare with TSCA list
tsca_list <- read.xlsx("L:/Lab/NHEERL_MEA/Project TSCA 2019/EPA_25092_EPA-Shafer_339_20190722.xlsx")
setDT(tsca_list)
tsca_info <- tcplLoadChem(field = "spid", val = unique(tsca_list$EPA_SAMPLE_ID))
intersect(comps[mea.acute == 0, unique(as.character(DTXSID))], tsca_info$dsstox_substance_id) # character(0)!
intersect(comps[, unique(as.character(DTXSID))], tsca_info$dsstox_substance_id) # gut check: several intersect here
length(intersect(comps[, unique(as.character(DTXSID))], tsca_info$dsstox_substance_id)) # 41 intersect. Got it.
length(intersect(comps[mea.acute == 0, unique(as.character(DTXSID))], tsca_info$dsstox_substance_id)) # 0!
length(intersect(comps[mea.acute == 1, unique(as.character(DTXSID))], tsca_info$dsstox_substance_id)) # all 41 are here...
# 
rm(tsca_list)

