# script to check if the 'outlier' values were actually removed from the pipelined mc0 data
# 06/12/2020

# excerpt from 'source_to_lvl0_nheerl_mea_acute.R'
# where the 24 "outlier" wells were removed

# # from "ToxCast MEA data Outliers and data check.docx" ( we do not have this file )
# # remove columns 7-8 for plateID 'MW 1076-37' on 20160317
# idx <- which(df$MEA_PLATE_ID_RUN1=='MW 1076-37' &
#                df$EXPERIMENT_DATE=='20160317' &
#                df$`MEA_WELL_ID _ALL_RUNS`%in%paste0(paste0(LETTERS[1:6],'0'),rep(7:8,each=6)))
# df[idx, 14 + c(0*43 + 1:43, (0+3)*43 + 1:43)] <- NA # Run 1 baseline and dose columns
# 
# # remove columns 1-2 for plateID 'MW 1048-15' on 20160531
# idx <- which(df$MEA_PLATE_ID_RUN3=='MW 1048-15' &
#                df$EXPERIMENT_DATE=='20160531' &
#                df$`MEA_WELL_ID _ALL_RUNS`%in%paste0(paste0(LETTERS[1:6],'0'),rep(1:2,each=6)))
# df[idx, 14 + c(2*43 + 1:43, (2+3)*43 + 1:43)] <- NA # Run 3 baseline and dose columns
# 
# # change plateID 'MW 1042-50' to 'MW 1044-1' for run 3 (said corrected in raw file, but still here)
# df$MEA_PLATE_ID_RUN3[df$MEA_PLATE_ID_RUN3=='MW 1042-50'] <- 'MW 1044-1'

# inital attempt ------------------------------------------------------------------------------------
# get the data - this is the most recent "reevaluation" data from Katie
# but she pulled/started with the data that was on invitrodb before then
mc0 <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/output/01APR2020/mc0_nheerl_mea_acute.csv", stringsAsFactors = F)
library(data.table)
setDT(mc0)
mc0[apid == "MW 1076-37" & coli == 1, .N] # 204 = 17 acid's * 12 wells. -> this apid must have been reused
mc0[apid == "MW 1076-37" & coli == 2, .N] # 102 = 17 acid's * 6 wells -> hmm, so only have data from 1 plate now...
mc0[apid == "MW 1076-37" & coli == 7, .N] # 102 = 17 acid's * 6 wells -> hmm, so only have data from 1 plate now...
# thought: I know the original data did not include the DMSO n stuff...
# so maybe Katie added that in later...

mc0[apid == "MW 1048-15" & coli %in% 1:2 & !is.na(rval), .N, by = c("rowi","coli")]
mc0[apid == "MW 1048-15" & coli %in% 1:2 & is.na(rval), .N, by = c("rowi","coli")]
# all non-cyto endpoints in col 1 are NA
# only some endpoints in col 2 row 1 are NA

# the mc0 file has only 306 data rows from this plate, cols 1 adn 2
# for col2, there are only 102 data rows = 6 *17. This implies there is only data from 1 plate
mc0[apid == "MW 1048-15" & coli %in% 2] # so I guess this columns was just removed
# fro col1, there are 204 data rows = 6*17*2. This implies that there is data from 2 plates
mc0[apid == "MW 1048-15" & coli %in% 1, .N]
mc0[apid == "MW 1048-15" & coli %in% 1 & is.na(rval), .N] # 100 = 6*17 - cytotox data from 1 well? 
mc0[apid == "MW 1048-15" & coli %in% 1 & is.na(rval), length(unique(rowi))] # all 6 rows
mc0[apid == "MW 1048-15" & coli %in% 1 & !is.na(rval), .N, by = "rowi"]
# okay, so I think the 100 = 6wells * 15 endpoints + 10 from well F1 that were NA already


# 06/14/2020 ---------------------------------------------------------------------------------
# loading mc0 data from 11Feb2020 reevaluation, closer to what was originally pipelined i believe
rm(mc0)
load('L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/R/reevaluation/mea_acute_11Feb2020_invitrodb.RData')
mc0.mea.acute[coli == 1] # empty. Wow, this whole column was just removed.
mc0.mea.acute[apid == "MW 1076-37" & coli == 7] # 102 rows = 17 acid's * 6 wells.
mc0.mea.acute[apid == "MW 1076-37" & coli == 8] # 102 data rows
# looking in flat file, 
# this apid was used in 20151027 as well as 20160317.
# Weird that the NA values are not even present... but def looks like they are not there
mc0.mea.acute[is.na(rval)] # okay, so some NA's are present

# see next plate
mc0.mea.acute[apid == "MW 1048-15" & coli == 1] # empty, as expected.
mc0.mea.acute[apid == "MW 1048-15" & coli == 2] # 102 rows. = 17 endpoints * 6 wells
# again, this plate was used in 20160531 as well as 20151006
# hmm.. what do other col's look like?
mc0.mea.acute[apid == "MW 1048-15" & coli == 3] # 102 rows. huh, still only look like one plate here
# oh right! "repeat" cultures were removed regardless

# let me verify that all previous repeats are not present
# ea chem would be 17 endpoints * 7 concs * 3 replicates = 357
mc0.mea.acute[, .N, by = c("spid","chnm")][order(N)]
# ya, looks like most chem have 357 data rows
# about 108 chem have 347-355 points. Probs just some accidental drops
# only emamectin benzoate has more than 357 (714 data rows)

# see if the compounds tested on these 2 plates were repeated at a later culture date
# loading my dat4
load("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat4_2020-06-10.RData")
dat4[spid == "TP0001412A04", unique(experiment.date)] # "20151006" Oh, okay, so this well is just NA idependently.
dat4[plate.id == "MW1048-15", unique(experiment.date)] # "20151006" "20160531"
# were all of the compounds tested on "20160531" later retested, so that data was removed regardless?
platerun2_spids <- dat4[plate.id == "MW1048-15" & experiment.date == "20160531", unique(spid)]
platerun2_spids <- setdiff(platerun2_spids, c("DMSO","Bicuculline","Tritonx100"))
dat4[spid %in% platerun2_spids, .(paste0(sort(unique(experiment.date)),collapse = ",")), by = "spid"]
# spid                V1
# 1: TP0001413D06 20160531,20160714
# 2: TP0001413D03 20160531,20160714
# 3: TP0001413D07 20160531,20160714
# 4: TP0001413D05 20160531,20160714
# 5: TP0001413D04 20160531,20160714
# 6: TP0001413D02 20160531,20160714
# okay, so yes, ALL of these SPIDs were retested on 20160714!
# so that is why the data from "20160531" "MW1048-15" is not even present for any columns 
# (in souce_to_lvl0_...R script, all previous runs were removed for each spid)


# do this same investigation for the previous plate
dat4[plate.id == "MW1076-37", unique(experiment.date)] # "20151027" "20160317"
# were all of the compounds tested on "20160317" later retested, so that data was removed regardless?
platerun2_spids <- dat4[plate.id == "MW1076-37" & experiment.date == "20160317", unique(spid)]
platerun2_spids <- setdiff(platerun2_spids, c("DMSO","Bicuculline","Tritonx100"))
dat4[spid %in% platerun2_spids, .(paste0(sort(unique(experiment.date)),collapse = ",")), by = "spid"]
# spid                V1
# 1: TP0001413B10 20160317,20160329
# 2: TP0001413B05 20160317,20160329
# 3: TP0001413B06 20160317,20160329
# 4: TP0001413B09 20160317,20160329
# 5: TP0001413B07 20160317,20160329
# 6: TP0001413B08 20160317,20160329
# okay, so yes, all of these spids were retested on 20160329
# so none of the data from 20160317 was even kept

# just curious, what were my wllq assingments for this plate?
dat4[plate.id == "MW1076-37" & experiment.date == "20160317", .N, by = c("wllq","wllq_notes")]
# only 3 wells really were removed.