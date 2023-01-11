# how many F1 Lysis wells that are used for the 6 other Lysis control wells have wllq=0?
library(data.table)
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/"
setwd(root_output_dir)
source("mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R")

dat4 <- get_latest_dat("dat4")
dat4[wllq == 0 & rowi == 6 & coli == 1 & (grepl("LDH",acnm) | grepl("LDH",acsn)), .N, by = "wllq_notes"]
# wllq_notes  N
# 1:                Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;  10
# 2:                                             Baseline # of AE < 10;   3
# 3:                                       Baseline MFR < 0.6377603 Hz;  11
# 4:                                                        rval is NA;   1
# 5: Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; Contamination;   1
# 6:                                                         Mis-dosed;   1

dat4[wllq == 0 & rowi == 6 & coli == 1 & grepl("LDH",acnm) & is.na(rval)]
# this is from the plate with missing LDH data from APCRA, all good here
# 20190403_Calculations_Group_1_missingData(LDH).xlsx

# where misdosed or contaminated - can I confirm that a diff well was used for the total lysis here?
dat4[wllq == 0 & rowi == 6 & coli == 1 & (grepl("LDH",acnm) | grepl("LDH",acsn)) & grepl("(Mis-dosed)|(Contamination)",wllq_notes)]
# treatment       spid experiment.date  plate.id     apid rowi coli conc acnm wllt wllq                                                          wllq_notes      rval
# 1:     Media      Media        20190530 MW68-0807 20190530    6    1   10 <NA>    b    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; Contamination;  0.7799667
# 2:     LYSIS Tritonx100        20160531 MW1048-15 20160531    6    1  100 <NA>    p    0                                                         Mis-dosed;  2.7071333
# srcf                              dat3      origin                 acsn acid
# 1: 20190515_Calculations_DNT Group_2.xlsx     DNT2019_dat3_2020-06-18.RData     DNT2019 NHEERL_MEA_acute_LDH 2495
# 2:              Conc_Response_Log_JS.xlsx ToxCast2016_dat3_2020-06-19.RData ToxCast2016 NHEERL_MEA_acute_LDH 2495
# for LYSIS well in ToxCast data, there are no extra Lysis wells, so nothing to worry about there
# For the Media well in DNT data, I know that a different well was used for the lysis data, so this is not a prob

# 24 other cases...
dat4[(grepl("LDH",acnm) | grepl("LDH",acsn)) & grepl("(Baseline MFR <)|(# of AE <)",wllq_notes) & rowi == 6 & coli == 1, .N, by = "origin"]
# origin  N
# 1:   APCRA2019 10
# 2:     DNT2019  1
# 3: ToxCast2016 14
# Toxcast is no matter, so there are just 10 cases of to consider

# these cases:
# treatment       spid experiment.date  plate.id     apid rowi coli conc                      acnm wllt wllq
# 1:     Lysis Tritonx100        20190328 MW1236-20 20190328    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 2:     Lysis Tritonx100        20190402 MW1236-22 20190402    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 3:     Lysis Tritonx100        20190402 MW1236-23 20190402    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 4:     Lysis Tritonx100        20190411 MW1237-18 20190411    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 5:     Lysis Tritonx100        20190418 MW66-9803 20190418    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 6:     Lysis Tritonx100        20190423 MW66-9805 20190423    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 7:     Lysis Tritonx100        20190423 MW66-9810 20190423    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 8:     Lysis Tritonx100        20190423 MW66-9811 20190423    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 9:     Lysis Tritonx100        20190425 MW66-9812 20190425    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
# 10:     Lysis Tritonx100        20190507 MW67-3712 20190507    6    1    1 CCTE_Shafer_MEA_acute_LDH    x    0
dat4[experiment.date == "20190328" & rowi == 6 & coli == 1 & grepl("LDH",acnm)]
check_dates <- dat4[(grepl("LDH",acnm) | grepl("LDH",acsn)) & grepl("(Baseline MFR <)|(# of AE <)",wllq_notes) & 
                       !grepl("(Contamination)|(Mis-dosed)",wllq_notes) & rowi == 6 & coli == 1 & origin != "ToxCast2016", unique(experiment.date)]

# plot it
stripchart(rval ~ apid, dat4[experiment.date %in% check_dates & (grepl("LDH",acnm) | grepl("LDH",acsn)) & rowi == 6 & coli == 1 & wllq == 1], vertical = T, pch = 1,
           cex.axis = 0.8, las = 2)
stripchart(rval ~ apid, dat4[experiment.date %in% check_dates & (grepl("LDH",acnm) | grepl("LDH",acsn)) & rowi == 6 & coli == 1 & wllq == 0], vertical = T, pch = 1,
           col = "red", cex = 1.5, add = T)
legend(x = "topright", bg = "transparent", legend = c("wllq=0"), col = "red", pch = 1, cex = 1.5)
# next: check out the Row H lysis wells (or p wells) to see if these are affected



# # let's first just compare these lysis wells in the main plate (not those in Row H)
# dat4$apid <- factor(dat4$apid, levels = sort(unique(as.numeric(dat4$apid))), ordered = T)
# stripchart(rval ~ apid, dat4[(grepl("LDH",acnm) | grepl("LDH",acsn)) & grepl("(Lysis)|(LYSIS)",treatment) & rowi < 7 & wllq == 1], vertical = T, pch = 1,
#           cex.axis = 0.8, las = 2)
# stripchart(rval ~ apid, dat4[(grepl("LDH",acnm) | grepl("LDH",acsn)) & grepl("(Lysis)|(LYSIS)",treatment) & rowi < 7 & wllq == 0], vertical = T, pch = 1,
#            col = "red", cex = 1.5, add = T)
# legend(x = "topright", bg = "transparent", legend = c("wllq=0"), col = "red", pch = 1, cex = 1.5)
# # there are a few instances where the red circles seem to be significantly lower
# # but overall, not too different
# 
# # what this with no lysis values afer a certain date?
# dat4[(grepl("LDH",acnm) | grepl("LDH",acsn)) & grepl("(Lysis)|(LYSIS)",treatment) & apid > 20190528, unique(rowi)]
# # oh, right... all of these are labelled "Media"...


# 07/27/2020-----------------------------------------------------------------------
# confirming that no lysis wells have wllq=0 bc of contamination reasons
dat4 <- get_latest_dat("dat4")
# Getting data from folders APCRA2019, DNT2019, GF2019, ToxCast2016 
# APCRA2019_dat4_2020-07-21.RData 
# DNT2019_dat4_2020-07-22.RData 
# GF2019_dat4_2020-07-21.RData 
# ToxCast2016_dat4_2020-07-23.RData 

lysis_wells <- dat4[grepl("AB",acnm) & spid == "Tritonx100", .(experiment.date, plate.id, rowi, coli, wllq, wllq_notes, spid)]

# confirm there is exactly 1 Lysis well per plate
if(nrow(lysis_wells[, .N, by = c("experiment.date","plate.id")][N != 1]) > 0 ) {
  stop(paste0("The following plates do not have exactly 1 Lysis well:\n",lysis_wells[, .N, by = c("experiment.date","plate.id")][N != 1]))
}
# all clear

lysis_wells[, .N, by = "wllq_notes"]
# wllq_notes   N
# 1:                                                      269
# 2: Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;   10
# 3:                              Baseline # of AE < 10;    3
# 4:                        Baseline MFR < 0.6377603 Hz;   11
# 5:                                          Mis-dosed;    1
# 6:                                         rval is NA;    3

# no contaminations, that's good
# what is this "mis-dosed" well?
lysis_wells[wllq_notes == "Mis-dosed; "]
# as found above, this is from the toxcast data
# from the mea outliers and data check doc, said that cols 1and 2 were all misdose on this plate
# This is fine, because there are 2 other lysis wells from this apid, and there are no 1/2 lysis wells to worry about 
