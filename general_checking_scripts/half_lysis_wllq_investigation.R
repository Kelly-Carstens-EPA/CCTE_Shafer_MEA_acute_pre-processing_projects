# checking out 1/2 Lysis wells dervied from lysis wells with wllq==0
library(data.table)
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R')

dat4 <- get_latest_dat("dat4")

# isolate the wells used for Lysis
dat4[grepl("AB",acnm) & spid == "Tritonx100", .N, by = "treatment"]
# treatment   N
# 1:       Lysis  84
# 2: PICRO/Lysis   2
# 3:   TTX/Lysis   4
# 4:       LYSIS 207
# looks right...

# 1 per plate.id?
dat4[grepl("AB",acnm) & spid == "Tritonx100", .N, by = .(experiment.date, plate.id)]
# most look good
dat4[grepl("AB",acnm) & spid == "Tritonx100", .N, by = .(experiment.date, plate.id)][N != 1]
# Empty data.table (0 rows and 3 cols): experiment.date,plate.id,N
# sweet!

lysis_wells <- dat4[grepl("AB",acnm) & spid == "Tritonx100", .(experiment.date, plate.id, rowi, coli, wllq, wllq_notes, treatment, spid, origin)]
lysis_wells[, .N, by = c("wllq","origin")]
# wllq      origin   N
# 1:    1   APCRA2019  38
# 2:    0   APCRA2019  10
# 3:    1     DNT2019  39
# 4:    1      GF2019   3
# 5:    1 ToxCast2016 189
# 6:    0 ToxCast2016  18

# I dont' care about the TC wells so much, because there are no 1/2 lysis wells derived form these
# So, there are only 10 lysis wells from APCRA that may be a concern.

lysis_wells[wllq == 0 & origin != "ToxCast2016"]
#     experiment.date  plate.id rowi coli wllq                                           wllq_notes treatment    origin
# 1:        20190328 MW1236-20    6    1    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019
# 2:        20190402 MW1236-22    6    1    0                              Baseline # of AE < 10;      Lysis APCRA2019
# 3:        20190402 MW1236-23    6    1    0                              Baseline # of AE < 10;      Lysis APCRA2019
# 4:        20190411 MW1237-18    6    1    0                        Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019
# 5:        20190418 MW66-9803    6    1    0                        Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019
# 6:        20190423 MW66-9805    6    1    0                        Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019
# 7:        20190423 MW66-9810    6    1    0                        Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019
# 8:        20190423 MW66-9811    6    1    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019
# 9:        20190425 MW66-9812    6    1    0                        Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019
# 10:        20190507 MW67-3712    6    1    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;      Lysis APCRA2019

# let's confirm that the LDH values are even included for these plates
plates <- lysis_wells[wllq == 0 & origin != "ToxCast2016", unique(plate.id)]
dates <- lysis_wells[wllq == 0 & origin != "ToxCast2016", unique(experiment.date)]
ldat <- dat4[plate.id %in% plates & grepl("LDH",acnm)]

ldat <- merge(dat4[grepl("LDH",acnm)], lysis_wells[wllq == 0 & origin != "ToxCast2016", .(experiment.date, plate.id, spid, origin, wllq, wllq_notes)], by = c("experiment.date","plate.id","spid","origin"),
      suffixes = c("","_flag"), all = T)
ldat[treatment == "2 * ½ Lysis", .N, by = "wllq_flag"]

ldat$apid <- factor(ldat$apid, levels = sort(unique(ldat$apid)), ordered = T)
stripchart(rval ~ apid, ldat[wllq == 1 & is.na(wllq_flag) & treatment == "2 * ½ Lysis"], vertical = T, pch = 1, las = 2, cex.axis = 0.65)
stripchart(rval ~ apid, ldat[wllq == 1 & wllq_flag == 0 & treatment == "2 * ½ Lysis"], vertical = T, pch = 1, cex = 1.25, col = "red", add = T)
stripchart(med_rval ~ apid, ldat[wllq == 1 & treatment == "2 * ½ Lysis", .(med_rval = median(rval)), by = "apid"], vertical = T, pch = 19, col = "red", add = T)
stripchart(med_rval ~ apid, ldat[wllq == 1 & is.na(wllq_flag) & treatment == "2 * ½ Lysis", .(med_rval = median(rval)), by = "apid"], vertical = T, pch = 19, add = T)
legend(x = "topleft", legend = c("2 * ½ Lysis, MEA Lysis wllq=1","2 * ½ Lysis, MEA Lysis wllq=0","Median of all 2 * ½ Lysis on apid","Median of only black points"),
       pch = c(1,1,19,19), col = c("black","red","red","black"))
title("2 * ½ Lysis Wells Wllq Analysis")
