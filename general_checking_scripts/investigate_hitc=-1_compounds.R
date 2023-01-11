library(data.table)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
source("mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R")

load("sbox_dat/sbox_dat_2020-06-10.RData")

# how many chem no res for mfr?
mc5_mc6[hitc == -1, .N, by = c("spid","chnm")]
# spid                   chnm N
# 1:          DMSO                   <NA> 4
# 2: EPAPLT0154A04    1,2-Diphenoxyethane 4
# 3: EPAPLT0154D01        Diphenolic acid 4
# 4: EPAPLT0154D07   Methyl phenylacetate 4
# 5: EPAPLT0154E01       Cadmium chloride 4
# 6: EPAPLT0154E02         1,4-Butanediol 4
# 7: EPAPLT0154H01 Trimethoxyphenylsilane 4
# 8:  TP0001411G12        Sodium benzoate 2
check_spids <- mc5_mc6[hitc == -1, setdiff(unique(spid),c("DMSO"))]

# see if I can determine the reasons for this
dat4 <- get_latest_dat()
dat4[spid %in% check_spids, .N, by =c("wllq","wllq_notes")]
dat4[spid %in% check_spids, paste0(sort(unique(wllq_notes)),collapse=","), by =c("rowi","coli")]
dat4[spid %in% check_spids, sort(unique(wllq_notes)), by =c("rowi","coli")]

dat4[spid %in% check_spids, .N, by =c("rowi","coli")]
# woah, only have data from 2 rows here!!

dat4[spid %in% check_spids, unique(experiment.date), by =c("spid")]
# spid       V1
# 1: EPAPLT0154A04 20190326
# 2: EPAPLT0154D01 20190411
# 3: EPAPLT0154D07 20190416
# 4: EPAPLT0154E01 20190418
# 5: EPAPLT0154E02 20190418
# 6: EPAPLT0154H01 20190507
# 7:  TP0001411G12 20150908

# is this data missing in dat1?
dat1 <- get_latest_dat(lvl = "dat1")
dat1[experiment.date == "20190326", .N, by = c("rowi","coli")]

dat4[spid == "EPAPLT0154A04" & wllq == 1, .N, by = c("rowi","coli","plate.id","experiment.date","origin")]
# rowi coli  plate.id experiment.date    origin  N
# 1:    4    2 MW1236-17        20190326 APCRA2019 17
# 2:    4    8 MW1236-17        20190326 APCRA2019 17
# 3:    4    8 MW1236-18        20190326 APCRA2019 17
# 4:    4    8 MW1236-16        20190326 APCRA2019 10
dat1[experiment.date == "20190326" & plate.id == "MW1236-17", .N, by= c("rowi","coli","origin")]
# all 48 rows are present, with 30 data points

dat2 <- get_latest_dat(lvl = "dat2")
dat2[experiment.date == "20190326" & plate.id == "MW1236-17", .N, by= c("rowi","coli","origin")]
# have 15 data rows for all 48 wells

dat3 <- get_latest_dat(lvl = "dat3")
dat3[experiment.date == "20190326" & plate.id == "MW1236-17", .N, by= c("rowi","coli","origin")]
# 17 endpoints for all 48 wells

# so something must have happend at level 4...
# wait, is this data present in dat4?
dat4[experiment.date == "20190326" & plate.id == "MW1236-17", .N, by= c("rowi","coli","origin")]
# yes, it is present

dat4[experiment.date == "20190326" & plate.id == "MW1236-17", unique(wllq), by= c("rowi","coli","origin")]

# this is the issue: - wait, of course we only have data from a few rows for a given spid!!
dat4[spid %in% check_spids & grepl("firing",acsn), sort(unique(wllq_notes)), by =c("rowi","coli","experiment.date","conc")][order(experiment.date)]
dat4[spid %in% check_spids & wllq == 1 & grepl("firing",acsn), .N, by =c("rowi","coli","experiment.date","conc")][order(experiment.date)]

for (si in check_spids) {
  cat("\n\n",si,sep="")
  cat("\nall conc's:",dat4[spid == si & grepl("firing",acsn), unique(conc)])
  cat("\nconc's with wllq=1:",dat4[spid == si & wllq == 1 & grepl("firing",acsn), unique(conc)],"\n")
  print(dat4[spid == si & grepl("firing",acsn), .N, by = c("wllq","wllq_notes")])
}

# okay, here we go:
# EPAPLT0154A04
# all conc's: 0.03 0.1 0.3 1 3 10 30
# conc's with wllq=1: 30 0.03 
# wllq                    wllq_notes  N
# 1:    0 Baseline MFR < 0.6377603 Hz;  17
# 2:    1                                4
# 
# 
# EPAPLT0154D01
# all conc's: 0.03 0.1 0.3 1 3 10 30
# conc's with wllq=1: 1 10 30 
# wllq                    wllq_notes  N
# 1:    0 Baseline MFR < 0.6377603 Hz;  15
# 2:    1                                6
# 
# 
# EPAPLT0154D07
# all conc's: 0.03 0.1 0.3 1 3 10 30
# conc's with wllq=1: 1 30 0.1 
# wllq                                                       wllq_notes  N
# 1:    0                                    Baseline MFR < 0.6377603 Hz;  15
# 2:    1                                                                   4
# 3:    0                     Baseline MFR < 0.6377603 Hz; Contamination;   1
# 4:    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; rval is NA;   1
# 
# 
# EPAPLT0154E01
# all conc's: 0.03 0.1 0.3 1 3 10 30
# conc's with wllq=1: 30 0.03 1 
# wllq                    wllq_notes  N
# 1:    0 Baseline MFR < 0.6377603 Hz;  15
# 2:    1                                6
# 
# 
# EPAPLT0154E02
# all conc's: 0.03 0.1 0.3 1 3 10 30
# conc's with wllq=1: 30 1 0.03 
# wllq                    wllq_notes  N
# 1:    0 Baseline MFR < 0.6377603 Hz;  15
# 2:    1                                6
# 
# 
# EPAPLT0154H01
# all conc's: 0.03 0.1 0.3 1 3 10 30
# conc's with wllq=1: 0.03 30 0.1 
# wllq                                           wllq_notes  N
# 1:    1                                                       7
# 2:    0                        Baseline MFR < 0.6377603 Hz;  13
# 3:    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;   1
# 
# 
# TP0001411G12
# all conc's: 40 10 3 1 0.3 0.1 0.03
# conc's with wllq=1: 3 
# wllq                                                                            wllq_notes  N
# 1:    0                                                         Baseline MFR > 3.4036511 Hz;  11
# 2:    1                                                                                        2
# 3:    0                                                         Baseline MFR < 0.6377603 Hz;   6
# 4:    0 # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz; rval is NA;   2

# for all of these, I understand now why they were removed.
