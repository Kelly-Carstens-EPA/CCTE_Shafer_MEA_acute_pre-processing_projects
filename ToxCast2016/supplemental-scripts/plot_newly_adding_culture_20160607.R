# get graphs of the newly added culture dat, so show Kathleen
# to verify is safe to add

setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")
load("output/ToxCast2016_dat4_2020-06-10.RData")

dat4[apid == "20160607" & grepl("firing",acsn), .N, by = c("wllq","wllq_notes")]
# wllq                                                                wllq_notes  N
# 1:    1                                                                           94
# 2:    0                                             Baseline MFR < 0.6377603 Hz;  24
# 3:    0 # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz;  23
# 4:    0                              # of AE less than 10 in baseline recording;   2
# 5:    0                                             Baseline MFR > 3.4036511 Hz;   1

plot(dat4[apid == "20160607" & grepl("firing",acsn) & wllq == 1, .(log10(conc), rval)])
stripchart(rval ~ conc, dat4[apid == "20160607" & grepl("firing",acsn) & wllq == 1], vertical = T, pch = 1, method = "jitter")
title(main = "Percent Change in Mean Firing Rate in 20160607 by conc where wllq=1")

# another culutre, for comparison
stripchart(rval ~ conc, dat4[apid == "20160609" & grepl("firing",acsn) & wllq == 1], vertical = T, pch = 1, method = "jitter")
title(main = "Percent Change in Mean Firing Rate in 20160609 by conc where wllq=1")
dat4[apid == "20160609" & grepl("firing",acsn), .N, by = c("wllq","wllq_notes")]
# wllq                                                                            wllq_notes   N
# 1:    1                                                                                       129
# 2:    0                                                         Baseline MFR < 0.6377603 Hz;    7
# 3:    0             # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz;    5
# 4:    0 # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz; rval is NA;    1
# 5:    0                                          # of AE less than 10 in baseline recording;    1
# 6:    0                                                         Baseline MFR > 3.4036511 Hz;    1

# checkout these values:
dat4[grepl("LDH",acsn) & experiment.date == "20151222" & plate.id == "MW1060-36" & rowi == "5" & coli == "3"]
# rval is 72% dead?? blankc-corrected value is just quite large
dat4[grepl("LDH",acsn) & experiment.date == "20151222" & plate.id == "MW1060-36" & rowi == "6" & coli == "3"]
# rval is 57%!!