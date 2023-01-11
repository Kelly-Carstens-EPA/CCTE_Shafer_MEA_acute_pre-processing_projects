# starting to assess the changes in mean firing rate in DMSO wells
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/lvl0_snapshots")
library(data.table)

load("dat4_2020-06-22.RData")

# questions:
# - why are there only 3 DMSO wells on this apid?
# - why are the 3 DMSO Wells so low? did i label these incorrectly?

# Q1 - are there really only 3 DMSO wells?
dat4[apid == "20190326" & wllt == "n", .N, by = c("plate.id","acsn")]
# exactly 1 DMSO well is present for each acsn (inlcuding LDH and AB) on each of the 3 plates

# are there still 48 wells on each plate?
dat4[apid == "20190326", .N, by = c("plate.id","acsn")]
# yep, all have 48 wells, with LDH at 54 = 48 + 6

# what are the other wells that usually have DMSO labelled as?
# MFR, as representative of all mea endpoints
dat4[apid == "20190326" & coli == 1 & grepl("firing",acsn), .N, by = c("plate.id","wllt","treatment","rowi")][order(plate.id, rowi)]
# plate.id wllt treatment rowi N
# 1: MW1236-16    b     Media    1 1
# 2: MW1236-16    b     Media    2 1
# 3: MW1236-16    n      DMSO    3 1
# 4: MW1236-16    b     Media    4 1
# 5: MW1236-16    b     Media    5 1
# 6: MW1236-16    b     Media    6 1
# 7: MW1236-17    b     Media    1 1
# 8: MW1236-17    b     Media    2 1
# 9: MW1236-17    n      DMSO    3 1
# 10: MW1236-17    b     Media    4 1
# 11: MW1236-17    b     Media    5 1
# 12: MW1236-17    b     Media    6 1
# 13: MW1236-18    b     Media    1 1
# 14: MW1236-18    b     Media    2 1
# 15: MW1236-18    n      DMSO    3 1
# 16: MW1236-18    b     Media    4 1
# 17: MW1236-18    b     Media    5 1
# 18: MW1236-18    b     Media    6 1
# so I only labelled every well other than C1 as "Media"
# just looked at the caluclations file L:\Lab\NHEERL_MEA\Project TSCA_APCRA\20190313 Culture\20190313_Calculations_Group_1_checked.xlsx,
# and indeed, only well C3 is labelled as DMSO. The rest are TTX or PICRO, which were added after the second recording.
# Wow.
# i'm concnered that there is just such a negative response in all three of these wells.
# I am started to question whether the Calc file is correct?
# Just checked the lab notebook, and there is no other data here

# do i have reason to believe that there was a mix-up with the labelling of TTX/other wells?
stripchart(rval ~ treatment, dat4[apid == "20190326" & wllt != "t" & grepl("firing",acsn)], vertical = T, method = "jitter", pch = 1)
stripchart(rval ~ rowi, dat4[apid == "20190326" & wllt != "t" & grepl("firing",acsn)], vertical = T, method = "jitter", pch = 1, 
           group.names = c("TTX5","PICRO5","DMSO","TTX3","PICRO3","LYSIS"), xlab = "col1 row labels in calculations file")
# hmm, so the LYSIS def looks like it has not been added yet
# it is interesting that the TTX wells are high-ish, adn the PICRO wells are on the low-end.
# wow, I would not expect that with just media added!!

# how do the ranges of these values compare to clear media/dmso/ttx/picro val's from other plates?
par(mfrow = c(1,2))
stripchart(rval ~ treatment, dat4[apid != "20190326" & wllq == 1 & origin == "APCRA2019" & wllt != "t" & grepl("firing",acsn)], vertical = T, method = "jitter", pch = 1)
title(main = "% change MFR in Control wells\nAll APCRA Plates")
stripchart(rval ~ rowi, dat4[apid == "20190326" & wllq == 1 & wllt != "t" & grepl("firing",acsn)], vertical = T, method = "jitter", pch = 1, 
           group.names = c("TTX5","PICRO5","DMSO","TTX3","PICRO3","LYSIS"), xlab = "col1 row labels in calculations file", ylim = range(dat4[origin == "APCRA2019" & wllt != "t" & wllq == 1 & grepl("firing",acsn),rval]))
title(main = "% change MFR in Control wells\n20190326")
# compared to the rest of the data from apcra:
# - I wanted to think that the TTX and PICRO labels might be mixed up. But the PICRO values from 20190326 are def not low enough to be 
# I think it is reasonable that all of the TTX and PICRO might have been Media. (Honestly don't know if Media or DMSO  - I think that is lost in history)
# - yes the DMSO wells are low, but their are plenty of other DMSO Wells that are that low. This value is not enough evidence alone to question the treatment label

# cytotox endpoints:

