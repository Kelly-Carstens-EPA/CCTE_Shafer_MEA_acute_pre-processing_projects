# checking out resp at cndx 1 and 2 again
library(data.table)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")

# loading most recent run in sbox
load("sbox_dat/sbox_dat_2020-06-21.RData")
# note this is for only about 5 acid's

# aeid summary
mc5_mc6[, unique(aenm), by = "aeid"]
# aeid                                   V1
# 1: 2425 NHEERL_MEA_acute_firing_rate_mean_up
# 2: 2426     NHEERL_MEA_acute_burst_number_up
# 3: 2438  NHEERL_MEA_acute_synchrony_index_up
# 4: 2439              NHEERL_MEA_acute_LDH_up
# 5: 2440               NHEERL_MEA_acute_AB_up
# 6: 2442 NHEERL_MEA_acute_firing_rate_mean_dn
# 7: 2443     NHEERL_MEA_acute_burst_number_dn
# 8: 2455  NHEERL_MEA_acute_synchrony_index_dn

# let's see what bval "would have been" with check "n" wells
mc3[is.na(cval)] # empty
bval.nwells.table <- mc3[wllt == "n", .(bval.nwells = median(cval)), by = c("apid","aeid")]
mc3 <- merge(mc3, bval.nwells.table, all.x = T, by = c("apid","aeid"))
mc3[, diff.bval := bval - bval.nwells]
mc3[wllt == "n", cndx := 0] # for graphing/visualization below

# MFR
ae <- 2425

# Alamar Blue - 
ae <- 2440
# where there are big differences, is that just noise, or where potent chem tested on those plates
# and caused a major resp adn cndx 1 adn2?
mc3[aeid == ae, summary(diff.bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -4914.3  -780.5   177.7   424.9  1738.8  6153.0
# pos - bval with cndx1&2 is higher
# neg - bval with cndx 1&2 is lower (i.e, )

largest.dec.apid <- mc3[aeid == ae, .(apid = unique(apid)), by = "diff.bval"][order(diff.bval)][1:16, apid]
yrange <- mc3[aeid == ae & cndx %in% c(0:7), range(cval)]

par(mfrow = c(3,3))
for (plate in largest.dec.apid[1:9]) {
  # boxplot(cval ~ cndx, mc3[aeid == ae & wllt %in% c("n","t") & apid == plate], ylim = yrange)
  stripchart(cval ~ cndx, mc3[aeid == ae & wllt %in% c("n","t") & apid == plate], ylim = yrange, vertical=T, method="jitter", pch=1)
  abline(h = mc3[aeid == ae & apid == plate, unique(bval.nwells)], col = "red")
  abline(h = mc3[aeid == ae & apid == plate, unique(bval)], col = "blue")
}

# comment 1: 
# the plate to plate variability is much greater than dmso to twolow variability

diff.spids <- mc3[apid == largest.dec.apid[2], unique(spid)]
mc5_mc6[spid %in% diff.spids & aeid == ae, .(hitc)]


# up resp
largest.inc.apid <- mc3[aeid == ae, .(apid = unique(apid)), by = "diff.bval"][order(-diff.bval)][1:16, apid]
yrange <- mc3[aeid == ae & cndx %in% c(0:7), range(cval)]

par(mfrow = c(3,3))
for (plate in largest.inc.apid[1:9]) {
  boxplot(cval ~ cndx, mc3[aeid == ae & wllt %in% c("n","t") & apid == plate], ylim = yrange)
  abline(h = mc3[aeid == ae & apid == plate, unique(bval.nwells)], col = "red")
  abline(h = mc3[aeid == ae & apid == plate, unique(bval)], col = "blue")
}

# let's just break down everything...
diff.summary <- mc3[aeid == ae, .(diff.bval = unique(diff.bval)), by = "apid"]
diff.summary[diff.bval > 0, .N] # 52
diff.summary[diff.bval < 0, .N] # 42
diff.summary[diff.bval == 0, .N] # 3 no dif!

# it seems that the magnitude of diff for inc in bval is greater than the other direction... let's verify
diff.summary[diff.bval > 0, summary(diff.bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 78.67  774.83 1423.50 1804.16 2460.08 6153.00 
diff.summary[diff.bval < 0, summary(-diff.bval)]
boxplot(diff.summary$diff.bval)

# check out resp at cndx 1 and 2

# summary of 
mc3[wllt == "n", cndx := 0]
boxplot(resp ~ cndx, mc3[aeid == 2425 & wllt %in% c("n","t")])
# first check out mfr endpoints

# big idea:
# let's trick tcpl into testing the cndx 1 thing!!
# add additional wells with cndx = 1
cndx1.twells <- mc3[cndx == 1 & wllt == "t"]
cndx1.twells[, wllt := "n"]
setnames(cndx1.twells, old = "cval", new = "rval")
# merge with lvl 0 to get all the right id stuff
# rbind to lvl 0, then re-pipeline!
