filei <- "L:/Lab/NHEERL_MEA/Project TSCA_APCRA/20190320 Culture/G1/TC_20190320_MW1236-24_13_00(000)(000).csv"

longdat <- fileToLongdat(filei, run.type.tag.location)
longdat[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean", .N, by = c("wllq","wllq_notes")]
# wllq                                           wllq_notes  N
# 1:    1                                                      32
# 2:    0                        Baseline MFR > 3.4036511 Hz;   2
# 3:    0                              Baseline # of AE < 10;   3
# 4:    0                        Baseline MFR < 0.6377603 Hz;   4
# 5:    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;   7
# okay, so if I included the baseline recording, 
# 14 of the 48 wells would be removed from the cytodat for this plate (~30% of wells)

longdat[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & wllq == 0, .N, by = c("coli")][order(coli)]
longdat[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & wllq == 0 & grepl("Baseline MFR < ",wllq_notes), .(acsn, well, activity_value, wllq_notes)]
# acsn well activity_value                                           wllq_notes
# 1: Mean Firing Rate (Hz)   C1       0.687031                              Baseline # of AE < 10; 
# 2: Mean Firing Rate (Hz)   C2       0.982997                              Baseline # of AE < 10; 
# 3: Mean Firing Rate (Hz)   D2       0.461077 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 4: Mean Firing Rate (Hz)   D3       0.003042 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 5: Mean Firing Rate (Hz)   D4       0.230538 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 6: Mean Firing Rate (Hz)   D5       0.191490 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 7: Mean Firing Rate (Hz)   D7       0.228371 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 8: Mean Firing Rate (Hz)   E1       0.199700 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 9: Mean Firing Rate (Hz)   E2       0.357560 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 10: Mean Firing Rate (Hz)   E7       0.996624                              Baseline # of AE < 10; 
longdat[acnm == "CCTE_Shafer_MEA_acute_active_electrodes_number" & wllq == 0 & grepl("Baseline # of AE < ",wllq_notes), .(acsn, well, activity_value, wllq_notes)]
# acsn well activity_value                                           wllq_notes
# 1: Number of Active Electrodes   C1              9                              Baseline # of AE < 10; 
# 2: Number of Active Electrodes   C2              9                              Baseline # of AE < 10; 
# 3: Number of Active Electrodes   D2              9 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 4: Number of Active Electrodes   D3              0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 5: Number of Active Electrodes   D4              7 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 6: Number of Active Electrodes   D5              4 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 7: Number of Active Electrodes   D7              9 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 8: Number of Active Electrodes   E1              7 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 9: Number of Active Electrodes   E2              8 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; 
# 10: Number of Active Electrodes   E7              8                              Baseline # of AE < 10; 

# so most of these are not borderline
# I guess the question is whether we can trust these values, even though short recording
# I don't thin that woudl be unreasonable

# summaries of the entire data set
dat4[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & grepl("(Baseline MFR <)|(# of AE <)",wllq_notes), .N, by = c("experiment.date","plate.id")][order(experiment.date)]
# experiment.date  plate.id  N
# 1:        20190326 MW1236-16 11
# 2:        20190326 MW1236-17  9
# 3:        20190326 MW1236-18 12
# 4:        20190328 MW1236-19 16
# 5:        20190328 MW1236-20 20
# 6:        20190328 MW1236-21  3
# 7:        20190402 MW1236-23 10
# 8:        20190402 MW1236-22 15
# 9:        20190404 MW1237-12  3
# 10:        20190404 MW1237-10  3
# 11:        20190404 MW1237-11  4
# 12:        20190409 MW1237-13 10
# 13:        20190409 MW1237-14  9
# 14:        20190409 MW1237-15  7
# 15:        20190411 MW1237-16 19
# 16:        20190411 MW1237-18 16
# 17:        20190411 MW1237-17 12
# 18:        20190416 MW66-9604 16
# 19:        20190416 MW1237-19  7
# 20:        20190416 MW66-9613  6
# 21:        20190418 MW66-9801 17
# 22:        20190418 MW66-9803 10
# 23:        20190418 MW66-9804 11
# 24:        20190423 MW66-9811  5
# 25:        20190423 MW66-9805 11
# 26:        20190423 MW66-9810  9
# 27:        20190425 MW66-9812 11
# 28:        20190425 MW66-9817  9
# 29:        20190425 MW66-9818  3
# 30:        20190430 MW67-3701  1
# 31:        20190430 MW67-3702  1
# 32:        20190502 MW67-3708  2
# 33:        20190502 MW67-3707  1
# 34:        20190502 MW67-3709  3
# 35:        20190507 MW67-3710 10
# 36:        20190507 MW67-3711  9
# 37:        20190507 MW67-3712 11
# 38:        20190509 MW67-3716  5
# 39:        20190509 MW67-3714  7
# 40:        20190509 MW67-3715  6
# 41:        20190514 MW67-3718  2
# 42:        20190514 MW67-3719  2
# 43:        20190514 MW68-0701  6
# 44:        20190516 MW68-0703  6
# 45:        20190516 MW68-0704  2
# 46:        20190516 MW68-0719  5
dat4[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & grepl("(Baseline MFR <)|(# of AE <)",wllq_notes), .N, by = c("experiment.date","plate.id")][order(experiment.date)][, summary(N)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.250   8.000   8.109  11.000  20.000 

stripchart(N ~ experiment.date + plate.id, dat4[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & grepl("(Baseline MFR <)|(# of AE <)",wllq_notes), .N, by = c("experiment.date","plate.id")],
           vertical = T, pch = 1, las = 2, cex.axis = 0.5)
abline(h = 14)

# quick look at how the spike rate changes over time
library(data.table)
bdat_file <- "L:/Lab/NHEERL_MEA/Project TSCA_APCRA/20190320 Culture/1236-22/TC_20190320_MW1236-22_13_00(000)_spike_counts.csv"
tdat_file <- "L:/Lab/NHEERL_MEA/Project TSCA_APCRA/20190320 Culture/1236-22/TC_20190320_MW1236-22_13_01(000)_spike_counts.csv"
bdat <- read.csv(bdat_file)
setDT(bdat)
bdat <- bdat[1:(nrow(bdat)-11)] # getting rid of summary info at the end
bdat[, c(grep("[A-F][1-8]",names(bdat), value = T)) := lapply(.SD, as.numeric), .SDcols = grep("[A-F][1-8]",names(bdat), value = T)]

bdat$Interval.End..S. <- as.numeric(bdat$Interval.End..S.)
bdat[, minute := ceiling(Interval.End..S. / 60)]

# summary by minute
bdat_min <- bdat[, lapply(.SD, sum), by = "minute", .SDcols = grep("^[A-F][1-8]$",names(bdat), value = T)]

# plot this data
plot(A1 ~ minute, bdat_min, type = "l")
plot(B1 ~ minute, bdat_min, type = "l")
plot(C1 ~ minute, bdat_min, type = "l")

# why is the last interval so low?
bdat[minute == 60, unique(Interval.End..S. %% 60)] # huh, so it looks like minute 60 does span a full minute
bdat[1:40, 1:3] # huh, I really don't know

# check out the treated data
tdat <- read.csv(tdat_file)
setDT(tdat)
tdat <- tdat[1:(nrow(tdat)-11)] # getting rid of summary info at the end
tdat[, c(grep("[A-F][1-8]",names(tdat), value = T)) := lapply(.SD, as.numeric), .SDcols = grep("[A-F][1-8]",names(tdat), value = T)]

tdat$Interval.End..S. <- as.numeric(tdat$Interval.End..S.)
tdat[, minute := ceiling(Interval.End..S. / 60)]

# summary by minute
tdat_min <- tdat[, lapply(.SD, sum), by = "minute", .SDcols = grep("^[A-F][1-8]$",names(tdat), value = T)]

plot(A1 ~ minute, tdat_min, type = "l")

# summary by 10 seconds
tdat[, sec10 := round(Interval.End..S. / 10, 1)]
tdat_10 <- tdat[, lapply(.SD, sum), by = "sec10", .SDcols = grep("^[A-F][1-8]$",names(tdat), value = T)]
plot(A1 ~ sec10, tdat_10, type = "l")
# huh, cool. So we are looking at some very patterned spiking here.
abline(h = tdat_10[, min(A1)])

# okay, what we want to know is - does the mean firing rate change over time?

# make a matrix that sums up all of the spikes before it
make_vector_cumulative <- function(x) {
  for (i in 2:length(x)) {
    x[i] <- x[i-1] + x[i]
  }
  x
}
tdat_cum <- tdat[, lapply(.SD, make_vector_cumulative), .SDcols = grep("^[A-F][1-8]$",names(tdat), value = T)]
tdat_cum[, c("start","end") := list(tdat$Interval.Start..S., tdat$Interval.End..S.)]
tdat_mfr <- tdat_cum[, lapply(.SD, function(x) x/tdat_cum$end), .SDcols = grep("^[A-F][1-8]$",names(tdat), value = T)]
tdat_mfr[, c("start","end") := list(tdat$Interval.Start..S., tdat$Interval.End..S.)]

plot(A1 ~ end, tdat_mfr)
abline(v = 25*60)
# okay, this is informative! early on, the mfr is highly dependent on what's happening in a well at a given moment
# then teh values smooth out over time

plot(A8 ~ end, tdat_mfr)
abline(v = 25*60) # 1500
# wow-sauce, this is interesting

# for every well, what is the mean difference in mfr at 25 min and at 2400 sec?
tdat_mfr[, .(A1[end == 1500] - A1[end == 2400])]
tdat_mfr_per_change <- tdat_mfr[, lapply(.SD, function(x) (x - x[tdat_mfr$end == 2400])/x[tdat_mfr$end == 2400]), .SDcols = setdiff(names(tdat_mfr),c("start","end"))]
tdat_mfr_per_change[, c("start","end") := list(tdat$Interval.Start..S., tdat$Interval.End..S.)]

# at what time are most mfr's within 10% of the value at 2400s?
plot(A1 ~ end, tdat_mfr_per_change)

# these are the last seconds when the mfr is more than 10% away from teh value at 2400 seconds
tdat_mfr_per_change[, lapply(.SD, function(x) max(which(abs(x[1:2400]) > 0.1))), .SDcols = setdiff(names(tdat_mfr),c("start","end"))]
# A1   A2   A3   A4  A5   A6   A7   A8   B1   B2   B3   B4   B5   B6   B7   B8   C1   C2  C3   C4   C5   C6   C7   C8   D1   D2 D3   D4   D5   D6   D7
# 1: 784 2249 1513 2120 876 1980 1824 2041 2089 1692 2028 1771 1912 2029 2049 2142 1749 2054 898 1811 2058 1726 1957 1991 2181 2069  6 1985 1628 1719 2251
# D8   E1   E2   E3   E4   E5   E6   E7   E8  F1  F2   F3  F4   F5   F6   F7   F8
# 1: 2250 1807 1958 2085 2314 1638 1908 1964 2143 830 749 1493 391 1895 1676 1870 2154
# huh, some of these values are quite large. Most are over 1500 seconds

plot_stuff <- function(well) {
  par(mfrow = c(2,1))
  plot(get(well) ~ end, tdat_mfr, main = well, ylab = "cumulative Mean Firing Rate (Hz)")
  abline(v = 2400, lty = "dashed")
  abline(h = c(0.9*tdat_mfr[2400,..well], 1.1*tdat_mfr[2400,..well]))
  plot(get(well) ~ sec10, tdat_10, type = "l", ylab = "spikes per 10 sec interval")
}
well <- "A4"
plot_stuff(well)

# E8 is interesting - MFR is just steadily decreasing over time
# A2 - there is virtually no activity until just afere 1500 seconds. Then MFR steadily increases
# A3 - similar to A2, this one has virtually no activity until around 700 seconds, then steadily increases. MFR at 1500 sec is nearly within 10% of final, but still another bad example
# A4 - basically same as A1


# okay, wait, why am I looking at treated wells? I need to check this out for baseline, that's all that I really care about!
bdat[, sec10 := round(Interval.End..S. / 10, 1)]
bdat_10 <- bdat[, lapply(.SD, sum), by = "sec10", .SDcols = grep("^[A-F][1-8]$",names(bdat), value = T)]

# make a matrix that sums up all of the spikes before it
bdat_cum <- bdat[, lapply(.SD, make_vector_cumulative), .SDcols = grep("^[A-F][1-8]$",names(bdat), value = T)]
bdat_cum[, c("start","end") := list(bdat$Interval.Start..S., bdat$Interval.End..S.)]
bdat_mfr <- bdat_cum[, lapply(.SD, function(x) x/bdat_cum$end), .SDcols = grep("^[A-F][1-8]$",names(bdat), value = T)]
bdat_mfr[, c("start","end") := list(bdat$Interval.Start..S., bdat$Interval.End..S.)]

# for every well, what is the mean difference in mfr at 25 min and at 2400 sec?
bdat_mfr_per_change <- bdat_mfr[, lapply(.SD, function(x) (x - x[bdat_mfr$end == 2400.75])/x[bdat_mfr$end == 2400.75]), .SDcols = setdiff(names(bdat_mfr),c("start","end"))]
bdat_mfr_per_change[, c("start","end") := list(bdat$Interval.Start..S., bdat$Interval.End..S.)]

# these are the last seconds when the mfr is more than 10% away from teh value at 2400 seconds
bdat_mfr_per_change[, lapply(.SD, function(x) max(which(abs(x[1:2400]) > 0.1))), .SDcols = setdiff(names(bdat_mfr),c("start","end"))]
# A1   A2   A3   A4   A5   A6  A7   A8   B1   B2   B3   B4   B5   B6   B7   B8   C1   C2   C3   C4   C5   C6   C7   C8   D1   D2   D3   D4   D5   D6
# 1: 1367 1803 1491 1725 1714 1479 889 1842 1633 1068 1780 1291 1232 1671 1753 1725 1713 1883 1441 1861 2094 2047 2010 1665 1772 1893 2084 2079 2043 2098
# D7   D8   E1   E2   E3   E4   E5   E6   E7   E8   F1   F2  F3   F4   F5   F6   F7   F8
# 1: 2014 1887 1560 1604 1800 1801 1785 1736 1672 1864 1693 1713 726 1569 1358 1658 1463 1761
# huh, some of these values are quite large. Most are over 1500 seconds

plot_stuff <- function(well) {
  par(mfrow = c(2,1))
  plot(get(well) ~ end, bdat_mfr, main = well, ylab = "cumulative Mean Firing Rate (Hz)")
  abline(v = 2400, lty = "dashed")
  abline(h = c(0.9*bdat_mfr[2400,..well], 1.1*bdat_mfr[2400,..well]))
  plot(get(well) ~ sec10, bdat_10, type = "l", ylab = "spikes per 10 sec interval")
}
well <- "A6"
plot_stuff(well)
for (well in paste0(rep(LETTERS[1:6],8),rep(1:8,each = 6))) {
  plot_stuff(well)
}


# A4 - mfr is just generally decreasing over time
# D4 - MFR starts out way higher, then settles down
# C8 MFR gradually decreases
# A5 - gradually decreasing
# A6 - same

# pretty much in all wells, the MFR peaks around 500 sec, then the cumulative MFR more or less gradually decreases over time
# about half are concave up adn decrease asymptotically to the final MFR<
# and the other half are concave down, with a real slope -> i.e., the MFR is changing noticeable over time.

# oh wait... I just remember that usually if a recording is over 2400s, they cut off the extra time at the beginning of the recording, instead of the end
# so the mfr values I have here based on cumulative spikes to 2400 s are not the same as what will be in neural stats file

# thoughts:
# - on one hand, I have seen enough to determine that the baseline MFR from 0-25min is not neccessarily a good indicator of the MFR at 2400sec
# - However, I also see that the MFR is always decreasing after ~500 seconds.
# so, in that way, the MFR from 0-25min might be a good upper bound?

# I think that Tim and Kathleen would say - don't use the 25 min recording to make any well quality decisions!

# let's check out how the LDH and AB values look on this plate
dat4 <- get_latest_dat("dat4","APCRA2019")
wllq_updates <- longdat[grepl("(Baseline MFR <)|(# of AE <)",wllq_notes) & acnm == "CCTE_Shafer_MEA_acute_cross_correlation_area", 
                        .(experiment.date, plate.id, rowi, coli, wllq, wllq_notes)]
dat4 <- merge(dat4, wllq_updates, by = c("experiment.date","plate.id","rowi","coli"), suffixes = c(".org",""), all = T)
dat4[is.na(wllq), wllq := wllq.org]
dat4[is.na(wllq_notes), wllq_notes := wllq_notes.org]
dat4[, c("wllq.org","wllq_notes.org") := list(NULL, NULL)]

par(mfrow = c(1,1))
stripchart(rval ~ conc, dat4[plate.id == "MW1236-24" & grepl("LDH",acnm) & wllq == 1 & wllt == "t"], vertical = T, pch = 1, method = "jitter", ylim = c(0, 0.0005))
stripchart(rval ~ conc, dat4[plate.id == "MW1236-24" & grepl("LDH",acnm) & wllq == 0 & wllt == "t"], vertical = T, pch = 19, col = "red", method = "jitter", add = T)

endpoint <- "LDH"
for (compound in unique(dat4[plate.id == "MW1236-24" & wllt == "t", unique(treatment)])) {

  if (nrow(dat4[treatment == compound & plate.id == "MW1236-24" & grepl(endpoint,acnm) & wllq == 0 & wllt == "t"]) > 0) {
    plotdat <- dat4[experiment.date == "20190402" & treatment == compound & grepl(endpoint,acnm)]
    plotdat$conc <- factor(plotdat$conc, levels = sort(unique(as.numeric(plotdat$conc))), ordered = T)
    stripchart(rval ~ conc, plotdat[wllq == 1], 
               vertical = T, pch = 1, method = "jitter", ylim = plotdat[, range(rval)],
               main = paste0(endpoint," for ",compound))
    stripchart(rval ~ conc, plotdat[plate.id != "MW1236-24" & grepl(endpoint,acnm) & wllq == 0], vertical = T, pch = 1, cex = 1.5, col = "pink", method = "jitter", add = T)
    stripchart(rval ~ conc, plotdat[plate.id == "MW1236-24" & grepl(endpoint,acnm) & wllq == 0], vertical = T, pch = 1, col = "red", method = "jitter", add = T, cex = 1.5)
  }
}
dat4[treatment == "B05", unique(spid)]
# "EPAPLT0154B06"

# 3 compounds are affected
# fir B04 - would not make a big diff either way
# for B05, if I remove the red points, there would only be 3 conc's left, and a curve would not be fit! But would probs be not a hit either way
# for B06

# okay, what affected again?
dat4[plate.id == "MW1236-24" & grepl("AB",acnm) & wllq == 0, .N, by = "spid"]
# spid N
# 1:          DMSO 1
# 2: EPAPLT0154B04 4
# 3: EPAPLT0154B05 6
# 4:    Picrotoxin 1
# 5: EPAPLT0154B06 2

# summarize affect on bval
dat4[experiment.date == "20190402" & (wllt == "n" | wllt == "t" & conc < 0.3) & grepl("AB",acnm) & wllq == 1, median(rval)] # 12393.67
dat4[experiment.date == "20190402" & (wllt == "n" | wllt == "t" & conc < 0.3) & grepl("AB",acnm) & (wllq == 1 | plate.id == "MW1236-24"), median(rval)] # 12393.67
# 12315.33
# bval would decrease by about 77 (-0.6%)
dat4[experiment.date == "20190402" & (wllt == "n" | wllt == "t" & conc < 0.3) & grepl("LDH",acnm) & wllq == 1, median(rval)] # 12393.67
dat4[experiment.date == "20190402" & (wllt == "n" | wllt == "t" & conc < 0.3) & grepl("LDH",acnm) & (wllq == 1 | plate.id == "MW1236-24"), median(rval)] # 12393.67
# LDH bval is 0 either way!
# bval would basically not change whether I include and exclude this well

# PICRO doesn't matter for cytotox endpoints

# EPAPLT0154B04
dat4[spid == "EPAPLT0154B04", unique(plate.id)] # hmm, only these 3 plates. Then where are the extra points coming from?
dat4[spid == "EPAPLT0154B04" & grepl("AB",acnm), .N, by = "conc"]

alldat4 <- get_latest_dat("dat4")
alldat4[spid == "EPAPLT0154B04", unique(apid)]
# "20190402". What???

# how many other plates does this affect?
alldat4[grepl("Recording length",wllq_notes), unique(plate.id)]
# "MW1076-38"
# cool,just one other plate from TC