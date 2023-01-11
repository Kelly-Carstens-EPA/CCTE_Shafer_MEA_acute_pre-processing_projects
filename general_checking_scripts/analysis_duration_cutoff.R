# seeking to find a coff for the analysis duration
library(data.table)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")

load("APCRA2019/output/APCRA2019_alldat1_2020-05-21.RData")
dat1 <- alldat1
load("DNT2019/output/DNT2019_alldat1_2020-05-19.RData")
dat1 <- rbind(dat1, alldat1)
load("ToxCast2016/output/ToxCast2016_alldat1_2020-05-19.RData")
dat1 <- rbind(dat1, alldat1, fill = TRUE)
rm(alldat1)

# let's see the distribution of analysis durations
dat1[, summary(analysis_duration)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 544.2  2400.0  2400.5  2405.2  2403.2  2889.0
stripchart(dat1[, unique(analysis_duration)], vertical = T, method = "jitter", pch = 1)
title(main = "Scatter plot of Analysis Duration times in seconds")
boxplot(dat1[run_type == "baseline", analysis_duration])

# okay, so obviously the analysis at 544 seconds should be removed.
# but how high is too high?
# let's look at 

plot(dat1[run_type == "baseline" & tcpl_acsn == "NHEERL_MEA_acute_spike_number", .(analysis_duration, activity_value)])
title(main = "Spike Number vs Analysis Duration in Baseline Recordings")
plot(dat1[run_type == "baseline" & tcpl_acsn == "NHEERL_MEA_acute_spike_number", .(median(activity_value,na.rm=T)), by = "analysis_duration"])


plot(dat1[run_type == "baseline" & tcpl_acsn == "NHEERL_MEA_acute_burst_number", .(activity_value), by = "analysis_duration"])

# ya know what is more important...
# how the baseline and treated recording times compare
duration_diff <- merge(dat1[run_type == "baseline" & tcpl_acsn == "NHEERL_MEA_acute_spike_number", .(apid, rowi, coli, analysis_duration)], 
                       dat1[run_type == "treated" & tcpl_acsn == "NHEERL_MEA_acute_spike_number", .(apid, rowi, coli, analysis_duration)], by = c("apid","rowi","coli"),
                       suffixes = c(".baseline",".treated"))
duration_diff[, analysis_diff := analysis_duration.baseline - analysis_duration.treated]
stripchart(duration_diff[, unique(analysis_diff)], vertical=T, method = "jitter", pch = 1)

boxplot(dat1[run_type == "treated", unique(analysis_duration)])

duration_diff[, analysis_percent_diff := (analysis_duration.treated - analysis_duration.baseline) / analysis_duration.baseline * 100]
duration_diff[abs(analysis_percent_diff) > 10]

# is 10% a good coff?
# let's look at teh percent change in spike number in the media wells
# (using media instead of dmso, since we do often see a change there)
load("APCRA2019/output/APCRA2019_alldat4_2020-05-21.RData")
load("DNT2019/output/DNT2019_alldat4_2020-05-21.RData")
load("ToxCast2016/output/ToxCast2016_alldat4_2020-05-21.RData")
dat4 <- rbindlist(list(APCRA2019_alldat4, DNT2019_alldat4, ToxCast2016_alldat4))
rm(list = c("APCRA2019_alldat4","ToxCast2016_alldat4","DNT2019_alldat4"))

dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_spike_number" & wllq == 1, summary(rval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -99.994 -14.771  -1.861  -6.496   8.496  99.502 
# wow, so even media wells have quite a range
dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_spike_number" & wllq == 1, .N]
# 61, eh sample size

stripchart(dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_spike_number" & wllq == 1, rval], vertical = T, pch = 1, method = "jitter")
# there is definitely a cluster in the middle...
dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_spike_number" & wllq == 1 & rval < 50, max(rval)]
# [1] 25.66432
dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_spike_number" & wllq == 1 & rval > -50, min(rval)]
# [1] -32.22949
abline(h = 25)
abline(h = -32.5)

# wait!! the analysis durations are different here as well! :(
# I don't know why we are using spike number at all... what does it tell us that firing rate does not?

# let's check out burst number
stripchart(dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_burst_number" & wllq == 1, rval], vertical = T, pch = 1, method = "jitter")
# there is definitely a cluster in the middle...
dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_burst_number" & wllq == 1 & rval < 50, max(rval)]
# [1] 30.8977
dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_burst_number" & wllq == 1 & rval > -50, min(rval)]
# [1] -36.68511
abline(h = 31)
abline(h = -37)

# let's check out the analysis duration for media wells
media_wells <- dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_spike_number", .(rval, wllq_final = wllq), by = c("apid","rowi","coli")]
media_baseline <- merge(dat1[tcpl_acsn =="NHEERL_MEA_acute_spike_number" & run_type == "baseline"], media_wells, all.x =F, by = c("apid","rowi","coli"))
media_treated <- merge(dat1[tcpl_acsn =="NHEERL_MEA_acute_spike_number" & run_type == "treated"], media_wells, all.x =F, by = c("apid","rowi","coli"))
media_dat1 <- merge(media_baseline, media_treated, by = c("apid","rowi","coli","well","tcpl_acsn","rval","wllq_final"), suffixes = c(".b",".t"))
media_dat1[, analysis_percent_diff := (analysis_duration.t - analysis_duration.b)/analysis_duration.b]
stripchart(media_dat1$analysis_percent_diff, vertical=T, pch = 1, method = "jitter")
# let's just look at the media recordings where the percent change in analysis duration is less than 5%

stripchart(media_dat1[wllq_final == 1 & abs(analysis_percent_diff) < 0.005, rval], vertical = T, pch = 1, method = "jitter", ylab = "Percent Change in Spike Number")
# there is definitely a cluster in the middle...
media_dat1[wllq_final == 1 & abs(analysis_percent_diff) < 0.005 & rval < 50, max(rval)]
# [1] 25.66432
media_dat1[wllq_final == 1 & abs(analysis_percent_diff) < 0.005 & rval > -50, min(rval)]
# [1] -32.22949
# abline(h = 25)
# abline(h = -32.5)
title(main = "Scatterplot of Media Wells Percent Change in Spike Number")

# repeat for acute burst number
media_wells <- dat4[spid == "Media" & acsn == "NHEERL_MEA_acute_burst_number", .(rval, wllq_final = wllq), by = c("apid","rowi","coli")]
media_baseline <- merge(dat1[tcpl_acsn =="NHEERL_MEA_acute_burst_number" & run_type == "baseline"], media_wells, all.x =F, by = c("apid","rowi","coli"))
media_treated <- merge(dat1[tcpl_acsn =="NHEERL_MEA_acute_burst_number" & run_type == "treated"], media_wells, all.x =F, by = c("apid","rowi","coli"))
media_dat1 <- merge(media_baseline, media_treated, by = c("apid","rowi","coli","well","tcpl_acsn","rval","wllq_final"), suffixes = c(".b",".t"))
media_dat1[, analysis_percent_diff := (analysis_duration.t - analysis_duration.b)/analysis_duration.b]
stripchart(media_dat1$analysis_percent_diff, vertical=T, pch = 1, method = "jitter")
# let's just look at the media recordings where the diff is less than 10 seconds

stripchart(media_dat1[wllq_final == 1 & abs(analysis_percent_diff) < 0.005, rval], vertical = T, pch = 1, method = "jitter", ylab = "Percent Change in Burst Number")
# there is definitely a cluster in the middle...
media_dat1[wllq_final == 1 & abs(analysis_percent_diff) < 0.005 & rval < 50, max(rval)]
# [1] 28.03298
media_dat1[wllq_final == 1 & abs(analysis_percent_diff) < 0.005 & rval > -50, min(rval)]
# [1] -35.76389
title(main = "Scatterplot of Media Wells Percent Change in Burst Number")

# okay! since the rval corresponds directly to the lengths of the recordings...
# I am going to set the threshold for change in analysis duration length to 20% change in time
# then maybe just flag if both recordings are significanlty above or below?
# i.e., 20% is well within the range of media wells change

# target:
# what do I think should be the allowable threshold for any recording length
# or, what is teh allowable threshold for the difference in recording length between baseline and treated?
# make pretty graphs/tables to support

# how many recordings would I have to remove, if use the 10% threshold?
baseline_recordings <- dat1[run_type == "baseline", .(well, apid, coli, rowi, srcf, wllq, analysis_duration, tcpl_acsn)]
treated_recordings <- dat1[run_type == "treated", .(well, apid, coli, rowi, srcf, analysis_duration, tcpl_acsn)]
compare_recordings <- merge(baseline_recordings, treated_recordings, by = setdiff(names(treated_recordings), c("srcf","analysis_duration")), suffixes = c(".t",".b"))
compare_recordings[, analysis_percent_diff := (analysis_duration.t - analysis_duration.b) / analysis_duration.b * 100]
compare_recordings[abs(analysis_percent_diff) > 10, .N, by = c("apid","analysis_duration.b","analysis_duration.t","analysis_percent_diff")]
# apid analysis_duration.b analysis_duration.t analysis_percent_diff   N
# 1: 20150728_MW1062-30             2404.75             2889.00              20.13723 720
# 2: 20150924_MW1047-34             2693.50             2401.50             -10.84091 720
# 3: 20151222_MW1056-40             2400.75             2641.25              10.01770 720
# 4: 20160317_MW1076-38              544.25             2415.75             343.86771 720
