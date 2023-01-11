# analyzing the mean firing rate data 
# created in mr_baseline_cutoff_data_prep.R

library(data.table)

setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/MFR_cutoff_investigation")

load(file = "data/mc3_meanfiringrate_sbox_2020-02-22.RData") # 02 was a type, meant 05
load(file = "data/wdat1_meanfiringrate_sbox_2020-02-22.RData")

# # previous algebra thoughts...
# wdat1[,test_val := activity_value.treated*100/(100 + 3*bmad - bval)]
# wdat1[activity_value.diff < 5/60 & activity_value.baseline < test_val]
# 
# # for dn resp
# wdat1[,test_val2 := activity_value.treated*100/(100 - 3*bmad + bval)]
# wdat1[activity_value.diff < 5/60 & activity_value.baseline > test_val2]
# summary(wdat1$test_val2) # oof, got some neg
# max_baseline_with_high_resp <- wdat1[activity_value.diff < 5/60 & activity_value.baseline > test_val2, max(activity_value.baseline)]
# min_baseline_with_safe_resp <- wdat1[activity_value.diff < 5/60 & activity_value.baseline < test_val2, min(activity_value.baseline)]
# max_baseline_with_high_resp
# # [1] 5.946412
# min_baseline_with_safe_resp
# # [1] 0.1240756
# # aw man! since there is overlap in the baseline values for these groups, I can't draw a clear cutoff this way...
# # to be continued...
# # will def need to look into the bval noise a bit. Might try again by apid

bmad <- unique(wdat1$bmad)

plot(wdat1[abs(activity_value.diff) < 7/60, .(activity_value.baseline, resp)])
abline(h = 3*bmad)
abline(h= -1*3*bmad)
title(main = paste0("Resp versus baseline mean firing rate (Hz)\nwhere MFRt - MFRb < 5*60 (Hz)"))


# how does the resp vary with the raw diff?
plot(wdat1[, .(activity_value.diff, resp)])
abline(b = 0, a = 1)


# add in cval
wdat1[, cval := resp + bval]

# # Previous musings::
# 
# # wow. I don't understand how the values can get this big when baseline is greater than 1!
# # mc4 mthd_id=2 from bitbucket:
# # bmad.aeid.lowconc.nwells = function() {
# #   
# #   e1 <- bquote(dat[ , bmad := mad(resp[wllt == "n"], na.rm = TRUE)])
# #   list(e1)
# #   
# # }
# 
# dbDisconnect(con)
# 
# # then I can:
# # get alldat1, and extract the baseline data for mean firing rate
# # get table with columns baseline_mfr, treated_mfr, diff, resp, bmad
# # table[diff <= 5*60 & resp < bmad, min(baseline_mfr)] OR (I think I want this top one)
# # table[diff <= 5*60 & resp > bmad, max(baseline_mfr)] 
# # where diff <= 5*60, does resp increase mostly monotonically as baseline_mfr decrease?
# 
# # plot to answer that:
# plot(table[diff <= 5*60, .(baseline_mfr, resp)])
# abline(h = table[, unique(bmad)])
# # find baseline_mfr_cutoff such that 
# abline(v = baseline_mfr_cutoff)
# # all points to the right of this line are below bmad

# May 26, 2020

# interestign that wherever the diff is small, it seems that mfr at baseline is also on the small end
wdat1[abs(activity_value.diff) < 5/60, summary(activity_value.baseline)]
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1241  0.8464  1.4064  1.4841  1.9865  5.1995 
wdat1[, summary(activity_value.baseline)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1143  1.2796  1.8134  1.8941  2.4078  7.1416 
# if we do adda cutoff, it should probably be less than 1.25

# um, what?
wdat1[abs(activity_value.diff) > 5/60, .N] # 12289 points...
wdat1[abs(activity_value.diff) < 5/60, .N] # 1177 . Woah!!
wdat1[is.na(activity_value.diff), .N] # none - right, mfr can be 0

wdat1[, summary(activity_value.diff)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -5.9464 -1.1276 -0.5327 -0.6508 -0.1402 11.9770 

wdat1[abs(cval) > 100, .(resp, spid, cval, bval)]

# trying out bval by exp date
wdat1[, date := sapply(apid, function(x) strsplit(x, split = "_")[[1]][1])]
wdat1[, .(use_concs = paste0(sort(unique(logc))[1:2],collapse = ",")), by = c("date","spid")]

wdat1[abs(activity_value.diff) < 5/60 & resp > 3*bmad , summary(cval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -12.053  -1.564   1.260   2.797   3.838  45.746 
# all of these are resonably small
wdat1[abs(activity_value.diff) < 5/60 & resp > 3*bmad & cval > 10]
# so this is not great
# cval's of 30 and 40 are not great, but those are just 2 exceptions
# i really expected bval to be mroe more tame

boxplot(cval ~ apid, wdat1[cval < 300])
boxplot(wdat1$bval)

# let's get the cndx'es
library(tcpl)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplListFlds("mc1")
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplLoadAcid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")
mc1 <- tcplLoadData(lvl = 1L, fld = "acid", val = 2432)
nrow(mc1)
wdat1 <- merge(wdat1, mc1)

wdat1[(wllt == "t" & cndx %in% c(1,2)) | wllt == "n"]

wdat1[m0id.x != m0id.y] # empty, weird

# checkout cval to baseline
plot_cval <- function(rval_coff, spikes_per_min, first = F, color = spikes_per_min, yrange = c(-100,100)) {
  if (first) {
    plot(wdat1[abs(activity_value.diff) < spikes_per_min/60, .(60*activity_value.baseline, rval)], ylim = yrange, col = color, pch = 19,
         ylab = "Percent change in mean firing rate (MFRt - MFRb)/MFRb * 100", xlab = "Baseline Mean Firing Rate (spikes/min)")
  } else {
    points(wdat1[abs(activity_value.diff) < spikes_per_min/60, .(60*activity_value.baseline, rval)], ylim = yrange, col = color, pch = 19,
           ylab = "rval (MFRt - MFRb)/MFRb * 100", xlab = "Baseline Mean Firing Rate (spikes/min)")
  }
  abline(h = rval_coff, col = "gray80", lwd = 2)
  abline(h = -1*rval_coff, col = "gray80", lwd = 2)
  mfr_cutoff <- wdat1[abs(activity_value.diff) < spikes_per_min/60 & abs(rval) > rval_coff, max(activity_value.baseline)]
  print(60*mfr_cutoff)
  abline(v = 60*mfr_cutoff, col = color, lwd = 1.5)
  text(x = 60*(mfr_cutoff + 0.15), y = max(yrange)*0.8, labels = paste0(signif(mfr_cutoff*60, 3)," spikes/min"), srt = -90)
  # title(main = paste0("Percent Change in Mean Firing Rate vs Mean Firing Rate at Baseline\nwhere MFRt - MFRb < ",spikes_per_min," spikes per min"))
}

rval_coff <- 20
plot_cval(rval_coff, 2)
plot_cval(rval_coff, 3)
plot_cval(rval_coff, 4)
plot_cval(rval_coff, 5)
plot_cval(rval_coff, 6)
plot_cval(rval_coff, 7)

plot_cval(rval_coff, 10)



# all the points
graphics.off()
pdf(file = "rval_vs_baseline_mfr_2020-05-26.pdf")
yrange <- range(wdat1[abs(activity_value.diff) < 20/60, .(cval)])
plot_cval(rval_coff, 20, first = T, yrange = yrange)
plot_cval(rval_coff, 10, yrange = yrange)
plot_cval(rval_coff, 5, color = "black", yrange = yrange)
legend(x = "topright", title = "Points where raw difference in MFR is less than:", 
       legend = c("20 spikes per min","10 spikes per min","5 spikes per min",paste0("+/-",rval_coff,"% change")), 
       pch = c(rep(19,3),15), col = c(20, 10, "black","gray80"), cex = 0.8)
title(main = "Percent Change in Mean Firing Rate vs Mean Firing Rate at Baseline")
graphics.off()

# making this plot one mroe time
plot(wdat1[abs(activity_value.diff) < 5/60, .(activity_value.baseline, resp)], ylim = c(-100,100))
abline(h = 3*bmad)
abline(h = -3*bmad)
title(main = paste0("Resp versus baseline mean firing rate (Hz)\nwhere MFRt - MFRb < 5/60 (Hz)"))

wdat1[, summary(activity_value.baseline)]
boxplot(unique(wdat1$bval))
stripchart(x = unique(wdat1$bval), vertical  = T, method = "jitter", pch = 1)

# how many points would I lose total?
nrow(wdat1[wllq==1]) # [1] 13466
wdat1[wllq==1 & activity_value.baseline*60 < 20,.N] # [1] 137
wdat1[wllq==1 & activity_value.baseline*60 <30,.N] # [1] 394
plot(density(60*wdat1$activity_value.baseline), main = "Density plot of Baseline Mean Firing Rate", xlab = "Baseline MFR (spikes/min)")
abline(v=20)
abline(v=30)

# find the upper and lower 5th percentiles
plot(density(60*wdat1$activity_value.baseline), main = "Density plot of Baseline Mean Firing Rate", xlab = "Baseline MFR (spikes/min)")
lower_percentile <- quantile(wdat1$activity_value.baseline, c(.05))
upper_percentile <- quantile(wdat1$activity_value.baseline, c(.95))
sapply(c(lower_percentile, upper_percentile), function(x) abline(v=60*as.numeric(x)))

# what percentile is closest to 20 spikes per minute?
get_val <- 20.0/60
N <- 100
lowq <- 0.01
highq <- 0.05
for (i in 1:N) {
  medq <- mean(c(lowq, highq))
  med_x <- as.numeric(quantile(wdat1$activity_value.baseline, c(medq)))
  if(as.numeric(med_x) > get_val) {
    highq <- medq
  } else {
    lowq <- medq
  }
  i <- i+1
}
medq <- mean(c(lowq, highq))
med_x <- as.numeric(quantile(wdat1$activity_value.baseline, c(medq)))
print(paste0("Approximate quantile for ",get_val,":"))
print(medq)
quantile(wdat1$activity_value.baseline, medq)

abline(v = med_x*60)

# add in the flip side of this lower bound
upper_q <- quantile(wdat1$activity_value.baseline, 1-medq)
abline(v = upper_q*60)
text(x = c(med_x*60, as.numeric(lower_percentile)*60, as.numeric(upper_percentile)*60, as.numeric(upper_q*60)) + 10, 
     y = 0.006, labels = paste0(signif(c(medq, 0.05, 0.95, 1-medq)*100, 3),"%"), srt = -90)

# # the danger is applying a too agressive mfr coff, and losing all of the good points within
# #how many points would we lose with a coff of, say, 20?
# wdat1[activity_value.baseline < 25/60, .N] # lose 137 points
# wdat1[activity_value.baseline < 20/60, summary(60*activity_value.diff)]
# # most have a diff within -10 to 10 spikes per min
# # but definitely not all, some have a significant diff
# 
# # would we lose a lot of DMSO wells if we did this?
# wdat1[activity_value.baseline < 20/60, .N, by = "spid"] # only 5 DMSOwells
# 
# wdat1[abs(bval) > 70, summary(activity_value.diff)]
# 
# plot(wdat1[, .(activity_value.baseline, activity_value.treated)])

# May 27, 2020
# saving the mc5_mc6 data before I re-pipeline
library(tcpl)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
tcplLoadAeid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")
# acnm aeid                                 aenm
# 1: NHEERL_MEA_acute_firing_rate_mean 2425 NHEERL_MEA_acute_firing_rate_mean_up
# 2: NHEERL_MEA_acute_firing_rate_mean 2442 NHEERL_MEA_acute_firing_rate_mean_dn

mc5 <- tcplLoadData(lvl = 5L, fld = "aeid", val = c(2425,2442))
mc5 <- tcplPrepOtpt(mc5)
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val = c(2425,2442))
mc6_collapsed <- mc6[, .(flag_list = paste0(sort(unique(mc6_mthd_id)),collapse=",")), by = c("spid","aeid","m4id","m5id")] # I think this only includes spids with at least 1 flag
nrow(mc5) # 1030
mc5_mc6 <- merge(mc5, mc6_collapsed, all.x = T, all.y = F, by = c("aeid","spid","m4id","m5id"))
nrow(mc5_mc6) # 1030
save(mc5_mc6, file = "mc5_mc6_meanfiringrate_sbox_nocoff_2020-05-22.RData")

# grab a graph of a compound with bad outlier (even more cool if hit call would change)
wdat1[activity_value.baseline < 20/60 & abs(resp) > 80, .N, by = "spid"] # EPAPLT0154D01 has the most points (4) in this category
tcplPlot(lvl = 6L, fld = c("aeid","spid"), val = list(2425, "EPAPLT0154D01"), type = "mc", output = "pdf", fileprefix = "EPAPLT0154D01_sbox_AEID2425_2020-05-22")
tcplPlot(lvl = 6L, fld = c("aeid","spid"), val = list(2442, "EPAPLT0154D01"), type = "mc", output = "pdf", fileprefix = "EPAPLT0154D01_sbox_AEID2442_2020-05-22")


# June 1, 2020
# confirming what the upper and lower percentile values numerically are
# so that I can get a numerical cutoff
wdat1[, .N, by = "wllq"]
# wllq     N
# 1:    1 13466
# wdat1 comes from mc3, which has already removed where wllq == 0
# note that wllq has been set to 0 where nAE < 10, and for known wllq factors 
quantile(wdat1$activity_value.baseline, c(0.01,0.99))
# 1%       99% 
# 0.3310055 4.3419145 # in Hz
60*quantile(wdat1$activity_value.baseline, c(0.01,0.99))
# 1%       99% 
# 19.86033 260.51487 # in spikes per min

# June 3, 2020
# finding the cutoff at the 5 and 95 percentiles
wdat1[, .N, by = "wllq"]
# again, all wllq == 1
quantile(wdat1$activity_value.baseline, c(0.05,0.95))
# 5%       95% 
# 0.6377603 3.4036511 
60*quantile(wdat1$activity_value.baseline, c(0.05,0.95))
# 5%       95% 
# 38.26562 204.21906 
