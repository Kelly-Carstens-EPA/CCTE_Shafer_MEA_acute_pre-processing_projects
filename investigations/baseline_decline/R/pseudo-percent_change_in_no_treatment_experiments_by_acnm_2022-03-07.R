# ------------------------------------------------------------------------ #
# Calculate distribution of % change in baseline
# Using recording to which no treatment (not even DMSO) was added
# Mar 7, 2022
# ------------------------------------------------------------------------ #

# load packages
library(data.table)
library(ggplot2)
library(cowplot)

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(path = "mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)
# Note: on branch "experimental1"

# load data
dat1 <- get_latest_dat(lvl = "dat1", 'BaselineDeclineAnalysis')
# save(dat1, file = 'C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/data/BaselineDeclineAnalysis_dat1_2022-02-14.RData')



# Calculate pseudo rvals (% change in treated vs baseline recordin --------

dat1[, summary(analysis_duration)]
# oh, these are pretty much all 15 mins, -  1 sec or + 64 sec

dat1[, min_after_exp_start_time_end := min_after_exp_start_time + analysis_duration/60]
dat1[, min_after_exp_start_time_mid := min_after_exp_start_time + 0.5*analysis_duration/60]

dat1[, window_bas_start := 20]
dat1[, window_bas_end := window_bas_start+40]
dat1[, window_trt_start := window_bas_end+20]
dat1[, window_trt_end := window_trt_start+40]

dat1[, use_for_bas_val := as.numeric(window_bas_start <= min_after_exp_start_time_mid & 
                                       min_after_exp_start_time_mid <= window_bas_end)]
dat1[, use_for_trt_val := as.numeric(window_trt_start <= min_after_exp_start_time_mid & 
                                       min_after_exp_start_time_mid <= window_trt_end)]

# Confirm >1 recording for each tiem per plate
dat1[, .N, by = .(use_for_bas_val, use_for_trt_val)]
dat1[, .(length(unique(srcf[use_for_bas_val == 1]))), by = .(apid, plate.id)]
# cool, 1-2 poitns per plate
dat1[, .(length(unique(srcf[use_for_trt_val == 1]))), by = .(apid, plate.id)]
# same!

dat1[, .N, by = .(window_bas_start, min_after_exp_start_time_mid)]

dat1[, .(length(unique(srcf[use_for_bas_val == 1]))), by = .(apid, plate.id, maestro_type_foldername)]
dat1[, .(length(unique(srcf[use_for_trt_val == 1]))), by = .(apid, plate.id, maestro_type_foldername)]
dat1[apid == '20220208' & plate.id == 'MW78-6309', .N, by = .(apid, plate.id, maestro_type_foldername, srcf, min_after_exp_start_time_mid, window_bas_end, min_after_exp_start_time, analysis_duration)]
dat1[apid == '20220208' & plate.id == 'MW78-6309', .N, by = .(apid, plate.id, maestro_type_foldername, srcf, min_after_exp_start_time_mid, window_trt_end, min_after_exp_start_time, analysis_duration)]
# ah, this recording was just a bit shorter, so the mid point is barely within the window trt end time


# > Calcuate psedo % change! ----------------------------------------------

dat2 <- dat1[, .(activity_value.bas = mean(activity_value[wllq == 1 & use_for_bas_val == 1], na.rm = T),
                 activity_value.trt = mean(activity_value[wllq == 1 & use_for_trt_val == 1], na.rm = T)), 
             by = .(acsn, well, plate.id, experiment.date, apid, coli, rowi, acnm, files_log, maestro_type_foldername, buffer)]
dat2[, rval := (activity_value.trt - activity_value.bas)/abs(activity_value.bas)*100]
dat2[, median_rval := median(rval,na.rm = T), by = .(acnm)]
dat2 <- dat2[order(median_rval)]
dat2$acnm <- factor(dat2$acnm, levels= unique(dat2$acnm), ordered = T)
upperbound <- dat2[, quantile(rval, 0.99, na.rm = T)]

p <- ggplot(dat2, aes(x = rval, y = acnm))+
  geom_jitter(width = 0, height = 0.2, pch = 1, color = 'gray70')+
  geom_boxplot(fill = 'cornflowerblue', alpha = 0.3, outlier.shape = NA)+
  geom_vline(xintercept = 0)+
  # geom_vline(xintercept = 20, lty = 'dashed')+
  # geom_vline(xintercept = 15, lty = 'dashed')+
  geom_vline(xintercept = 10, lty = 'dashed')+
  # geom_vline(xintercept = 5, lty = 'dashed')+
  # geom_vline(xintercept = -20, lty = 'dashed')+
  # geom_vline(xintercept = -15, lty = 'dashed')+
  geom_vline(xintercept = -10, lty = 'dashed')+
  # geom_vline(xintercept = -5, lty = 'dashed')+
  geom_vline(xintercept = 0)+
  xlim(NA, upperbound)+
  ggtitle(paste0('MEA Acute Distribution of % change in activity value',
  '\nfrom window 1 20-60min to window 2 (80-120min)',
  '\nin untreated experiments on all 3 Maestro',
  '\n',length(dat2[rval > upperbound]), ' outliers removed above ',signif(upperbound,3)))+
  theme_bw()
p
ggsave(filename = 'investigations/baseline_decline/figs/pseudo-rval_distribution_by_acnm_in_no_treatment_experiments.png',
       width = 10, height = 10,
       plot = p)

summary(dat2$median_rval)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -24.89688  -5.02983   0.08324  -1.78086   3.63844  21.31778 
# huh, so -25% is the worst
# I coudl do a test-test, so see which endpoint significantly diff than 0...

dat2[median_rval < -5][order(median_rval)][, cat(unique(as.character(acnm)), sep = '\n')]
dat2[median_rval > 5][order(-median_rval)][, cat(unique(as.character(acnm)), sep = '\n')]


# Old stuff:
# Idea: visually show the duration of the recording? ----------------------

# Could add points at the tail end of the recording, so that there is a horizontal indicating the duration of the recording
# help with visualizing the recording block
# but, meh.

# Specific things I could check out:
# 
# - Just view a set of these plots for eveyr plate, for 1 - 3 endpoints
# - If I were to calculate the rval, using just time, given no treatment, what would the rval's be? (show how much they decrease)
# - How many of the activity values decrease from acceptable to not acceptable ranges over the time period (i.e. for MFR or nAE?)
