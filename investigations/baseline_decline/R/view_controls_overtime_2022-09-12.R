# ------------------------------------------------------------------------ #
# How does the baseline decline look for all MEA Acute data to date?
# Will look at all Median and DMSO-only wells
# Sept 12, 2022
# ------------------------------------------------------------------------ #

# load packages
library(data.table)
library(ggplot2)
library(cowplot)

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(path = "mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)

# Load latests data
load('lvl0_snapshots/dat4_2020-07-29.RData')
dat4[, .N, by = .(origin)]
# origin      N
# 1: ToxCast2016 447120
# 2:   APCRA2019 101898
# 3:     DNT2019  84474
# 4:      GF2019   6498

# Add in another dataset that will be added next time
add.dat <- get_latest_dat(lvl = "dat4", c('DNTFalseNegatives'))
dat4 <- rbind(dat4, add.dat)

dat4 <- remove_dmso_outliers(dat4, create_fig = F)
# [1] "Number of wllt='n' wells with MFR rval outside of bounds:"
# origin wllq  N
# 1: ToxCast2016    1  3
# 2:   APCRA2019    1 23
# 3:     DNT2019    1  8
# 4:      GF2019    1  1
# 5: ToxCast2016    0  8
# 6:   APCRA2019    0 23
# 7:     DNT2019    0 15
# [1] "Wells that will be set to wllq=0 for each acnm:"
# acnm  N
# 1:                         CCTE_Shafer_MEA_acute_active_electrodes_number 81
# 2:                          CCTE_Shafer_MEA_acute_burst_duration_IQR_mean 81
# 3:                           CCTE_Shafer_MEA_acute_burst_duration_IQR_std 81
# 4:                              CCTE_Shafer_MEA_acute_burst_duration_mean 81
# 5:                               CCTE_Shafer_MEA_acute_burst_duration_std 81
# 6:                             CCTE_Shafer_MEA_acute_burst_frequency_mean 81
# 7:                              CCTE_Shafer_MEA_acute_burst_frequency_std 81
# 8:                                     CCTE_Shafer_MEA_acute_burst_number 81
# 9:                            CCTE_Shafer_MEA_acute_burst_percentage_mean 81
# 10:                             CCTE_Shafer_MEA_acute_burst_percentage_std 81
# 11:                       CCTE_Shafer_MEA_acute_bursting_electrodes_number 81
# 12:                           CCTE_Shafer_MEA_acute_cross_correlation_HWHM 81
# 13:                CCTE_Shafer_MEA_acute_cross_correlation_HWHM_normalized 81
# 14:                           CCTE_Shafer_MEA_acute_cross_correlation_area 81
# 15:                CCTE_Shafer_MEA_acute_cross_correlation_area_normalized 81
# 16:                                 CCTE_Shafer_MEA_acute_firing_rate_mean 81
# 17:                        CCTE_Shafer_MEA_acute_firing_rate_mean_weighted 81
# 18:                  CCTE_Shafer_MEA_acute_inter-network_burst_interval_CV 81
# 19:                      CCTE_Shafer_MEA_acute_interburst_interval_CV_mean 81
# 20:                       CCTE_Shafer_MEA_acute_interburst_interval_CV_std 81
# 21:                         CCTE_Shafer_MEA_acute_interburst_interval_mean 81
# 22:                          CCTE_Shafer_MEA_acute_interburst_interval_std 81
# 23:                           CCTE_Shafer_MEA_acute_interspike_interval_CV 81
# 24:       CCTE_Shafer_MEA_acute_mean_interspike_interval_within_burst_mean 81
# 25:        CCTE_Shafer_MEA_acute_mean_interspike_interval_within_burst_std 81
# 26:     CCTE_Shafer_MEA_acute_median_interspike_interval_within_burst_mean 81
# 27:      CCTE_Shafer_MEA_acute_median_interspike_interval_within_burst_std 81
# 28:                       CCTE_Shafer_MEA_acute_network_burst_duration_IQR 81
# 29:                      CCTE_Shafer_MEA_acute_network_burst_duration_mean 81
# 30:                       CCTE_Shafer_MEA_acute_network_burst_duration_std 81
# 31:                          CCTE_Shafer_MEA_acute_network_burst_frequency 81
# 32:                             CCTE_Shafer_MEA_acute_network_burst_number 81
# 33:                         CCTE_Shafer_MEA_acute_network_burst_percentage 81
# 34:                      CCTE_Shafer_MEA_acute_per_burst_spike_number_mean 81
# 35:                       CCTE_Shafer_MEA_acute_per_burst_spike_number_std 81
# 36:         CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean 81
# 37:          CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_std 81
# 38: CCTE_Shafer_MEA_acute_per_network_burst_mean_spikes_per_electrode_mean 81
# 39:  CCTE_Shafer_MEA_acute_per_network_burst_mean_spikes_per_electrode_std 81
# 40:              CCTE_Shafer_MEA_acute_per_network_burst_spike_number_mean 81
# 41:               CCTE_Shafer_MEA_acute_per_network_burst_spike_number_std 81
# 42:                                     CCTE_Shafer_MEA_acute_spike_number 81
# 43:                                  CCTE_Shafer_MEA_acute_synchrony_index 81
# 44:                                               CCTE_Shafer_MEA_acute_AB 81
# 45:                                              CCTE_Shafer_MEA_acute_LDH 81

# Add bval's
plotdat <- dat4[wllq == 1]
plotdat[, med_of_controls := median(rval[wllt == 'n']), by = .(apid, acnm)]


# View stuff --------------------------------------------------------------

dat4[, experiment_date_num := as.Date(experiment.date, format = '%Y%m%d')]
# dat4[, .N, by = .(experiment.date, experiment_date_num)]
dat4[wllt == 'n', .N, by = .(spid)]
# spid     N
# 1:  DMSO 39021
# 2: Water   405
ggplot(dat4[wllt == 'n' & wllq == 1 & acnm == 'CCTE_Shafer_MEA_acute_firing_rate_mean_weighted'], aes(x = experiment_date_num, y = rval)) +
  geom_point()

# wow, it's really no worse no than it was in teh past!

# Let's get rid of this extra white space
plotdat[wllt == 'n' & wllq == 1, summary(rval)]
plotdat[wllt == 'n' & wllq == 1 & rval > 1000, .N]
# still 803??
plotdat[wllt == 'n' & wllq == 1 & !grepl('(LDH)|(AB)',acnm), summary(rval)]
plotdat[wllt == 'n' & wllq == 1 & !grepl('(LDH)|(AB)',acnm) & rval > 1000, .N]
plotdat[wllt == 'n' & wllq == 1 & !grepl('(LDH)|(AB)',acnm) & rval > 500, .N]
# 57


pdf(file = 'investigations/baseline_decline/figs/mea_acute_percent_change_in_controls_over_time_2022-09-12.pdf',
    width = 10, height = 6)
for (acnmi in unique(plotdat$acnm)) {
  
  p <- ggplot(plotdat[wllt == 'n' & wllq == 1 & acnm ==acnmi & rval < 500], aes(x = experiment.date, y = rval)) +
    geom_point(pch = 1)+
    geom_point(aes(y = med_of_controls))+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = plotdat[wllt == 'n' & wllq == 1 & acnm ==acnmi, median(rval)], col = 'blue', lty = 'dashed')+
    ylab('rval (% change in activity level)')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))+
    ggtitle(paste0(acnmi,'\nMEA Acute percent change in activity from baseline to treated recording window\nfor DMSO and Water control wells. All MEA Acute data to date'),
            subtitle = paste0('Solid points = median of controls by experiment date. ',plotdat[wllt == 'n' & wllq == 1 & acnm ==acnmi & rval >= 500, .N],
                              ' outliers above 500% excluded removed\nBlue line shows median across all experiment dates'))
  print(p)
}
graphics.off()
