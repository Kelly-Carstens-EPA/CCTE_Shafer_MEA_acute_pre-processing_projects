# checking data

# in source_to_lvl0_nheerl_mea_acute.R, all data values for the wells in columns 7 and 8 are removed in 'MW 1076-37' on 20160317
# supposedly based on the file "ToxCast MEA data Outliers and data check.docx"
# I am checking if I really want to remove the data from all of those wells
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")
load(file = 'output/ToxCast2016_alldat1_2020-05-18.RData')
load(file = 'output/ToxCast2016_alldat2_2020-05-18.RData')

# just want a pic of these values, so others can decide
# let look at AE, MFR, num bursts, and num network spikes

make_outlier_check_graphs <- function(apidi, exclude_coli, file_name) {
  
  see.endpoints <- c("NHEERL_MEA_acute_firing_rate_mean","NHEERL_MEA_acute_synchrony_index", 
                     "NHEERL_MEA_acute_bursting_electrodes_number_mean","NHEERL_MEA_acute_per_network_burst_spike_number_mean" )
  
  graphics.off()
  pdf(file = file_name)
  
  # BASELINE ACTIVITY in those wells
  # histogram of MFR for baseline of all values
  # hist(alldat1[tcpl_acsn == "NHEERL_MEA_acute_firing_rate_mean" & run_type == "baseline",activity_value])
  # points(x = alldat1[tcpl_acsn == "NHEERL_MEA_acute_firing_rate_mean" & run_type == "baseline" & apid == apidi & coli %in% c(7,8),activity_value],
  #        y = rep(0, 12))
  
  # compare activity against all other wells in data set
  for (endpoint in see.endpoints) {
    boxplot(alldat1[wllq == 1 & tcpl_acsn == endpoint & run_type == "baseline",activity_value])
    points(y = alldat1[tcpl_acsn == endpoint & run_type == "baseline" & apid == apidi & coli %in% exclude_coli,activity_value],
           x = rep(1, 12), col = "red", cex = 1)
    title(main = paste0("Baseline ",endpoint, "\nin ToxCast 2016"), ylab = paste0(endpoint))
  }
  
  # compare activity against other wells on plate
  for (endpoint in see.endpoints) {
    boxplot(alldat1[wllq == 1 & tcpl_acsn == endpoint & run_type == "baseline" & apid == apidi,activity_value])
    points(y = alldat1[tcpl_acsn == endpoint & run_type == "baseline" & apid == apidi & coli %in% exclude_coli,activity_value],
           x = rep(1, 12), col = "red", cex = 1)
    title(main = paste0("Baseline ",endpoint, "\nin ",apidi), ylab = paste0(endpoint))
  }
  
  
  # RVAL's
  
  # compare activity against all other wells in data set
  for (endpoint in see.endpoints) {
    boxplot(alldat2[wllq == 1 & acsn == endpoint,rval])
    points(y = alldat2[acsn == endpoint & apid == apidi & coli %in% exclude_coli,rval],
           x = rep(1, 12), col = "red", cex = 1)
    title(main = paste0("Percent Change ",endpoint, "\nin ToxCast 2016"), ylab = paste0(endpoint))
  }
  
  # compare activity against other wells on plate
  for (endpoint in see.endpoints) {
    boxplot(alldat2[wllq == 1 & acsn == endpoint & apid == apidi,rval])
    points(y = alldat2[acsn == endpoint & apid == apidi & coli %in% exclude_coli,rval],
           x = rep(1, 12), col = "red", cex = 1)
    title(main = paste0("Percent Change ",endpoint, "\nin ",apidi), ylab = paste0(endpoint))
  }
  
  graphics.off()
}

apidi <- "20160317_MW1076-37"
make_outlier_check_graphs(apidi, exclude_coli = c(7,8), file_name = paste0("figs/check_if_outlier_",apidi,".pdf"))
apidi <- "20160531_MW1048-15"
make_outlier_check_graphs(apidi, exclude_coli = c(1,2), file_name = paste0("figs/check_if_outlier_",apidi,".pdf"))

# conclusion: visually, there is no reason to exclue these values
