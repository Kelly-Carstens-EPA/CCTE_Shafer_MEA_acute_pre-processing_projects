# want to see which endpoints are NA
library(data.table)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")

load("APCRA2019/output/APCRA2019_dat1_2020-06-04.RData")
alldat1 <- dat1
rm(dat1)
load("DNT2019/output/DNT2019_dat1_2020-06-04.RData")
alldat1 <- rbind(alldat1, dat1)
rm(dat1)
load("ToxCast2016/output/ToxCast2016_dat1_2020-06-06.RData")
alldat1 <- rbind(alldat1, dat1)
rm(dat1)

nrow(alldat1)
alldat1[, .(num_na = sum(is.na(activity_value)), num_zero = sum(activity_value == 0, na.rm = T), .N), by = "tcpl_acsn"]

alldat1[activity_value < 0.1, summary(activity_value)]

alldat.w <- dcast(alldat1, well + plate.id + experiment.date + apid + rowi + coli + run_type + srcf + analysis_start + analysis_duration + wllq +wllq_notes+ files_log ~ tcpl_acsn,
                  value.var = "activity_value")

# okay, so I am not sure that the mean burst duration should ever be NA instead of 0
# but, in any case, all of those wells have wllq == 0, so it doesn't matter!!
alldat.w[is.na(NHEERL_MEA_acute_burst_duration_mean), .N, by = c("NHEERL_MEA_acute_burst_number","run_type","wllq","wllq_notes")]

# other endpoint that I think should be 0 instead of NA:
# NHEERL_MEA_acute_burst_percentage_mean    426      589
# NHEERL_MEA_acute_network_burst_percentage   1078        0
# NHEERL_MEA_acute_burst_number     20      995 - but again, 
# NHEERL_MEA_acute_firing_rate_mean     20      406 (hmm, it's only NA in 20 wells...)

# other like the per network burst spike number mean... eh, less of a big deal to me

alldat.w[is.na(NHEERL_MEA_acute_firing_rate_mean)] # for all 20 wells, all values are NA here - this is where the other 20 values are NA as well I assume

alldat.w[is.na(NHEERL_MEA_acute_per_network_burst_spike_number_mean), .N, by = "NHEERL_MEA_acute_burst_number"]
