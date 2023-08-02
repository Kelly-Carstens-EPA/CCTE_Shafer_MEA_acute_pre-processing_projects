# script to compare the data, for development
# May 18, 2020
library(data.table)

# load all of the alldat1 from each dataset
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
load(file = "APCRA2019/output/APCRA2019_alldat1_2020-05-18.RData")
apcra_alldat1 <- alldat1
apcra_alldat1[, dataset := "APCRA2019"]
load(file = "DNT2019/output/DNT2019_alldat1_2020-05-18.RData")
dnt_alldat1 <- alldat1
dnt_alldat1[, dataset := "DNT2019"]
load(file = "ToxCast2016/output/ToxCast2016_alldat1_2020-05-18.RData")
toxcast_alldat1 <- alldat1
toxcast_alldat1[, dataset := "ToxCast2016"]

alldat1 <- rbind(apcra_alldat1, dnt_alldat1, toxcast_alldat1, fill=T)
rm(list = c("apcra_alldat1","toxcast_alldat1","dnt_alldat1"))

# ANALYSIS OF SETTINGS DIFFERENCES

settings <- names(alldat1)[!(names(alldat1) %in% c("well","activity_value","apid","coli","rowi","srcf","run_type","wllq","tcpl_acsn","dataset","analysis_duration","analysis_start"))]

# get well trt data for each file

# quesiton 1: are the settings always the same from treated to baseline (other than timing)?
settings.summary <- alldat1[, lapply(.SD, function(x) length(unique(x))), .SDcols = settings, by = "apid"]

for (setting in settings) {
  setnames(settings.summary, old = setting, new = "settingi")
  cat(setting,"\n")
  print(settings.summary[settingi > 1, unique(apid)])
  setnames(settings.summary, new = setting, old = "settingi")
}

# these settings has some differences between treated vs baseline settings:
# Digital High Pass Filter 
# [1] "20160621_MW 1072-9"  "20160317_MW 1076-38" "20151105_MW 1077-14"
# Current Temperature 
# [1] "20190326_MW1236-17"    "20190326_MW1236-18"    "20190328_MW1236-19"    "20190328_MW1236-20"    "20190328_MW1236-21"    "20190402_MW1236-22"   
# [7] "20190404_MWNo Barcode" "20190404_MW1237-12"    "20190409_MW1237-13"    "20190409_MW1237-14"    "20190409_MW1237-15"    "20190411_MW1237-16"   
# [13] "20190411_MW1237-17"    "20190411_MW1237-18"    "20190416_MW1237-19"    "20190416_MW66-9604"    "20190416_MW66-9613"    "20190418_MW66-9803"   
# [19] "20190418_MW66-9804"    "20190423_MW66-9810"    "20190507_MW67-3711"    "20190507_MW67-3712"    "20190509_MW67-3715"    "20190509_MW67-3716"   
# [25] "20190514_MW67-3718"    "20190514_MW67-3719"    "20190514_MW68-0701"    "20190827_MW69-3809"   
# CO2 Conc. Set Point 
# [1] "20190528_MW68-0808"
# Actual CO2 Conc. 
# [1] "20190326_MW1236-17"    "20190328_MW1236-19"    "20190328_MW1236-20"    "20190328_MW1236-21"    "20190402_MW1236-22"    "20190402_MW1236-23"   
# [7] "20190404_MWNo Barcode" "20190409_MW1237-13"    "20190409_MW1237-15"    "20190411_MW1237-18"    "20190416_MW1237-19"    "20190416_MW66-9604"   
# [13] "20190418_MW66-9803"    "20190425_MW66-9818"    "20190502_MW67-3708"    "20190514_MW67-3718"    "20190516_MW68-0719"    "20190528_MW68-0808"   
# [19] "20190618_MW69-0108"    "20190627_MW69-0117"    "20190801_MW69-0216"    "20190827_MW69-3807"    "20190827_MW69-3809"    "20191008_MW70-2408"   

# not too concerned about the current temp or actual C02 conc. Those aren't goign to vary wildy, and it just happnes

# Co2 set point:
alldat1[apid == "20190528_MW68-0808", unique(`CO2 Conc. Set Point`), by = "srcf"] # just diff by a sig fig
# srcf    V1
# 1: TC_20190508_MW68-0808_13_00(000).csv 5.00%
# 2: TC_20190508_MW68-0808_13_01(000).csv 5.0 %

# digital high pass filter
alldat1[apid %in% c("20160621_MW 1072-9",  "20160317_MW 1076-38", "20151105_MW 1077-14"), unique(`Digital High Pass Filter`), by = "srcf"]
# srcf       V1
# 1: TC_MW 1072-9_20160608_20160621_13_00(000)_Batch Process (1 Files)(000)_Neural Statistics Compiler(000).csv     None
# 2:                              TC_MW 1072-9_20160608_20160621_13_01(000)_Neural Statistics Compiler(000).csv 5 Hz IIR
# 3:                             TC_MW 1076-38_20160303_20160317_14_00(000)_Neural Statistics Compiler(000).csv 5 Hz IIR
# 4:                             TC_MW 1076-38_20160303_20160317_14_01(001)_Neural Statistics Compiler(000).csv     None
# 5:                             TC_MW 1077-14_20151020_20151105_15_00(000)_Neural Statistics Compiler(000).csv 5 Hz IIR
# 6:                             TC_MW 1077-14_20151020_20151105_15_01(000)_Neural Statistics Compiler(000).csv     None
alldat1[, .N, by = c("Digital High Pass Filter") ]
# Digital High Pass Filter      N
# 1:               200 Hz IIR 123840
# 2:                 5 Hz IIR 295920
# 3:                     None   2160

# get a sense of how many settings change significanlty
for (setting in settings) {
  if (nrow(unique(alldat1[,..setting])) > 1) {
    print(alldat1[, .N, by = setting])
  }
}

# May 22, 2020
# for these differences in settings...
# if I do  t-test or something, how much woudl I be measuring diff in settings vs just diff in datasets/cultures?... don't know
alldat1[, paste0(unique(`Voltage Scale`),collapse=","), by = "dataset"]
# dataset                             V1
# 1:   APCRA2019 -5.48486178148311E-08 V/sample
# 2:     DNT2019 -5.48486178148311E-08 V/sample
# 3: ToxCast2016               6.5E-08 V/sample
alldat1[, paste0(unique(`AxIS Version`),collapse=","), by = "dataset"]
# dataset                V1
# 1:   APCRA2019           1.5.2.4
# 2:     DNT2019           1.5.2.4
# 3: ToxCast2016 2.1.1.16,2.0.2.11

# let's look at network burst differences
alldat1[grepl("network",tcpl_acsn), unique(tcpl_acsn)]
# [1] "NHEERL_MEA_acute_network_burst_percentage"            "NHEERL_MEA_acute_per_network_burst_spike_number_mean"
# [3] "NHEERL_MEA_acute_per_network_burst_spike_number_std" 
alldat1[tcpl_acsn == "NHEERL_MEA_acute_per_network_burst_spike_number_mean" & run_type == "baseline", .(min = min(activity_value, na.rm=T), med = median(activity_value,na.rm=T), 
                                                               max = max(activity_value, na.rm=T), num_na = sum(is.na(activity_value)),
                                                               pts_in_group = .N), 
        by = c("Minimum Number of Spikes (network bursts)")]
x5 <- alldat1[tcpl_acsn == "NHEERL_MEA_acute_per_network_burst_spike_number_mean" & run_type == "baseline" & `Minimum Number of Spikes (network bursts)` == 5 & !is.na(activity_value), activity_value]
x10 <- alldat1[tcpl_acsn == "NHEERL_MEA_acute_per_network_burst_spike_number_mean" & run_type == "baseline" & `Minimum Number of Spikes (network bursts)` == 10 & !is.na(activity_value), activity_value]


plot(density(x10), type = "l")
lines(density(x5))

t.test(x5, x10)
# pop means are probably identical...
# but what about the distributions?
# or is the 1 outlier in toxcast data set just reallythrowing it off?
# groups <- as.factor(c(rep("5",length(x5)), rep("10", length(x10))))
# vals <- factor()
# stripchart(groups ~ c(x5, x10))
# 
# x <- gl(n=4,k=25)


acsni <- "NHEERL_MEA_acute_per_network_burst_spike_number_mean"
stripchart(activity_value ~ `Minimum Number of Spikes (network bursts)`, 
           alldat1[tcpl_acsn == acsni & run_type == "baseline"],
           vertical=T,pch=1, method="jitter", main = paste0(acsni,"\nin baseline recording"))
boxplot(activity_value ~ `Minimum Number of Spikes (network bursts)`, 
        alldat1[tcpl_acsn == "NHEERL_MEA_acute_per_network_burst_spike_number_mean" & run_type == "baseline"],
        vertical=T,pch=1, method="jitter", main = paste0(acsni,"\nin baseline recording"))

acsni <- "NHEERL_MEA_acute_network_burst_percentage" 
stripchart(activity_value ~ `Minimum Number of Spikes (network bursts)`, 
           alldat1[tcpl_acsn == acsni & run_type == "baseline"],
           vertical=T,pch=1, method="jitter", main = paste0(acsni,"\nin baseline recording"))
boxplot(activity_value ~ `Minimum Number of Spikes (network bursts)`, 
        alldat1[tcpl_acsn ==acsni & run_type == "baseline"],
        vertical=T,pch=1, method="jitter", main = paste0(acsni,"\nin baseline recording"))
# hmm, these are really fairly similar

acsni <- "NHEERL_MEA_acute_per_network_burst_spike_number_std"
stripchart(activity_value ~ `Minimum Number of Spikes (network bursts)`, 
           alldat1[tcpl_acsn ==  acsni & run_type == "baseline" & activity_value < 3000],
           vertical=T,pch=1, method="jitter", main = paste0(acsni,"\nin baseline recording"))
boxplot(activity_value ~ `Minimum Number of Spikes (network bursts)`, 
        alldat1[tcpl_acsn == acsni & run_type == "baseline"& activity_value < 3000],
        vertical=T,pch=1, method="jitter", main = paste0(acsni,"\nin baseline recording"))

x5 <- alldat1[tcpl_acsn == acsni & run_type == "baseline" & `Minimum Number of Spikes (network bursts)` == 5 & !is.na(activity_value), activity_value]
x10 <- alldat1[tcpl_acsn == acsni & run_type == "baseline" & `Minimum Number of Spikes (network bursts)` == 10 & !is.na(activity_value), activity_value]
t.test(x5, x10)
