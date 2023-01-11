# trying again to compare with the flat file
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output")
library(data.table)

# Outliers removed in source_to_lvl0_nheerl_mea_acute.R --------------------------
# from "ToxCast MEA data Outliers and data check.docx" ( we do not have this file )
# remove columns 7-8 for plateID 'MW 1076-37' on 20160317
idx <- which(df$MEA_PLATE_ID_RUN1=='MW 1076-37' &
               df$EXPERIMENT_DATE=='20160317' &
               df$`MEA_WELL_ID _ALL_RUNS`%in%paste0(paste0(LETTERS[1:6],'0'),rep(7:8,each=6)))
df[idx, 14 + c(0*43 + 1:43, (0+3)*43 + 1:43)] <- NA # Run 1 baseline and dose columns

# remove columns 1-2 for plateID 'MW 1048-15' on 20160531
idx <- which(df$MEA_PLATE_ID_RUN3=='MW 1048-15' &
               df$EXPERIMENT_DATE=='20160531' &
               df$`MEA_WELL_ID _ALL_RUNS`%in%paste0(paste0(LETTERS[1:6],'0'),rep(1:2,each=6)))
df[idx, 14 + c(2*43 + 1:43, (2+3)*43 + 1:43)] <- NA # Run 3 baseline and dose columns
# -----------------------------------------------------------------------------------

load(file = "ToxCast2016_dat4_2020-06-08.RData")
plot(dat4[experiment.date == "20160317" & grepl("firing",acsn), .(log10(conc), rval)], xlab = "log10(conc)")
points(dat4[experiment.date == "20160317"& plate.id == "MW1076-37" & coli %in% c(7,8) & grepl("firing",acsn), .(log10(conc), rval)], col = "blue",pch=19)
title(main = "% Change in MFR in 20160317\n'Outlier' points in MW1076-37 columns 7 and 8 are in blue")
# okay, yeah, these are clear outliers. But really enough to remove?

plot(dat4[experiment.date == "20160317" & grepl("AB",acsn), .(log10(conc), rval)])
points(dat4[experiment.date == "20160317"& plate.id == "MW1076-37" & coli %in% c(7,8) & grepl("AB",acsn), .(log10(conc), rval)], col = "blue", pch = 19)
title(main = "CellTiter Blue Blank-Corrected Flourescence values in 20160317\n'Outlier' points in MW1076-37 columns 7 and 8 are in blue")

plot(dat4[experiment.date == "20160317" & grepl("burst_number",acsn), .(log10(conc), rval)], pch = 1)
points(dat4[experiment.date == "20160317"& plate.id == "MW1076-37" & coli %in% c(7,8) & grepl("burst_number",acsn), .(log10(conc), rval)], col = "blue", pch = 19)
# real toss up

plot(dat4[experiment.date == "20160531" & grepl("firing",acsn), .(log10(conc), rval)], xlab = "log10(conc)")
points(dat4[experiment.date == "20160531"& plate.id == "MW1048-15" & coli %in% c(1,2) & grepl("firing",acsn), .(log10(conc), rval)], col = "blue", pch = 19)
title(main = "% Change in MFR in 20160531\n'Outlier' points in MW1048-15 columns 1 and 2 are in blue")
dat4[experiment.date == "20160531"& plate.id == "MW1048-15" & coli %in% c(1,2) & grepl("firing",acsn), .(log10_conc = log10(conc), rval, treatment)][order(log10_conc)]
# this definitely does not need to be removed
# log10_conc        rval treatment
# 1:   1.397940    9.968940       BIC
# 2:   1.397940   -9.341801       BIC
# 3:  -2.698970  -10.407478      DMSO
# 4:  -2.698970   -6.028753      DMSO
# 5:  -2.698970 -100.000000      DMSO
# 6:   2.000000 -100.000000     LYSIS
# 7:   1.602060   20.064176  TX000815
# 8:   1.602060  -36.949220  TX000972
# 9:   1.480007    2.456390  TX004572
# 10:   1.397940  102.458344  TX009070
# 11:   1.602060  -52.718017  TX012340
# 12:   1.602060  -12.690720  TX015578
# the blue points are the lowest conc are DMSO wells. We know that there is sometimes a DMSO effect. Let's not try to cherry pick that out
dat4[treatment == "DMSO" & wllq == 1 & grepl("firing",acsn) & rval < -60]
# okay, I guess this really is an outlier DMSO well
# the BIC wells with almost no response are weird but again, that happens
dat4[treatment == "BIC" & wllq == 1 & grepl("firing",acsn), summary(rval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -94.73   10.94   40.77   49.06   75.76  249.23 

# I am feeling quite passionate about not removing outliers at the moment, just because they look bad.
# So I am leaving it as it is

load('ToxCast2016_dat1_2020-06-06.RData')
dat1[experiment.date == "20160317"& plate.id == "MW1076-37" & coli %in% c(7,8)]
dat1[experiment.date == "20160317"& plate.id == "MW1076-37" & coli %in% c(7,8) & grepl("firing",tcpl_acsn), summary(activity_value)]
rm(dat1)


# another look at the flat file, want to see where vals set to NA
flatfile <- "L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv"
dat <- read.csv(flatfile, stringsAsFactors = F)

# change plateID 'MW 1042-50' to 'MW 1044-1' for run 3 (said corrected in raw file, but still here)
# (taken from source_to_lvl0_nheerl_mea_aucte.R)
dat$MEA_PLATE_ID_RUN3[dat$MEA_PLATE_ID_RUN3=='MW 1042-50'] <- 'MW 1044-1'

# compare LDH values
id_dat <- dat[, c("EPA_SAMPLE_ID","CONCENTRATION","EXPERIMENT_DATE","MEA_PLATE_ID_RUN1","MEA_PLATE_ID_RUN2","MEA_PLATE_ID_RUN3","MEA_WELL_ID._ALL_RUNS")]
setDT(id_dat)
id_dat <- melt(id_dat, measure.vars = c("MEA_PLATE_ID_RUN1","MEA_PLATE_ID_RUN2","MEA_PLATE_ID_RUN3"), variable.factor = F, variable.name = "run", value.name = "plate.id")
LDH_dat <- dat[, c("EXPERIMENT_DATE","LDH_WELL__ID","LDH_.DEAD_RUN1","LDH_.DEAD_RUN2","LDH_.DEAD_RUN3")]
setDT(LDH_dat)
LDH_dat <- melt(LDH_dat, id.vars = c("LDH_WELL__ID","EXPERIMENT_DATE"), variable.factor = F, variable.name = "run", value.name = "rval")
LDH_dat[, run := sub("LDH_.DEAD_","",run)]
id_dat[, run := sub("MEA_PLATE_ID_","",run)]
setnames(LDH_dat, old = "LDH_WELL__ID", new = "well")
setnames(id_dat, old = "MEA_WELL_ID._ALL_RUNS", new = "well")
LDH_dat <- merge(id_dat, LDH_dat, by = c("run","well","EXPERIMENT_DATE"))

LDH_dat[, plate.id := sub(" ","",plate.id)]
LDH_dat[, date_plate := paste(EXPERIMENT_DATE, plate.id,sep="_")]

# assign coli and rowi
LDH_dat[, coli := as.numeric(sub("[[:alpha:]]*","",well))]
LDH_dat[, rowc := sub("[[:digit:]]*$","",well)]
LDH_dat[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
LDH_dat[, rowc := NULL]

# get LDH data from dat4
LDH_dat4 <- dat4[grepl("LDH",acsn)]
LDH_dat4[, date_plate := paste(experiment.date, plate.id,sep = "_")]

setdiff(LDH_dat4$date_plate, LDH_dat$date_plate)
# just 9 date plates, that is fine, don't need everythign to see the big picture

LDHm <- merge(LDH_dat, LDH_dat4, by = c("date_plate","plate.id","rowi","coli"), suffixes = c(".org",".new"))
plot(LDHm[, .(rval.org, rval.new)])
abline(a = 0, b= 1)
title(main = "LDH Percent of Total LDH\nNew values divided by median LYSIS well in exp date,\nOriginal values divided by single LYSIS well on plate")

# what are all of the 100 values?
LDHm[rval.org == 100, .N, by = c("treatment")]
# okay so these are almost all LYSIS wells. Since .org divided values by the LYSIS well on each plate, teh LYSIS wells by default were always 100
# but now, I divide by the median LYSIS well in the experiment date, so there is some variability there

plot(LDHm[rval.org < 60, .(rval.org, rval.new)])
abline(a = 0, b= 1)
# could be a better fit, but not terrible. Most wells have very little overall percent change, which is to be expected.


# compare conc's with what is in flat file




# 06/09/2020
# just want to see where values are NA, if those are like wllq notes
setDT(dat)
dat[, sum(is.na(.SD)), .SDcols = names(dat)[15:length(names(dat))]]
# I might just be okay with not doing this. What gain is there? Even if I find that there set some value to NA... will i accept it?
# I guess potentially, if I think it might be a wllq thing.

# are there any relevant NA values?
dat[is.na(MEA_NUMBER_OF_SPIKES_BASELINE_RUN_1), 1:25]
dat4[treatment == "TX001412" & experiment.date == "20151222" & plate.id == "MW1056-37" & grepl("firing",acsn)]
# there is an NA value for the same well!!

# re-run 6/10/2020
setwd("L:/Lab/ToxCast_Data/toxcast_data/files/nheerl_mea_acute/source")
# read source files(cyto has cytotoxicity endpoints), both are needed to clean up the data
cyto <- read.csv('ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv', check.names=F, stringsAsFactors=F)
df <- read.csv('ToxCast CC Burst Analysis_Network enabled.csv', check.names=F, stringsAsFactors=F)

# check if data is same between files
diff <- sapply(intersect(names(df), names(cyto)), function(x) all.equal(df[,x], cyto[,x]))
(cols <- names(diff)[which(diff!="TRUE")])
# [1] "CONCENTRATION"                                  
# [2] "MEA_AREA_UNDER_CROSS_CORRELATION_BASELINE_RUN_2"
# [3] "MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_4"    
# [4] "MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_5"

# replace concentration in df's with cyto's. Concentration in cyto already has concentration correction from stock
df$CONCENTRATION <- cyto$CONCENTRATION
# Replace the remaining columns with differences
df$MEA_AREA_UNDER_CROSS_CORRELATION_BASELINE_RUN_2 <- cyto$MEA_AREA_UNDER_CROSS_CORRELATION_BASELINE_RUN_2
df$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_4 <- cyto$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_4
df$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_5 <- cyto$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_5

# corrections

#cyto[480:485, 143:148]; df[480:485, 143:148] #Inserted As and NAs shifting data
#Comparison: both datasets have data shift b/w rows 481-528 and columns 144-272, but only df has rest of data
#cyto[480:485, 268:274]; df[480:485, 268:274] #Cyto shifted data replaced with cytotox data
df[481:528,144:272] <- df[481:528,146:274]

# remove extra columns
df[,(ncol(df)-1):ncol(df)] <- NULL
# fix SampleIDs
df$EPA_SAMPLE_ID[df$EPA_SAMPLE_ID=='BC'] <- 'BIC'
# fix PlateIDs
df$MEA_PLATE_ID_RUN1[df$MEA_PLATE_ID_RUN1=='MW1068-24'] <- 'MW 1068-24'

# from "ToxCast MEA data Outliers and data check.docx" ( we do not have this file )
# (I am removing this part)

# change plateID 'MW 1042-50' to 'MW 1044-1' for run 3 (said corrected in raw file, but still here)
df$MEA_PLATE_ID_RUN3[df$MEA_PLATE_ID_RUN3=='MW 1042-50'] <- 'MW 1044-1'

# add in cyto data
df <- cbind(df, cyto[,names(cyto)[!names(cyto)%in%names(df)]])

# remove earlier runs of any samples that are duplicated
# (I am removing this part)

# add missing underscore to single endpoint (simplifies scripting below)
names(df)[14+25+43*(0:5)] <- sub('DOSE','_DOSE',sub('BASELINE','_BASELINE',names(df)[14+25+43*(0:5)]))

# Reshape - split the data by run (1:3)

dat             <- data.frame('spid' = rep(df$EPA_SAMPLE_ID, 3), stringsAsFactors = FALSE)
dat$apid        <- c(df$MEA_PLATE_ID_RUN1, df$MEA_PLATE_ID_RUN2, df$MEA_PLATE_ID_RUN3)
dat$well_id     <- rep(df$`MEA_WELL_ID _ALL_RUNS`, 3)
dat$rowi        <- substring(dat$well_id, 1, 1)
dat$coli        <- sub(".*0", "", dat$well_id) 
dat$wllt        <- rep("t", 3*nrow(df))
dat$wllq        <- rep(1, 3*nrow(df))
dat$conc        <- rep(df$CONCENTRATION, 3)
dat$run         <- c(rep(1, nrow(df)), rep(2, nrow(df)), rep(3, nrow(df)))

endpoints <- sub('_BASELINE_RUN_1','',names(df)[grepl('_BASELINE_RUN_1', names(df))])
for (endpoint in endpoints) {
  dat[, paste0(endpoint,'_BASELINE')] <- as.numeric(unlist(df[, paste0(endpoint,'_BASELINE_RUN_', 1:3)]))
  dat[, paste0(endpoint,'_DOSE')]     <- as.numeric(unlist(df[, paste0(endpoint,'_DOSE_RUN_', 4:6)]))
}

# add cytotox data
dat$LDH <- c(df$`LDH_%DEAD_RUN1`, df$`LDH_%DEAD_RUN2`, df$`LDH_%DEAD_RUN3`)
dat$AB  <- c(df$`AB_%DEAD_RUN1`,  df$`AB_%DEAD_RUN2`,  df$`AB_%DEAD_RUN3`)

elect <- which(dat$MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE < 10)
dat[elect, 10:ncol(dat)] <- NA

# obtaining baseline-subtracted response values
for (endpoint in endpoints) {
  base  <- dat[,paste0(endpoint,'_BASELINE')]
  dose  <- dat[,paste0(endpoint,'_DOSE')]
  diff  <- dose - base
  # Remove outliers
  #mu    <- mean(diff, na.rm=T)
  #sd    <- sd(diff, na.rm=T)
  #idx   <- which(diff < mu-6*sd | diff > mu+6*sd)
  
  #diff[idx] <- NA
  # Add to dat
  dat[,paste0(endpoint,'_DIFF')] <- diff
}

# excluding, baseline, and dose raw values
dat <- dat[, c(1:9, 96:ncol(dat))]
colnames(dat)<- sub("_DIFF", "", colnames(dat))
# added by Amy
dat$experiment.date <- rep(df$EXPERIMENT_DATE, 3)
# melt the data by assay component
dat_melted <- melt(dat, id = c("spid", "apid", "experiment.date", "well_id", "rowi", "coli", "wllt", "wllq", "conc", "run"))

# change rows from letters to numbers
setDT(dat_melted)

dat_melted[rowi == 'A', rowi := '1']
dat_melted[rowi == 'B', rowi := '2']
dat_melted[rowi == 'C', rowi := '3']
dat_melted[rowi == 'D', rowi := '4']
dat_melted[rowi == 'E', rowi := '5']
dat_melted[rowi == 'F', rowi := '6']

# change coli, and rowi from character to numeric
dat_melted$coli <- as.numeric(dat_melted$coli)
dat_melted$rowi <- as.numeric(dat_melted$rowi)

# change value to rval
colnames(dat_melted)[colnames(dat_melted) == 'value'] <- 'rval'

# assign acsn to the registered components, acnm are used as acsn
# only components that were pipelined by the group were registered so that level 5 results can be compared with diff

acnm <- tcplLoadAcid(fld = 'aid', val = 573)[acid %in% 2447:2461]$acnm
#acnm
#[1] "NHEERL_MEA_acute_spike_number"                        "NHEERL_MEA_acute_firing_rate_mean"                   
#[3] "NHEERL_MEA_acute_burst_number"                        "NHEERL_MEA_acute_burst_duration_mean"                
#[5] "NHEERL_MEA_acute_per_burst_spike_number_mean"         "NHEERL_MEA_acute_interburst_interval_mean"           
#[7] "NHEERL_MEA_acute_burst_percentage_mean"               "NHEERL_MEA_acute_burst_percentage_std"               
#[9] "NHEERL_MEA_acute_per_network_burst_spike_number_mean" "NHEERL_MEA_acute_per_network_burst_spike_number_std" 
#[11] "NHEERL_MEA_acute_bursting_electrodes_number_mean"     "NHEERL_MEA_acute_network_burst_percentage"           
#[13] "NHEERL_MEA_acute_cross_correlation_area"              "NHEERL_MEA_acute_cross_correlation_HWHM"             
#[15] "NHEERL_MEA_acute_synchrony_index"      
unique(dat_melted$variable)
dat_melted[variable == "MEA_NUMBER_OF_SPIKES", acsn := "NHEERL_MEA_acute_spike_number" ]
dat_melted[variable == "MEA_MEAN_FIRING_RATE_HZ", acsn := "NHEERL_MEA_acute_firing_rate_mean" ]
dat_melted[variable == "MEA_NUMBER_OF_BURSTS", acsn := "NHEERL_MEA_acute_burst_number" ]
dat_melted[variable == "MEA_BURST_DURATION_AVG_ (SEC)", acsn := "NHEERL_MEA_acute_burst_duration_mean" ]
dat_melted[variable == "MEA_NUMBER_OF_SPIKES_PER_BURST_AVG", acsn := "NHEERL_MEA_acute_per_burst_spike_number_mean" ]
dat_melted[variable == "MEA_INTER_BURST_INTERVAL_AVG_(SEC)", acsn := "NHEERL_MEA_acute_interburst_interval_mean" ]
dat_melted[variable == "MEA_BURST_PERCENTAGE_AVG_(SEC)", acsn := "NHEERL_MEA_acute_burst_percentage_mean" ]
dat_melted[variable == "MEA_BURST_PERCENTAGE_STD_(SEC)", acsn := "NHEERL_MEA_acute_burst_percentage_std" ]
dat_melted[variable == "MEA_NUMBER_OF_SPIKES_PER_NETWORK_BURST_ AVG", acsn := "NHEERL_MEA_acute_per_network_burst_spike_number_mean" ]
dat_melted[variable == "MEA_NUMBER_OF_SPIKES_PER_NETWORK_BURST_ STD", acsn := "NHEERL_MEA_acute_per_network_burst_spike_number_std" ]
dat_melted[variable == "MEA_NUMBER_OF_ELECS_PARTICIPATING_IN_BURST_AVG", acsn := "NHEERL_MEA_acute_bursting_electrodes_number_mean" ]
dat_melted[variable == "MEA_NETWORK_BURST_PERCENTAGE", acsn := "NHEERL_MEA_acute_network_burst_percentage" ]
dat_melted[variable == "MEA_AREA_UNDER_CROSS_CORRELATION", acsn := "NHEERL_MEA_acute_cross_correlation_area" ]
dat_melted[variable == "MEA_HALF_WIDTH_AT_HALF_HEIGHT_OF_CROSS_CORRELATION", acsn := "NHEERL_MEA_acute_cross_correlation_HWHM" ]
dat_melted[variable == "MEA_SYNCHRONY_INDEX", acsn := "NHEERL_MEA_acute_synchrony_index" ]
# adding in 06/10/2020
dat_melted[variable == "LDH", acsn := "NHEERL_MEA_acute_LDH" ]
dat_melted[variable == "AB", acsn := "NHEERL_MEA_acute_AB" ]

str(dat_melted)

# fixing a few things that I know are off
dat_melted[experiment.date == "20150804" & apid == "MW 1079-13", apid := "MW1073-13"]
dat_melted[experiment.date == "20151201" & apid == "MW 186-25", apid := "MW1086-25"]
dat_melted[experiment.date == "20160614" & apid == "MW 1072-8", apid := "MW1072-08"]
dat_melted[experiment.date == "20151201" & apid == "MW1086-25", apid := "MW1086-26"] # see notes in fix_typos_toxcast2016.R
dat_melted[experiment.date == "20151208", apid := sub("MW 1068","MW1086",apid)] # see notes in fix_typos_toxcast2016.R

# now let's compare to my data
dat_melted[is.na(rval)]
dat_melted[, date_plate := paste(experiment.date, sub(" ","",apid),sep = "_")]
dat4[, date_plate := paste(experiment.date, plate.id, sep = "_")]

setdiff(unique(dat_melted$date_plate), unique(dat4$date_plate))
setdiff(unique(dat4$date_plate), unique(dat_melted$date_plate))
# "20160607_MW1062-28" "20160607_MW1063-1"  "20160607_MW1063-3" # this is the culture date that I added, so it is not in the flat file

useplates <- intersect(unique(dat_melted$date_plate), unique(dat4$date_plate))

dat_melted[is.na(rval) & date_plate %in% useplates, .N]
dat4[is.na(rval) & date_plate %in% useplates, .N] # significantly less here...

dat_melted[is.na(rval) & date_plate %in% useplates, .N, by = "acsn"]
# oh, let's remove the rows with NA acsn
odat <- dat_melted[!is.na(acsn)]

odat[is.na(rval) & date_plate %in% useplates, .N]
dat4[is.na(rval) & date_plate %in% useplates, .N]

odat[, experiment.date := as.character(experiment.date)]

mdat <- merge(odat[date_plate %in% useplates], dat4[date_plate %in% useplates], by = c("experiment.date","rowi","coli","acsn","date_plate"),
              suffixes = c(".org",".new"))
nrow(odat) # 149040
nrow(dat4) # 171360
nrow(mdat) # 149040

mdat[is.na(rval.org) & is.na(rval.new), .N] # 5388 are in agreement
mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 0, .N] # 3746 also essentially in agreement

mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, .N] # 290. Okay, so these are NA in the original, but not in my notes

mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, .N, by = "experiment.date"] # spread out across experiments, except for 20150618...
mdat[experiment.date == "20150618" & is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, .(treatment, date_plate, rval.org, rval.new)]

# I am noticing that I set wllq to zero here for a lot of wells, too.
mdat[experiment.date == "20150618", .N, by = c("wllq.new","wllq_notes")]
# wllq.new                                                                            wllq_notes    N
# 1:        1                                                                                       1111
# 2:        0                                                         Baseline MFR < 0.6377603 Hz;   527
# 3:        0             # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz;   389
# 4:        0                                             Baseline MFR < 0.6377603 Hz; rval is NA;    28
# 5:        0                                          # of AE less than 10 in baseline recording;    45
# 6:        0 # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz; rval is NA;    31
# 7:        0                                                                          rval is NA;    29

# did they just set all points to NA for this experiment date?
mdat[experiment.date == "20150618", unique(rval.org)] # no, there are several points kept here

# are all of the NAs on one plate?
mdat[experiment.date == "20150618" & is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, .N, by = c("date_plate","rowi","coli")]
# okay, where all 15 endpoints are NA, I think this is a wllq thing

mdat[experiment.date == "20150618" & date_plate == "20150618_MW1068-23" & rowi == 2 & coli == 3]
# ultimately what will happen is, when we go back I will read the lab notebook and check all of this out

# okay, I am starting to feel unsure again. 

# where rval is set to NA... was the actual rval really low or something?
mdat[experiment.date == "20150618" & is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, hist(rval.new)]

load("ToxCast2016_dat1_2020-06-06.RData")
dat1[experiment.date == "20150618" & plate.id == "MW1068-23" & rowi == 2 & coli == 3, .(tcpl_acsn, activity_value)]
# the mfr in this well is soooo close to the threshold I am using. There are 13 AE in this well
mfr_lower_threshold <- 0.6377603

# let's get a summary of the aseline mfr's in these problem NA wells
dat1[, date_plate := paste(experiment.date, plate.id, sep = "_")]
mdat2 <- merge(mdat, dat1[date_plate %in% useplates & run_type == "baseline", .(experiment.date, rowi, coli, acsn = tcpl_acsn, date_plate, rval.baseline = activity_value)])

mdat2[, well_id := paste(date_plate, rowi, coli, sep = "_")]
prob_wells <- mdat2[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, well_id]
mdat2[well_id %in% prob_wells & grepl("firing",acsn), summary(rval.baseline)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6465  1.1053  1.6979  1.6864  2.1724  2.9596
# huh, interesting... So the MFR is not universally small here

# big question:
# for the wells where just 1 random endpoint is NA... that seems sus to me
mdat[experiment.date == "20150804" & plate.id == "MW1073-13" & rowi == 2 & coli == 2] # IBI is removed, I calculated percent change at -55%
mdat[experiment.date == "20150804" & plate.id == "MW1073-13" & rowi == 5 & coli == 2] # IBI again is NA
mdat[experiment.date == "20150827" & plate.id == "MW1042-50" & rowi == 1 & coli == 2] # IBI again is NA, and net burst std is NA for rval.org and rval.new

# 65 of these "problem" cases pertain to the IBI
mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, .(.N), by = c("acsn")]
# acsn  N
# 1:            NHEERL_MEA_acute_interburst_interval_mean 65
# 2:                 NHEERL_MEA_acute_burst_duration_mean 16
# 3:                        NHEERL_MEA_acute_burst_number 16
# 4:               NHEERL_MEA_acute_burst_percentage_mean 19
# 5:                NHEERL_MEA_acute_burst_percentage_std 17
# 6:     NHEERL_MEA_acute_bursting_electrodes_number_mean 15
# 7:              NHEERL_MEA_acute_cross_correlation_HWHM 16
# 8:              NHEERL_MEA_acute_cross_correlation_area 16
# 9:                    NHEERL_MEA_acute_firing_rate_mean 16
# 10:            NHEERL_MEA_acute_network_burst_percentage 15
# 11:         NHEERL_MEA_acute_per_burst_spike_number_mean 16
# 12: NHEERL_MEA_acute_per_network_burst_spike_number_mean 15
# 13:  NHEERL_MEA_acute_per_network_burst_spike_number_std 16
# 14:                        NHEERL_MEA_acute_spike_number 16
# 15:                     NHEERL_MEA_acute_synchrony_index 16

# how many appear to be an entire well?
prob_wells_whole <- mdat2[well_id %in% prob_wells & is.na(rval.org), .N, by = "well_id"][N == 15]
prob_wells <- mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, well_id]
mdat[, well_id := paste(date_plate, rowi, coli, sep = "_")]
prob_wells_whole <- mdat[well_id %in% prob_wells & is.na(rval.org), .N, by = "well_id"][N == 17]
# 15 wells are mysteriously NA for all 15 endpoints (cyto dat is not included!)

mdat2[well_id %in% setdiff(prob_wells,prob_wells_whole$well_id), .(.N), by = c("acsn")]

# LDH adn AB wells
mdat[grepl("LDH",acsn) & is.na(rval.org) & wllq.new == 1 & !(well_id %in% prob_wells_whole$well_id)]
mdat[grepl("AB",acsn) & is.na(rval.org) & wllq.new == 1 & !(well_id %in% prob_wells_whole$well_id), .N, by = c("plate.id","experiment.date")]
# plate.id  N
# 1: MW1076-38 48
# all 48 from the same plate
mdat[grepl("AB",acsn), summary(rval.new)]
mdat[grepl("AB",acsn) & is.na(rval.org) & wllq.new == 1 & !(well_id %in% prob_wells_whole$well_id), summary(rval.new)]

mdat[grepl("AB",acsn) & rval.new < 0]

# checkout/estimate the new % values on this plate.
med_rval <- mdat[grepl("AB",acsn) & experiment.date == "20151027" & treatment == "DMSO" & wllq.new == 1, median(rval.new)]
mdat[grepl("AB",acsn) & experiment.date == "20151027" & plate.id == "MW1076-38", rval.new.percent := (rval.new / med_rval)*100]
mdat[grepl("AB",acsn) & experiment.date == "20151027" & plate.id == "MW1076-38", rval.new.percent]

# last question: The concentrations
mdat[conc.org != conc.new]
# Empty data.table (0 rows and 26 cols): experiment.date,rowi,coli,acsn,date_plate,spid.org...
# yeet! almost seems too good to be true...
# oh wait, I copied the conc data from here, lol
mdat[is.na(conc.org), unique(conc.new), by = "treatment"]
# treatment      V1
# 1:      DMSO   0.002
# 2:       BIC  25.000
# 3:     LYSIS 100.000
# that is it.


# 06/11/2020
dat_melted[experiment.date == "20150618" & is.na(rval), .N, by = c("rowi","coli","apid","run")]
# rowi coli       apid run  N
# 1:    3    8 MW 1068-24   1 33
# 2:    6    1 MW 1068-24   1 33
# 3:    5    2  MW 1045-2   2 33
# 4:    1    2 MW 1068-23   3 33
# 5:    5    2 MW 1068-23   3 31
# 6:    6    1 MW 1068-23   3 31
# 7:    5    2 MW 1068-24   1 28
# 8:    5    3  MW 1045-2   2 18
# 9:    6    1  MW 1045-2   2 18
# 10:    5    3 MW 1068-23   3 28
# 11:    6    2 MW 1068-23   3 22
# 12:    4    2 MW 1068-23   3 12
# 13:    6    2  MW 1045-2   2  1
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")

load("output/ToxCast2016_dat1_2020-06-06.RData")
dat1[experiment.date == "20150618" & plate.id == "MW1068-24" & rowi == 3 & coli == 8 & run_type == "baseline", .(run_type, wllq, wllq_notes, tcpl_acsn)]
# totally clean....
# determine: are these wells truly NA?
# do I truly not know why these wells are NA?
# how were these values 0?
