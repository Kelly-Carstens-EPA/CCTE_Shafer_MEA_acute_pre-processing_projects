# seeing what is NA in the flat file again
# Ultimately, I found 4 instances of unexpected NA's in flat file:
# Experiment date 20150618, plate MW 1068-24, well C8 - all neural stats endpoints are NA or 0 at baseline, even though I found non-NA values for these points
# Experiment date 20150730, plate MW 1041-25, well F8 - almost all baseline values are blank
# Experiment date 20151222, plate MW 1060-36, run3, wells E3 and F3 - LDH values are removed
# all other differences seem to be accidental changes in the values or intentional removal of a few endpoints, which I will not follow
# (only concerned with acutal well quality issues, not purported outlier points)

library(data.table)

# Adapted from source_to_lvl0_nheerl_mea_acute.R
# to melt and compare the data from flat file with dat4
# -------------------------------------------------------------------------------------------------------

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
# added by Amy
dat$experiment.date <- rep(df$EXPERIMENT_DATE, 3)
# reorder
dat <- dat[, c(1:9, 98, 10:97)]

# -------------------------------------------------------------------------------------------------------
# 06/12/2020

# Question 1: are there wells that are truly all NA in teh flat file (or a combo NA and 0) that are diff from what I have?
# (I am kinda curious, but don't care as much about individual endpoint/wells where the value is mysteriously NA, but not for all endpoints)

welldat <- data.table()
for (i in 1:nrow(dat)) {
  sum_na <- sum(is.na(dat[i, 11:ncol(dat)]))
  sum_zero <- sum(dat[i, 11:ncol(dat)] == 0, na.rm = T)
  welldat <- rbind(welldat, c(dat[i, 1:10], "sum_na" = sum_na, "sum_zero" = sum_zero))
  if (i %% 100 == 0) {
    print(i)
    print(Sys.time())
  }
}

welldat[, summary(sum_na)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   2.235   0.000  86.000 
welldat[sum_na>0, summary(sum_na)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    4.00   28.00   21.92   33.00   86.00
# there are 88 total endpoint cols, including LDH and AB
welldat[sum_na == 86] # only 1 well
welldat[sum_na + sum_zero >= 86] #23 wells in this category.

# clean up welldat so that I can compare
welldat[rowi == 'A', rowi := '1']
welldat[rowi == 'B', rowi := '2']
welldat[rowi == 'C', rowi := '3']
welldat[rowi == 'D', rowi := '4']
welldat[rowi == 'E', rowi := '5']
welldat[rowi == 'F', rowi := '6']

welldat[experiment.date == "20150804" & apid == "MW 1079-13", apid := "MW1073-13"]
welldat[experiment.date == "20151201" & apid == "MW 186-25", apid := "MW1086-25"]
welldat[experiment.date == "20160614" & apid == "MW 1072-8", apid := "MW1072-08"]
welldat[experiment.date == "20151201" & apid == "MW1086-25", apid := "MW1086-26"] # see notes in fix_typos_toxcast2016.R
welldat[experiment.date == "20151208", apid := sub("MW 1068","MW1086",apid)] # see notes in fix_typos_toxcast2016.R
welldat[, fullid := paste(experiment.date, sub(" ","",apid),rowi,coli,sep = "_")]
allout <- welldat[sum_na + sum_zero >= 86, fullid]

# compare with dat4
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")
load("output/ToxCast2016_dat4_2020-06-10.RData")
dat4[, fullid := paste(experiment.date, plate.id,rowi,coli,sep = "_")]
dat4[fullid %in% allout, unique(wllq), by = "fullid"]
# all 23 are present. There is only 1 well that I have set to wllq=1
dat4[fullid == "20150618_MW1068-24_3_8"]
dat[experiment.date == "20150618" & apid == "MW 1068-24" & rowi == "C" & coli == "8"]
# all values are NA or 0 here, except for cytodat
# - verify visually in flat file- > just checked, yes, there are just lots of empty rows there, plus a few zeroes
# - see if any notes in js's file -> no notes. I see that the second plate in this file has nAE at zero though... so maybe accident?
# but yay, only 1 instance!!

# -------------------------------------------------------------------------------------------------------

# Question 2: are there differences in our nAE values? Was that an odd well quality method of communication/control?

# where nAE is NA or less than 10, but I have wllq set to 1
dat[MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE < 10 | is.na(MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE)]
# clean up for comparison again
dat[rowi == 'A', rowi := '1']
dat[rowi == 'B', rowi := '2']
dat[rowi == 'C', rowi := '3']
dat[rowi == 'D', rowi := '4']
dat[rowi == 'E', rowi := '5']
dat[rowi == 'F', rowi := '6']
dat[experiment.date == "20150804" & apid == "MW 1079-13", apid := "MW1073-13"]
dat[experiment.date == "20151201" & apid == "MW 186-25", apid := "MW1086-25"]
dat[experiment.date == "20160614" & apid == "MW 1072-8", apid := "MW1072-08"]
dat[experiment.date == "20151201" & apid == "MW1086-25", apid := "MW1086-26"] # see notes in fix_typos_toxcast2016.R
dat[experiment.date == "20151208", apid := sub("MW 1068","MW1086",apid)] # see notes in fix_typos_toxcast2016.R
dat[, fullid := paste(experiment.date, sub(" ","",apid),rowi,coli,sep = "_")]
lowae <- dat[MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE < 10 | is.na(MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE), fullid] # 313 data rows
dat4_lowae <- dat4[fullid %in% lowae, .(wllq = paste0(unique(wllq),collapse=","), wllq_notes = paste0(unique(wllq_notes),collapse=",")), by = "fullid"]
dat4_lowae[grepl("1",wllq)] # there are 16 cases the nAE is low in flat file, but not in my data. Including the single well above with all NA's
# - check these cases out visually in the flat file
# - check out notes in conc resp log
# - was it borderline?

dat[fullid == "20150618_MW1045-2_4_1", MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE] # confirmed in flat file this is 9
# just checked neural stats compiler files. This should definitely not be 9. 
# it seems highly unlikely that e.g. Kathleen would manually change a value from "16" to "9" as a weird way to mark the well quality
# this was definitely just an accident.
# I will check one more instance

# another instance
dat[fullid == "20150929_MW1077-15_3_2", MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE] # 7. Just checked neural stats file, this is not correct

# the rest of the cases:
dat[fullid %in% missedlowae$fullid, .(fullid, MEA_NUMBER_OF_ACTIVE_ELECTRODES_DOSE, MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE)]
# for all except the case below, the nAE at baselien is a not NA. It is just a small number. And i have verified in 2 cases above, I think these are just accidents.

# 20150730_MW1041-25_6_8    -> in flat file, all baseline values except for sync index are removed (not even a 0). This looks intentional
# maybe I should check welldat where only all baseline endpoints are na

# -------------------------------------------------------------------------------------------------------

# Question3 - are there some wells were all (or most) of the baseline only endpoints were removed, as a well quality note?

welldat[sum_na >= 43]
# or, as in the case of 20150730_MW1041-25_6_8, where almost all baseline endpoints are na
na40 <- welldat[sum_na >= 40, fullid]
dat4[fullid %in% na40, .(wllq = paste0(unique(wllq),collapse=","), wllq_notes = paste0(unique(wllq_notes),collapse=",")), by = "fullid"]
# only 2 cases where not all wllq==0:
# 20150730_MW1041-25_6_8 - as found above
# 20150618_MW1068-24_3_8 - also studied above, fully set to NAs and zero's at baseline and treated.

# there may be other instances where fewer endpoints were set to NA
# but I am less concerned about those, bc less likely to be a true well quality thing, and more likely just an outlier or mistake.

# -------------------------------------------------------------------------------------------------------
# continuing excerpt adapted from source_to_lvl0_nheerl_mea_acute.R

# REMOVING - for now. Want to see actual NA values
# elect <- which(dat$MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE < 10)
# dat[elect, 10:ncol(dat)] <- NA

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

# -------------------------------------------------------------------------------------------------------
# 06/11/2020 - comparing dat_melted to dat4
# note some of the below differences are actually due to points removed because of differences in nAE
# which resulted in the values becoming NA, even though those values are not NA in the flat file 
str(dat_melted)

# fixing a few things that I know are off
dat_melted[experiment.date == "20150804" & apid == "MW 1079-13", apid := "MW1073-13"]
dat_melted[experiment.date == "20151201" & apid == "MW 186-25", apid := "MW1086-25"]
dat_melted[experiment.date == "20160614" & apid == "MW 1072-8", apid := "MW1072-08"]
dat_melted[experiment.date == "20151201" & apid == "MW1086-25", apid := "MW1086-26"] # see notes in fix_typos_toxcast2016.R
dat_melted[experiment.date == "20151208", apid := sub("MW 1068","MW1086",apid)] # see notes in fix_typos_toxcast2016.R

# now let's compare to my data
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")
dat_melted[, date_plate := paste(experiment.date, sub(" ","",apid),sep = "_")]
load("output/ToxCast2016_dat4_2020-06-10.RData")
dat4[, date_plate := paste(experiment.date, plate.id, sep = "_")]

setdiff(unique(dat_melted$date_plate), unique(dat4$date_plate))
setdiff(unique(dat4$date_plate), unique(dat_melted$date_plate))
# "20160607_MW1062-28" "20160607_MW1063-1"  "20160607_MW1063-3" # this is the culture date that I added, so it is not in the flat file

# curious - how many of these wells am I including?
dat4[grepl("20160607",apid), paste0(unique(wllq),collapse=","), by = c("rowi","coli")] # overall, only 1 row removed for all endpoints

useplates <- intersect(unique(dat_melted$date_plate), unique(dat4$date_plate))

dat_melted[is.na(rval) & date_plate %in% useplates, .N]
dat4[is.na(rval) & date_plate %in% useplates, .N] # significantly less here...

dat_melted[is.na(rval) & date_plate %in% useplates, .N, by = "acsn"]
# oh, let's remove the rows with NA acsn
odat <- dat_melted[!is.na(acsn)]
odat[, experiment.date := as.character(experiment.date)]

mdat <- merge(odat[date_plate %in% useplates], dat4[date_plate %in% useplates], by = c("experiment.date","rowi","coli","acsn","date_plate"),
              suffixes = c(".org",".new"))

# 1 - see how many values are na
odat[is.na(rval), .N, by = "acsn"]
# for how many wells are all endpoints NA?
well_summary <- odat[, sum(!is.na(rval)), by = c("experiment.date","apid","rowi","coli")]
well_summary[V1 == 0]
# empty... hmm.

mdat[is.na(rval.org) & wllq.new == 1, .N, by = c("experiment.date","plate.id","rowi","coli")]
# in 108 wells, some endpoints are NA where wllq.new is 1. But for several, it seems that just 1 endpoint is NA

mdat[is.na(rval.org) & wllq.new == 1, .N, by = c("acsn")]

# let me compare the nAE, since I think there were differences there
dat_melted[variable == "MEA_NUMBER_OF_ACTIVE_ELECTRODES", summary(rval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -16.000  -1.000   0.000  -1.797   0.000  16.000       2 
# woah, why are there negatives in here? Right, this is the "diff" value
# (re-calculated dat up to elect point)
summary(dat[, c("MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE")])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00   15.00   16.00   15.11   16.00   16.00       2 
dat[dat$MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE < 10, c("spid","apid","well_id","MEA_NUMBER_OF_ACTIVE_ELECTRODES_BASELINE")]

dat4[grepl("# of AE less than 10",wllq_notes), .N, by = c("plate.id", "rowi", "coli")]
