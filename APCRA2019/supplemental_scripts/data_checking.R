# verifying the APCRA data with what is the flat file

# load dat4
load(file = paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData"))

# EXTRA CHECKING with what is in flat file
flatfile <- as.data.table(read_excel(path = "L:/Lab/NHEERL_MEA/Project TSCA_APCRA/APCRA_MEA_Data_With_Sync_Index_&_Cytotoxicity.xlsx"))
df <- as.data.frame(flatfile)

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

# add in LDH/AB:
dat[, paste0('LDH_%DEAD')] <- as.numeric(unlist(df[, paste0('LDH_%DEAD_RUN_', 1:3)]))
dat[, paste0('CTB_%VIABLE')] <- as.numeric(unlist(df[, paste0('CTB_%VIABLE_RUN_', 1:3)]))

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
# dat <- dat[, c(1:9, 96:ncol(dat))]
dat <- dat[, names(dat)[!grepl("(DOSE)|(BASELINE)",names(dat))]]
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

# acnm <- tcplLoadAcid(fld = 'aid', val = 573)[acid %in% 2447:2461]$acnm
#acnm
#[1] "NHEERL_MEA_acute_spike_number"                        "NHEERL_MEA_acute_firing_rate_mean"                   
#[3] "NHEERL_MEA_acute_burst_number"                        "NHEERL_MEA_acute_burst_duration_mean"                
#[5] "NHEERL_MEA_acute_per_burst_spike_number_mean"         "NHEERL_MEA_acute_interburst_interval_mean"           
#[7] "NHEERL_MEA_acute_burst_percentage_mean"               "NHEERL_MEA_acute_burst_percentage_std"               
#[9] "NHEERL_MEA_acute_per_network_burst_spike_number_mean" "NHEERL_MEA_acute_per_network_burst_spike_number_std" 
#[11] "NHEERL_MEA_acute_bursting_electrodes_number_mean"     "NHEERL_MEA_acute_network_burst_percentage"           
#[13] "NHEERL_MEA_acute_cross_correlation_area"              "NHEERL_MEA_acute_cross_correlation_HWHM"             
#[15] "NHEERL_MEA_acute_synchrony_index"      
sort(as.character(unique(dat_melted$variable)))
dat_melted[variable == "MEA_NUMBER_OF_SPIKES", acsn := "NHEERL_MEA_acute_spike_number" ]
dat_melted[variable == "MEA_MEAN_FIRING_RATE_(Hz)", acsn := "NHEERL_MEA_acute_firing_rate_mean" ]
dat_melted[variable == "MEA_NUMBER_OF_BURSTS", acsn := "NHEERL_MEA_acute_burst_number" ]
dat_melted[variable == "MEA_BURST_DURATION_AVG_(SEC)", acsn := "NHEERL_MEA_acute_burst_duration_mean" ]
dat_melted[variable == "MEA_NUMBER_OF_SPIKES_PER_BURST_AVG", acsn := "NHEERL_MEA_acute_per_burst_spike_number_mean" ]
dat_melted[variable == "MEA_INTER_BURST_INTERVAL_AVG_(SEC)", acsn := "NHEERL_MEA_acute_interburst_interval_mean" ]
dat_melted[variable == "MEA_BURST_PERCENTAGE_AVG", acsn := "NHEERL_MEA_acute_burst_percentage_mean" ]
dat_melted[variable == "MEA_BURST_PERCENTAGE_STD", acsn := "NHEERL_MEA_acute_burst_percentage_std" ]
dat_melted[variable == "MEA_NUMBER_OF_SPIKES_PER_NETWORK_BURST_AVG", acsn := "NHEERL_MEA_acute_per_network_burst_spike_number_mean" ]
dat_melted[variable == "MEA_NUMBER_OF_SPIKES_PER_NETWORK_BURST_STD", acsn := "NHEERL_MEA_acute_per_network_burst_spike_number_std" ]
dat_melted[variable == "MEA_NUMBER_OF_ELECS_PARTICIPATING_IN_BURST_AVG", acsn := "NHEERL_MEA_acute_bursting_electrodes_number_mean" ]
dat_melted[variable == "MEA_NETWORK_BURST_PERCENTAGE", acsn := "NHEERL_MEA_acute_network_burst_percentage" ]
# some endpoints are not in flat file:
# dat_melted[variable == "MEA_AREA_UNDER_CROSS_CORRELATION", acsn := "NHEERL_MEA_acute_cross_correlation_area" ]
# dat_melted[variable == "MEA_HALF_WIDTH_AT_HALF_HEIGHT_OF_CROSS_CORRELATION", acsn := "NHEERL_MEA_acute_cross_correlation_HWHM" ]
# dat_melted[variable == "MEA_SYNCHRONY_INDEX", acsn := "NHEERL_MEA_acute_synchrony_index" ]
dat_melted[variable == "CTB_%VIABLE", acsn := "NHEERL_MEA_acute_AB" ]
dat_melted[variable == "LDH_%DEAD", acsn := "NHEERL_MEA_acute_LDH" ]

str(dat_melted)
o_points <- dat_melted[is.na(acsn)]
dat_melted <- dat_melted[!is.na(acsn)]

# now let's compare to my data
dat_melted[is.na(rval)]
dat_melted[, date_plate := paste(experiment.date, sub(" ","",apid),sep = "_")]
dat4[, date_plate := paste(experiment.date, plate.id, sep = "_")]

setdiff(unique(dat_melted$date_plate), unique(dat4$date_plate))
setdiff(unique(dat4$date_plate), unique(dat_melted$date_plate))

# hmm, there are several NA experiment dates. Are all of the plates unique, can I get awa y with that?
dat_melted[, .N, by = "apid"]
dat4[, .N, by = "plate.id"]
# several questions - why only 240 - 480 points for most plates in dat melted?
# why the plate with only 816 points in dat4? Plate with 102 only has cyto endpoints, that okay 48+54
dat4[plate.id == "MW66-9604", .N, by = "treatment"] # okay, so there are no half lysis or full lysis ldh wells from this plate.-> right, so is the one that has no LDH data!
dat4[plate.id == "MW67-3712", .N, by = "treatment"]
# so yes, we can get away with just matching by plate

setnames(dat_melted, old = "apid", new = "plate.id")

dat_melted[, plate.id :=sub(" ","",plate.id)]
useplates <- intersect(unique(dat_melted$plate.id), unique(dat4$plate.id))

dat_melted[is.na(rval) & plate.id %in% useplates, .N] # 858 pts
dat4[is.na(rval) & plate.id %in% useplates, .N]

odat <- dat_melted
odat[is.na(rval) & plate.id %in% useplates, .N]
dat4[is.na(rval) & plate.id %in% useplates, .N]

mdat <- merge(odat[plate.id %in% useplates], dat4[plate.id %in% useplates], by = c("rowi","coli","acsn","plate.id"),
              suffixes = c(".org",".new"))
nrow(odat) # 25344
nrow(dat4) # 38730
nrow(mdat) # 24816

mdat[is.na(rval.org) & is.na(rval.new), .N] # 81 are in agreement
mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 0, .N] # 579 also essentially in agreement

mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, .N] # 88. Okay, so these are NA in the original, but not in my notes

mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1 & !grepl("(LDH)|(CTB)",acsn), .N, by = "plate.id"]
# plate.id  N
# 1: MW66-9818 55
mdat[plate.id == "MW66-9818" & is.na(rval.org), .N, by = "well_id"]

# Check if conc's are lining up:
mdat[, conc.org := as.numeric(conc.org)]
mdat[conc.org != conc.new & is.na(conc.org), .(treatment, conc.org, conc.new)]
# Empty data.table (0 rows and 3 cols): treatment,conc.org,conc.new
mdat[is.na(conc.org), .N, by = c("treatment")]
# treatment    N
# 1:     Media  330
# 2:      DMSO 1419
# 3:     Lysis  451
# coolio

# comparing LDH values
# NA values first
mdat[grepl("LDH",acsn) & is.na(rval.org) & wllq.new == 1] # 20 instances
mdat[grepl("LDH",acsn) & is.na(rval.org) & wllq.new == 1, .(treatment, rval.new, rval.org, well_id)] 
mdat[grepl("CTB",acsn) & is.na(rval.org) & wllq.new == 1] # emtpty

mdat[rval.new < 0 & grepl("LDH",acsn), summary(rval.org)] # seems like a lot of the big negative values were set to NA

LDH_dat <- mdat[grepl("LDH",acsn)]
LDH_dat[, rval.org := rval.org*100]
LDH_dat[is.na(rval.org), rval.org := 100]
LDH_dat[is.na(rval.new), rval.new := 100]
plot(LDH_dat[,.(rval.org, rval.new)])
abline(0,1)
# okay, there are some major differences here...
abline(h=0)
abline(v=0)
# I think that in a lot of cases, where the rval was negative, it was set to NA
# If 1/2 lysis wells were set to NA, then the original values would have been normalized by larger/more positve values
# 
# where points above 0, rval.new is smaller - divided by a larger 1/2 lysis value
# where points are below 0, rval.new is smaller in magnitude - again, divided by a larger 1/2 lysis value

plot(LDH_dat[rval.org < 50,.(rval.org, rval.new)])
points(LDH_dat[rval.org < 50 & experiment.date.org == "20190326",.(rval.org, rval.new)], col = "red")
points(LDH_dat[rval.org < 50 & experiment.date.org == "20190328",.(rval.org, rval.new)], col = "blue")
points(LDH_dat[rval.org < 50 & experiment.date.org == "20190402",.(rval.org, rval.new)], col = "purple")
points(LDH_dat[rval.org < 50 & experiment.date.org == "20190409",.(rval.org, rval.new)], col = "green")

plot(LDH_dat[,.(rval.org, rval.new)])
points(LDH_dat[experiment.date.org == "20190326",.(rval.org, rval.new)], col = "red")
points(LDH_dat[experiment.date.org == "20190328",.(rval.org, rval.new)], col = "blue")
points(LDH_dat[experiment.date.org == "20190402",.(rval.org, rval.new)], col = "purple")
points(LDH_dat[experiment.date.org == "20190409",.(rval.org, rval.new)], col = "green")
title(main = "LDH in APCRA 2019 MEA Acute New values Normalized by Experiment Date median\nvs. Original Values Normalized by plate average")


# the points that are significantly different:
LDH_dat[rval.org > 100, .(rval.org, rval.new, treatment, conc.new)]
LDH_dat[abs(rval.org - rval.new) > 10 & rval.org != 100, .(rval.org, rval.new, treatment, conc.new)]
# only 6 of the 19 wells here are treated wells. This really is not too bad.

# generally checking out cndx
unique(dat4$conc)
# [1]  0.030  0.100  0.300  1.000  3.000 10.000 30.000  0.002 25.000  5.000
boxplot(rval ~ conc, dat4[grepl("firing",acsn) & wllq == 1])
# I don't see much of a response at any conc
length(unique(dat4$apid))
par(mfrow = c(4,4))
for (plate in unique(dat4$apid)) {
  boxplot(rval ~ conc, dat4[apid == plate & grepl("firing",acsn) & wllq == 1])
}
# conclusion: we have to use cndx 1 and 2, bc there are 3 apid's where DMSO wells are all nearly -100!
# the DMSO effect!

# do we see this for AB?
par(mfrow = c(4,4))
for (plate in unique(dat4$apid)) {
  boxplot(rval ~ conc, dat4[apid == plate & grepl("AB",acsn) & wllq == 1])
}
# yes, in a few wells, but doesn't seem as bad
# but odd that DMSO is usually smaller

for (plate in unique(dat4$apid)) {
  boxplot(rval ~ conc, dat4[apid == plate & grepl("LDH",acsn) & !grepl("Lysis",treatment) & wllq == 1])
}