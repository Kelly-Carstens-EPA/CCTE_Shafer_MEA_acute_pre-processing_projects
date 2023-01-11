# comparing dat4 with what is in DNT flat file, for verification
# 06/18/2020, and earlier
library(data.table)
library(readxl)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
scripts <- list.files(path = "mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T)
scripts <- scripts[!grepl("run_me\\.R",scripts) & !grepl("wllt_conc_formalization\\.R",scripts)]
sapply(scripts, source)

dat4 <- get_latest_dat(lvl = "dat4","DNT2019")
# DNT2019_dat4_2020-06-10.RData 

# verifying spid's with what is in flat file
flatfile <- as.data.table(read_excel("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA/DNT acute flatfile.xlsx"))
setdiff(unique(dat4$spid), unique(flatfile$EPA_SAMPLE_ID))
setdiff(unique(flatfile$EPA_SAMPLE_ID), unique(dat4$spid)) # compounds in flat file, not in dat4
# [1] "TTX"              "PICRO"            "LYSIS"            "MEDIA"            "EPAPLT0169C05C05" "EPAPLT0167E10E10" "EPAPLT0167A02A02"
# [8] "EPAPLT0167A08A08" "EPAPLT0167A04A04" "EPAPLT0167D09D09" "EPAPLT0167A03"    "EPAPLT00"  
# hmm... all of the double-well ones are probably a mistake
sapply(c("EPAPLT0169C05","EPAPLT0167E10", "EPAPLT0167A02", "EPAPLT0167A08", "EPAPLT0167A04", "EPAPLT0167D09"), function(x) x %in% unique(dat4$spid))
# EPAPLT0169C05 EPAPLT0167E10 EPAPLT0167A02 EPAPLT0167A08 EPAPLT0167A04 EPAPLT0167D09 
# TRUE          TRUE          TRUE          TRUE          TRUE          TRUE # spid's without the duplicated wells are present in dat4
# "EPAPLT00" is probs just an extra, empty row
flatfile[EPA_SAMPLE_ID == "EPAPLT0167A03", .(unique(EXPERIMENT_DATE))] # 20191008
dat4[experiment.date == "20191008", unique(spid)]
# [1] "EPAPLT0167A04" "EPAPLT0167F07" "EPAPLT0167D04" "EPAPLT0167H07" "EPAPLT0169A03" "DMSO"          "Tritonx100"    "Media"        
# [9] "Picrotoxin"    "Tetrodotoxin"
# okay, this is from the last culture date, where there were only 5 compounds tested...
# hmm, so I have "EPAPLT0169A03" in dat4. That is probably what was meant by "EPAPLT0167A03". 
# Furthermore, "EPAPLT0167A03" is not in the 'MEA Acute Conc Res' tab of the spidmap file 
setdiff(unique(dat4$spid), unique(flatfile$EPA_SAMPLE_ID)) # compounds in dat4, not in flat file
# [1] "EPAPLT0169D05" "EPAPLT0169C05" "EPAPLT0167A02" "EPAPLT0167A08" "EPAPLT0167D09" "Tritonx100"    "Media"         "Picrotoxin"   
# [9] "Tetrodotoxin" 
setdiff(unique(dat4$experiment.date), unique(flatfile$EXPERIMENT_DATE)) # [1] "20190530" "20190618"
dat4[spid %in% c("EPAPLT0169D05", "EPAPLT0169C05", "EPAPLT0167A02", "EPAPLT0167A08", "EPAPLT0167D09"), unique(experiment.date), by = c("spid","treatment")]
# spid              treatment       V1
# 1: EPAPLT0169D05         1 20190827
# 2: EPAPLT0169C05        55 20190730
# 3: EPAPLT0169C05        55 20190618 # exp date not used in flatfile
# 4: EPAPLT0167A02        65 20190730
# 5: EPAPLT0167A02        65 20190618 # exp date not used in flatfile
# 6: EPAPLT0167A08        66 20190730
# 7: EPAPLT0167A08        66 20190618 # exp date not used in flatfile
# 8: EPAPLT0167D09        70 20190730
# 9: EPAPLT0167D09        70 20190618 # exp date not used in flatfile
flatfile[EXPERIMENT_DATE %in% c("20190827", "20190730", "20190618"), unique(EPA_SAMPLE_ID)]
# [1] "EPAPLT0169C05C05" "EPAPLT0167E10E10" "EPAPLT0167A02A02" "EPAPLT0167A08A08" "EPAPLT0167A04A04" "EPAPLT0167D09D09" "EPAPLT0167D05"   
# [8] "EPAPLT0167F02"    "EPAPLT0169F09"    "EPAPLT0167C02"    "EPAPLT0167D08"    "EPAPLT0167C10" 
# so the duplicates are definitely off. 
# and everything in "20190618", we know is not going to be in the flatfile
flatfile[EXPERIMENT_DATE %in% c("20190827"), .(trt = unique(`Chemical Number`), spid = unique(EPA_SAMPLE_ID))]
# trt          spid
# 1:   1 EPAPLT0167D05
# 2:   9 EPAPLT0167F02
# 3:  10 EPAPLT0169F09
# 4:  11 EPAPLT0167C02
# 5:  13 EPAPLT0167D08
# 6:  15 EPAPLT0167C10
# from the spid map file, treatment '1' correspodn to spid 169D05, not 167D05
flatfile[EXPERIMENT_DATE %in% c("20190730"), .(trt = unique(`Chemical Number`), spid = unique(EPA_SAMPLE_ID))]
# trt             spid
# 1: 55 repeat EPAPLT0169C05C05
# 2: 58 repeat EPAPLT0167E10E10
# 3: 65 repeat EPAPLT0167A02A02
# 4: 66 repeat EPAPLT0167A08A08
# 5: 69 repeat EPAPLT0167A04A04
# 6: 70 repeat EPAPLT0167D09D09
# so all of these mismatches are just due to duplication of well id
# all of the the differences in spids present can be explained!

# EXTRA CHECKING with what is in flat file
# (adapted from source_to_lvl0_nheerl_mea_acute.R)
df <- as.data.frame(flatfile)

# Reshape - split the data by run (1:3)

dat             <- data.frame('spid' = rep(df$EPA_SAMPLE_ID, 3), stringsAsFactors = FALSE)
dat$apid        <- c(df$MEA_PLATE_ID_RUN1, df$MEA_PLATE_ID_RUN2, df$MEA_PLATE_ID_RUN3)
dat$well_id     <- c(df$`MEA_WELL_ID _RUN_1`, df$`MEA_WELL_ID _RUN_2`, df$`MEA_WELL_ID _RUN_3`)
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
# looks like the plates are not reused, so can just merge that way

setnames(dat_melted, old = "apid", new = "plate.id")

dat_melted[, plate.id :=sub(" ","",plate.id)]
useplates <- intersect(unique(dat_melted$plate.id), unique(dat4$plate.id))

dat_melted[is.na(rval) & plate.id %in% useplates, .N]
dat4[is.na(rval) & plate.id %in% useplates, .N]

odat <- dat_melted
odat[is.na(rval) & plate.id %in% useplates, .N]
dat4[is.na(rval) & plate.id %in% useplates, .N]

mdat <- merge(odat[plate.id %in% useplates], dat4[plate.id %in% useplates], by = c("rowi","coli","acsn","plate.id"),
              suffixes = c(".org",".new"))
nrow(odat) # 20592
nrow(dat4) # 32058
nrow(mdat) # 20592

mdat[is.na(rval.org) & is.na(rval.new), .N] # 9 are in agreement
mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 0, .N] # 680 also essentially in agreement

mdat[is.na(rval.org) & !is.na(rval.new) & wllq.new == 1, .N] # 0!!


# Check if conc's are lining up:
mdat[, conc.org := as.numeric(conc.org)]
mdat[conc.org != conc.new & !is.na(conc.org), .(treatment, conc.org, conc.new)]
# not empty
mdat[, conc.org.r := signif(conc.org, digits = 3)] # they round to 3 sig figs in toxcast, so let's start there
mdat[, conc.new.r := signif(conc.new, digits = 3)]
mdat[conc.org.r != conc.new.r & !is.na(conc.org.r), unique(treatment)]
# [1] "3"     "5"     "8"     "DMSO"  "Media" "PICRO" "TTX"   "Lysis"
mdat[treatment == "3", .N, by = c("conc.org","conc.new")]
mdat[treatment == "3", .(conc.org = paste0(sort(unique(conc.org)),collapse=","), conc.new = paste0(sort(unique(conc.new)),collapse=",")), by = c("plate.id","rowi","coli")]
# plate.id      rowi coli   conc.org    conc.new
# 1: MW68-0808    1    2       0.03     0.03
# 2: MW68-0811    1    2 0.02952135     0.03
# 3: MW68-0812    1    2       0.03     0.03
# 4: MW68-0808    1    3        0.1      0.1
# 5: MW68-0811    1    3  0.0984045      0.1
# 6: MW68-0812    1    3        0.1      0.1
# 7: MW68-0808    1    4        0.3      0.3
# 8: MW68-0811    1    4  0.2952135      0.3
# 9: MW68-0812    1    4        0.3      0.3
# 10: MW68-0808    1    5          1        1
# 11: MW68-0811    1    5   0.984045        1
# 12: MW68-0812    1    5          1        1
# 13: MW68-0808    1    6          3        3
# 14: MW68-0811    1    6   2.952135        3
# 15: MW68-0812    1    6          3        3
# 16: MW68-0808    1    7         10       10
# 17: MW68-0811    1    7    9.84045       10
# 18: MW68-0812    1    7         10       10
# 19: MW68-0808    1    8         30       30
# 20: MW68-0811    1    8   29.52135       30
# 21: MW68-0812    1    8         30       30
# Looks like conc-correction only happened on 1 plate for the conc.orginal data
# but did not get translated to any of my data...
# wait a sec.... looking at the stock conc for treatment 3, it was eactly 20
# so the conc's for treatmetn 3 should not be modified at all
# the modified conc's look like the conc's for treatment 8!
# The calculations file I used looks totally right. So I am not going to worry about these compounds.
# treatments 5 and 8 were also from this culture
mdat[treatment == "5", unique(experiment.date.new)]

# for the rest of the compounds where the treatments are not exactly the same, but are same to 3 sig figs,
mdat[conc.org != conc.new & !is.na(conc.org), unique(treatment)]
# [1] "3"     "17"    "55"    "65"    "38"    "47"    "88"    "91"    "95"    "84"    "71"    "77"    "1"     "28"    "32"    "46"    "5"     "8"     "DMSO"  "Media" "PICRO" "TTX"  
# [23] "9"     "20"    "27"    "24"    "45"    "42"    "51"    "50"    "96"    "41"    "100"   "82"    "11"    "15"    "13"    "58"    "83"    "Lysis" "12" 
# either something got mixed up in teh flat file, as above, or somethign is truly off in the calculations file
# either way, the difference is by less than 3 sig figs! So I am not going to worry about it
# e.g. just looked at trt 17. I don't know why my values are not ;whole' numbers
# but it might just be the level of sig figs saved in excel/R options. I am not going to worry about it anymore.

mdat[is.na(conc.org), .N, by = c("treatment")]
# treatment    N
# 1:       TTX   78
# 2:      DMSO 1209
# 3:     PICRO   78
# 4:     Lysis    3
# 5:     Media   36
# coolio


# comparing LDH values
# NA values first
mdat[grepl("LDH",acsn) & is.na(rval.org) & wllq.new == 1] # 0!
mdat[grepl("CTB",acsn) & is.na(rval.org) & wllq.new == 1] # emtpty

# deprecated - I am not even calculating percent of total LDH anymore, so can't compare
# mdat[rval.new < 0 & grepl("LDH",acsn), summary(rval.org)] # seems like a lot of the big negative values were set to NA
# 
# LDH_dat <- mdat[grepl("LDH",acsn)]
# LDH_dat[, rval.org := rval.org*100]
# LDH_dat[is.na(rval.org), rval.org := 100]
# LDH_dat[is.na(rval.new), rval.new := 100]
# plot(LDH_dat[,.(rval.org, rval.new)])
# abline(0,1)
# # okay, there are some major differences here...
# abline(h=0)
# abline(v=0)
# # I think that in a lot of cases, where the rval was negative, it was set to NA
# # If 1/2 lysis wells were set to NA, then the original values would have been normalized by larger/more positve values
# # 
# # where points above 0, rval.new is smaller - divided by a larger 1/2 lysis value
# # where points are below 0, rval.new is smaller in magnitude - again, divided by a larger 1/2 lysis value
# 
# plot(LDH_dat[rval.org < 50,.(rval.org, rval.new)])
# LDH_dat[, unique(experiment.date.org)]
# points(LDH_dat[rval.org < 50 & experiment.date.org == "20190528",.(rval.org, rval.new)], col = "red")
# points(LDH_dat[rval.org < 50 & experiment.date.org == "20190730",.(rval.org, rval.new)], col = "blue")
# 
# plot(LDH_dat[,.(rval.org, rval.new)])
# points(LDH_dat[experiment.date.org == "20190326",.(rval.org, rval.new)], col = "red")
# points(LDH_dat[experiment.date.org == "20190328",.(rval.org, rval.new)], col = "blue")
# points(LDH_dat[experiment.date.org == "20190402",.(rval.org, rval.new)], col = "purple")
# points(LDH_dat[experiment.date.org == "20190409",.(rval.org, rval.new)], col = "green")
# title(main = "LDH in DNT 2019 MEA Acute New values Normalized by Experiment Date median\nvs. Original Values Normalized by plate average")
# 
# 
# # the points that are significantly different:
# LDH_dat[rval.org > 100, .(rval.org, rval.new, treatment, conc.new)]
# LDH_dat[abs(rval.org - rval.new) > 10 & rval.org != 100, .(rval.org, rval.new, treatment, conc.new)]
# # only 6 of the 19 wells here are treated wells. This really is not too bad.