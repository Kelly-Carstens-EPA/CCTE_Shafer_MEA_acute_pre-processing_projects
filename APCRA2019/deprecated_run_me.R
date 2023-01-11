# script to do the things!

###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project TSCA_APCRA"
dataset_title <- "APCRA2019"
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/")
select.neural.stats.files <- F # run selectInputFiles or no? else will read from most recent _files_log.txt file
select.calculations.files <- T
run.type.tag.location <- 5 # for files named as "tag1_tag2_tag3_....csv", Which tag in the file names defines the run_type?
###################################################################################
# END USER INPUT
###################################################################################

# load packages
library(data.table)
library(readxl)
library(pracma)

# source functions from master scripts
source('../mea-acute-neural-stats-to-mc0-scripts/gather_files-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/check-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/data_prep-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/collapsePlateData.R')
source('../mea-acute-neural-stats-to-mc0-scripts/acute_cytotox_prep06.R')

# loading some information for the funtions to reference
load("../mea-acute-neural-stats-to-mc0-scripts/standard_settings.RData")
get_acsn_map(type = "post_july_2016")

main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# select input files to use and log files in log file
if (select.neural.stats.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}


# before process any data, need to check that file names following the naming convention 
# specifically, at the specified run.type.tag.location, there should either be _00 or _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(main.output.dir)
# Reading from APCRA2019_files_log_2020-05-17.txt...
# Got 94 files.
# The following files appear to be named incorrectly:
#   filenames run.type.tags
# 1: TC_20190417_MW67-3707_13_00(000).csv            00
# 2: TC_20190417_MW67-3707_15_01(000).csv            01
# this will be okay, since the DIV is not used to match the treated and baseline files

# Check the selected files for common issues
# if you do not run these checks, fileToLongDat will throw an error later if any of these are an issue
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e
         })                                     

# extract all of the data from the files and transform into long data format
# If you have reviewed the checks above and are confident ignoring them, 
# you can set the corresponding "check.x" variables to F. Document why this is okay.
extractAllData(main.output.dir, dataset_title, run.type.tag.location, append = F, threshold = 30,
               check.settings = F, check.parameters = F, check.timing = F)

# output summary of wllq changes
load(file = "output/APCRA2019_alldat1_2020-05-18.RData")
alldat1[wllq == 0, .(wllq_set_to_zero = paste0(sort(unique(well)),collapse=",")), by = "apid"]
object.size(alldat1) # 31167632 bytes = 31 Mb. Not small
rm(alldat1)

# create alldat2, with the collapsed plate data
collapsePlateData(main.output.dir)

# show summary of the data, for a gut check, to confirm it's all there
load(file = "output/APCRA2019_alldat2_2020-05-18.RData")
boxplot(rval ~ acsn, alldat2)
# Warning messages:
#   1: In bplt(at[i], wid = width[i], stats = z$stats[, i], out = z$out[z$group ==  :
#                                                                         Outlier (Inf) in boxplot 8 is not drawn
#                                                                       2: In bplt(at[i], wid = width[i], stats = z$stats[, i], out = z$out[z$group ==  :
#                                                                                                                                             Outlier (Inf) in boxplot 14 is not drawn
alldat2[acsn == "NHEERL_MEA_acute_firing_rate_mean", summary(rval)]
alldat2[is.infinite(rval)] # wllq is zero here

boxplot(rval ~ acsn, alldat2[wllq==1])
# no warnings

# how many points are less than 100?
alldat2[wllq==1 & rval > 100, summary(rval)] # 1210 total
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 100.1   128.4   172.4   265.1   288.0  3926.6 
alldat2[wllq==1 & rval > 100, .N, by = "acsn"]
# acsn   N
# 1:                NHEERL_MEA_acute_burst_percentage_std 298
# 2:     NHEERL_MEA_acute_bursting_electrodes_number_mean  10
# 3:            NHEERL_MEA_acute_interburst_interval_mean 103
# 4: NHEERL_MEA_acute_per_network_burst_spike_number_mean 209
# 5:  NHEERL_MEA_acute_per_network_burst_spike_number_std 109
# 6:              NHEERL_MEA_acute_cross_correlation_HWHM  50
# 7:                    NHEERL_MEA_acute_firing_rate_mean 103
# 8:                        NHEERL_MEA_acute_spike_number 103
# 9:                        NHEERL_MEA_acute_burst_number 116
# 10:              NHEERL_MEA_acute_cross_correlation_area  76
# 11:                 NHEERL_MEA_acute_burst_duration_mean  16
# 12:         NHEERL_MEA_acute_per_burst_spike_number_mean  16
# 13:            NHEERL_MEA_acute_network_burst_percentage   1


# add cyto data
saveCytoData(main.output.dir, dataset_title)
load(file = "output/APCRA2019_cytodat_2020-05-18.RData")
str(cytodat)


# COMBINE the data!
load(file = "output/APCRA2019_alldat2_2020-05-18.RData")

# !! this data set only
# "20190404_MWNo Barcode" assign correct apid
alldat2[apid == "20190404_MWNo Barcode", apid := "20190404_MW1237-11"]

# get trt, conc values for alldat2 from cytodat
alldat3 <- list()
endpoints <- unique(alldat2$acsn)
for (endpoint in endpoints) {
  add.dat <- merge(alldat2[acsn == endpoint], cytodat[, .(treatment, apid, rowi, coli, conc)])
  alldat3 <- rbind(alldat3, add.dat)
  rm(add.dat)
}

# get the wllq for cytodat - just use 1 (any) acsn to get the table
wllq_summary <- alldat2[acsn == unique(alldat2$acsn)[1], .(wllq), by = c("apid","rowi","coli")]
cytodat <- merge(wllq_summary, cytodat, all.y = T)

# !!! for this data set only
# we are not using "20190402_MW1236-24" for the ontogeny data bc the recording time was too short
# assign wllq == 1 for these
cytodat[apid == "20190402_MW1236-24", wllq := 1]

alldat4 <- rbind(alldat3, cytodat)

# assign wllt (will look a bit diff for ea dataset, with naming)
# check trt names either spid or DMSO, Bicuculline, or Tritonx100
# check conc's make sure it all looks good/is numeric (not "Control")

# check that all data is there, nothing is missing
# set wllq to 0 where needed
# any other checks

save(alldat4, file = file.path(main.outut.dir, paste0("/output",dataset_title,"_alldat4_",as.character.Date(Sys.Date()),".RData")))

# may 20, 2020
load("output/APCRA2019_alldat2_2020-05-18.RData")
boxplot(rval ~ rowi, alldat2[wllq == 1 & coli == 1 & rowi %in% c(1,2,3,4,5,6) & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
title(main = "Percent Change in Mean Firing rate\nin Control wells of ToxCast MEA acute Experiments")

