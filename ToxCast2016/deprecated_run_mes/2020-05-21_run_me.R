###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/Experiment Date"
dataset_title <- "ToxCast2016"
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")
select.neural.stats.files <- F # select new files, or use the files in teh most recent neural_stats_files_log?
select.calculations.files <- F # will be getting the cytotox data from flat file for this data set
run.type.tag.location <- 6 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run_type?
plate.id.tag.location <- 2 # the files in this data set do not include the apid, so I will have to get from the file name
###################################################################################
# END USER INPUT
###################################################################################

# load packages
library(data.table)
library(readxl)
library(pracma)
library(tcpl)

# source functions from master scripts
source('../mea-acute-neural-stats-to-mc0-scripts/gather_files-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/check-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/data_prep-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/collapsePlateData.R')
source('../mea-acute-neural-stats-to-mc0-scripts/acute_cytotox_prep06.R')
source('../mea-acute-neural-stats-to-mc0-scripts/combineNeuralAndCyto.R')

# specific functions for this data set
source('supplemental-scripts/get_files_without_select.R')
source('supplemental-scripts/get_cyto_dat_toxcast2016.R')

# loading some information for the funtions to reference
load("../mea-acute-neural-stats-to-mc0-scripts/standard_settings.RData")
get_acsn_map(type = "pre_july_2016") # load the acsn map with the appropriate endpoints

main.output.dir <- getwd()

if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  # selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
  all_files <- getToxCastFiles() # short cut fun for this data set
  writeLogFile(all_files, main.output.dir, dataset_title, files_type = "neural_stats")
  length(all_files) # 414
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}


# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(main.output.dir)
# RESULT --------------------------------------------------------- 
# Reading from ToxCast2016_neural_stats_files_log_2020-05-18.txt...
# Got 414 files.
# All files are named correctly.
# Empty data.table (0 rows and 2 cols): filenames,run.type.tags
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title, check.settings = F, check.parameters = T, check.timing = T, threshold = 1*60), 
         error = function(e){
           closeAllConnections()
           e } )  
# RESULT --------------------------------------------------------- 
# ToxCast2016_check_summary_2020-05-18.txt is ready.
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (alldat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, append = F,
               check.settings = F, check.parameters = F, check.timing = F, plate.id.tag.location = plate.id.tag.location)
# RESULT --------------------------------------------------------- 
# Reading from ToxCast2016_neural_stats_files_log_2020-05-18.txt...
# Got 414 files.
# Reading data from files... ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_alldat1_2020-05-18.RData is ready.
# ..
# ---------------------------------------------------------------- 

# collapse the plate data by calculating the percent change in activity (alldat2)
collapsePlateData(main.output.dir)
# RESULT --------------------------------------------------------- 
# Loading L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_alldat1_2020-05-19.RData ...
# Collapsing treated and baseline data...
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_alldat2_2020-05-20.RData is ready.
# ---------------------------------------------------------------- 

# looking at data so far
load(file = "output/ToxCast2016_alldat2_2020-05-20.RData")
alldat2[wllq==1, summary(rval)]
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#  -100.000   -28.642    -7.043    -9.625     2.746 11505.212      4844 
rm(alldat2)

# get cytotox data
cytodat <- get_cyto_dat_toxcast2016()
str(cytodat)

# combine the cytodat with alldat2, add trt, conc, and wllq to ea
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# RESULT --------------------------------------------------------- 
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_alldat3_2020-05-20.RData is ready.
# ---------------------------------------------------------------- 
rm(cytodat)

# ASSIGN WLLT
load(file = "output/ToxCast2016_alldat3_2020-05-20.RData")

# ensure that any all non-spid values are in: "Tritonx100" "Bicuculline"  "DMSO" 
unique(alldat3$spid)
alldat3[spid == "LYSIS", spid := "Tritonx100"]
alldat3[spid == "BC" | spid == "BIC", spid := "Bicuculline"]
unique(alldat3$spid)

alldat3[, wllt := "t"]
alldat3[spid == "DMSO", wllt := "n"]
alldat3[spid == "Tritonx100" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "p"]
alldat3[spid == "Tritonx100" & !(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), wllt := "x"]
alldat3[spid == "Bicuculline" & !(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), wllt := "p"]
alldat3[spid == "Bicuculline" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "z"]


# ASSIGN UPDated SPIDS 

no_spid_dat <- alldat3[spid %in% c("DMSO","Bicuculline","Tritonx100")] # set this aside
# taken from source_to_lvl0_nheerl_mea_acute.R:
# First map the legacy sample IDs to the current IDs (TP), the xlsx was provided from Chemtrack since it was not mapped in invitrodb
colnames(alldat3)[colnames(alldat3) == 'spid'] <- 'spid_legacy'
alldat3$spid_legacy <- as.character(alldat3$spid_legacy)

library(xlsx)
TX_TP <- read.xlsx("L:/Lab/ToxCast_Data/toxcast_data/files/nheerl_mea_acute/source/unblinded_tx_to_tp_codes_NHEERL_SHAFER_ACUTE.xlsx",  1, stringsAsFactors = F)
colnames(TX_TP)[colnames(TX_TP) == "EPA_SAMPLE_ID"] <- "spid"
colnames(TX_TP)[colnames(TX_TP) == "BOTTLE_ID"] <- "spid_legacy"

alldat3 <- merge(x = alldat3, y = TX_TP[, c("spid", "spid_legacy")], all.x = TRUE, by = "spid_legacy")
alldat3[is.na(spid),unique(spid_legacy)] # [1] "Bicuculline" "DMSO"        "Tritonx100" 
alldat3 <- alldat3[!is.na(spid)]
alldat3[, spid_legacy := NULL]
alldat3 <- rbind(alldat3, no_spid_dat) # add back in dmso, BIC, and Tritonx100 data
rm(no_spid_dat)


# SET WLLQ TO ZERO WHERE RVAL IS NA
nrow(alldat3[is.na(rval)])
# 9168 (was 6066 in prev script)
# now, it is [1] 5773 (I did not drop re-runs, so that is probably why)
alldat3[wllq==1 & is.na(rval),.N] # [1] 5038
alldat3[is.na(rval), wllq := 0]

# CHECK CONC'S
unique(alldat3$conc)
alldat3[is.na(conc),unique(spid)] # [1] "DMSO"        "Bicuculline" "Tritonx100" 
# we will compare this with the previously pipelined data


# VERIFYING WITH WHAT WAS PREVIOUSLY PIPELINED

mc0.apr20 <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/output/01APR2020/mc0_nheerl_mea_acute.csv",stringsAsFactors = F)
setDT(mc0.apr20)

# check how Katie handled the NA conc's
mc0.apr20[spid %in% c("DMSO", "Bicuculline", "Tritonx100"), .(unique(conc)), by = "spid"]
# spid      V1
# 1: Bicuculline  25.000
# 2:        DMSO   0.002
# 3:  Tritonx100 100.000
alldat3[spid %in% c("DMSO", "Bicuculline", "Tritonx100"), .(unique(conc)), by = "spid"] # all of these are always NA
alldat3[spid == "Bicuculline", conc := 25.00]
alldat3[spid == "DMSO", conc := 0.002]
alldat3[spid == "Tritonx100", conc :=100.00]
alldat3[spid %in% c("DMSO", "Bicuculline", "Tritonx100"), .(unique(conc)), by = "spid"]

# making apid's consistent, so can compare
alldat3[, apid_old := sub("^[0-9]{8}_MW","MW ",apid)]
setdiff(alldat3$apid_old, mc0.apr20$apid) # [1] "MW 1072-08"
mc0.apr20[grepl("1072-8",apid)] # yep, just the zero
mc0.apr20[apid == "MW 1072-8", apid := "MW 1072-08"]
setdiff(mc0.apr20$apid, alldat3$apid_old) # [1] "MW 1079-13" "MW 186-25" 
mc0.apr20[apid == "MW 1079-13", apid := "MW 1073-13"]
mc0.apr20[apid == "MW 186-25" , apid := "MW 1086-25" ]
setdiff(mc0.apr20$apid, alldat3$apid_old)
alldat3$coli <- as.integer(alldat3$coli)

# I think something is off with this merging. NOt sure what, but I don't think these checks are telling me anything real
# compare.vals <- merge(alldat3, mc0.apr20, by.x = c("apid_old","rowi","coli","acsn","spid"), by.y = c("apid","rowi","coli","acnm","spid"),
#                       suffixes = c(".new",".cur"), all = F)
# 
# # all other conc's the same?
# compare.vals[conc.new != conc.cur] # empty
# 
# # check wllt assignments
# compare.vals[wllt.new != wllt.cur] # empty
# 
# # check wllq assignmnets
# compare.vals[wllq.new != wllq.cur] # several instances
# compare.vals[wllq.new == 1 & wllq.cur == 0, .N, by = c("apid","rowi","coli","apid_old")]
# 
# # okay, so why were these wells set to zero? Should i set these wells to wllq zero?
# compare.vals[wllq.new == 1 & wllq.cur == 0, unique(rval.cur)] # yep, all are Na here
# # but why are my values not NA? that does not make sense. Let's see an example
# compare.vals[wllq.new == 1 & wllq.cur == 0,unique(apid_old)]
# load("output/ToxCast2016_alldat1_2020-05-18.RData")
# alldat1[apid == "20150730_MW1041-25" & rowi == 6 & coli == 8 & is.na(activity_value), .N, by = c("run_type","tcpl_acsn")] # empty
# mc0.apr20[apid == "MW 1041-25"  & rowi == 6 & coli == 8 & is.na(rval), .N] # 14... almost all non-cyto endpoints
# mc0.apr20[apid == "MW 1041-25" & is.na(rval), .(rowi, coli, acnm)]
# mc0.apr20[apid == "MW 1041-25",paste0(unique(coli),collapse=","), by = "wllq"]
# mc0.apr20[apid == "MW 1041-25",paste0(unique(rowi),collapse=","), by = "wllq"]
# alldat1[apid == "20150730_MW1041-25", unique(srcf)]
# 
# compare.vals[wllq.new == 0 & wllq.cur == 1, .N] # 2501
# compare.vals[wllq.new == 0 & wllq.cur == 1 & is.na(rval.cur), .N] # 0
# mc0.apr20[is.na(rval), unique(wllq)] # 0

# skipping for now
# # get acid from acsn, via invitrodb
# tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
# shafer.assays <- tcplLoadAcid(fld = "asid",val=20,add.fld = "acsn")
# mea.acute <- shafer.assays[grepl("MEA_acute",acnm)]
# alldat3 <- merge(alldat3, mea.acute[, .(acsn,acid)], by = "acsn")

# check that all data is there, nothing is missing
check.points <- dcast(alldat3[, .N, by = c("acsn","apid")], apid ~ acsn, value.var = "N")
setnames(check.points, old = names(check.points), new = as.character(1:length(names(check.points))))
check.points[1:53]; check.points[54:99]; check.points[100:153]; check.points[154:207]
# result: 17 acsn's, all have 48 data points per apid
unique(alldat3$acsn)
unique(alldat3$wllq) # 1 0, no NA
length(unique(alldat3$apid)) # 207, = 69 directories * 3 plates per experiment
sort(unique(alldat3$conc))


# visualizations - ? future probably

ToxCast2016_alldat4 <- alldat3[, .(spid, apid, rowi, coli, conc, acsn, wllt, wllq, srcf, rval)]
save(ToxCast2016_alldat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_alldat4_",as.character.Date(Sys.Date()),".RData")))

# # checking May 20, 2020
# load("output/ToxCast2016_alldat4_2020-05-19.RData")
# unique(alldat4$spid)
# alldat4[spid == "Tritonx100", .N, by = c("rowi","coli")]
# # rowi coli    N
# # 1:    6    1 3519 # all are in well A6
# alldat4[rowi==6 & coli==1, unique(spid)]
# # [1] "Tritonx100" the only thing in the well is always LYSIS - for every assay component endpoint
# 
# load("output/ToxCast2016_alldat3_2020-05-19.RData")
# unique(alldat3$spid)
# alldat3[spid %in% c("BC","BIC"), spid := "Bicuculline"]
# boxplot(rval ~ spid, alldat3[spid %in% c("DMSO","Bicuculline","LYSIS") & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
# title(main = "Percent Change in Mean Firing rate\nin Control wells of ToxCast MEA acute Experiments")

load(file = "output/ToxCast2016_alldat3_2020-05-20.RData")
alldat3[grepl("TX008702",spid), unique(apid)]
unique(alldat3$spid)
