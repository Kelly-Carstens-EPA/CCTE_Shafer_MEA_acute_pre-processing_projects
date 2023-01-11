###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/Experiment Date"
dataset_title <- "ToxCast2016"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/" # where the dataset_title folder will be created
select.neural.stats.files <- T # select new files, or use the files in teh most recent neural_stats_files_log?
select.calculations.files <- F # will be getting the cytotox data from conc_Response_Log_JS.xslx
run.type.tag.location <- 6 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run_type?
plate.id.tag.location <- 2 # the files in this data set do not include the apid, so I will have to get from the file name
spidmap_file <- "L:/Lab/ToxCast_Data/toxcast_data/files/nheerl_mea_acute/source/unblinded_tx_to_tp_codes_NHEERL_SHAFER_ACUTE.xlsx" # will use flatfile, will be a bit different for this data set
use_sheet <- 1 # sheet name in spidmap_file
override_wllq_checks <- FALSE # set to TRUE if you have already verified your wllq updates
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(readxl)
library(tcpl)

# set up folders and working directory
if (!dir.exists(file.path(root_output_dir,dataset_title))) dir.create(file.path(start.dir,dataset_title))
setwd(file.path(root_output_dir,dataset_title))
main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# source all function in folder 'mea-acute-neural-stats-to-mc0-scripts', except for the run_me.R template
scripts <- list.files(path = "../mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T)
scripts <- scripts[!grepl("run_me\\.R",scripts) & !grepl("wllt_conc_formalization\\.R",scripts)]
sapply(scripts, source)

# specific functions for this data set
source('supplemental-scripts/get_files_without_select.R')
source("supplemental-scripts/get_trt_conc_dat_toxcast2016.R")
source('supplemental-scripts/fix_typos_toxcast2016.R')

# loading some information for the funtions to reference
get_acsn_map() # load the acsn map with the appropriate endpoints

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  # selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
  all_files <- getToxCastFiles(start.dir) # short cut fun for this data set
  writeLogFile(all_files, main.output.dir, dataset_title, files_type = "neural_stats")
  length(all_files) # 420
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}

# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(run.type.tag.location, main.output.dir, dataset_title)
# RESULT --------------------------------------------------------- 
# Reading from ToxCast2016_neural_stats_files_log_2020-06-06.txt...
# Got 420 files.
# All files are named correctly.
# Empty data.table (0 rows and 2 cols): filenames,run.type.tags
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# RESULT --------------------------------------------------------- 
# ToxCast2016_check_summary_2020-06-06.txt is ready
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT --------------------------------------------------------- 
# Reading from ToxCast2016_neural_stats_files_log_2020-06-06.txt...
# Got 420 files.
# Reading data from files...
# Processed TC_MW 1090-24_20151230_20160113_13_00(000)_Neural Statistics Compiler(000).csv
# Processed TC_MW 1090-24_20151230_20160113_13_01(000)_Neural Statistics Compiler(000).csv
# ...
# TC_MW 1076-38_20160303_20160317_14_01(001)_Neural Statistics Compiler(000).csv will be removed. Recording length is 544.25
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat1_2020-06-06.RData is ready.
# ---------------------------------------------------------------- 

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
str(dat1)
dat1[, .N/length(unique(dat1$tcpl_acsn)), by = "wllq_notes"]

# ** FIXES
# fix plate.id in 20151208, digit flip typo in neural stats files
dat1[experiment.date == "20151208", unique(plate.id)] # "MW1068-36" "MW1068-37" "MW1068-38"
dat1[experiment.date == "20151208", plate.id := sub("MW1068","MW1086",plate.id)]
dat1[experiment.date == "20151208", unique(plate.id)] # "MW1086-36" "MW1086-37" "MW1086-38"

# In 20151201, plate 1086-26 is named 1086-25 (I know this is wrong, because there is a 1086-25 in exp date 20151203)
dat1[experiment.date == "20151201" & plate.id == "MW1086-25", plate.id := "MW1086-26"]

# save the updated dat1
save(dat1, file = paste0("output/",dataset_title,"_dat1_",as.character.Date(Sys.Date()), ".RData"))
rm(dat1)

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Loading L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat1_2020-06-06.RData ...
# Collapsing treated and baseline data...
# 
# 20160113_MW1090-24
# Baseline stats file name: TC_MW 1090-24_20151230_20160113_13_00(000)_Neural Statistics Compiler(000).csv
# Treated stats file name: TC_MW 1090-24_20151230_20160113_13_01(000)_Neural Statistics Compiler(000).csv
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat2_2020-06-06.RData is ready.
# ---------------------------------------------------------------- 

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -100.000   -28.777    -7.119   -10.261     2.590 11505.212      4490
# ---------------------------------------------------------------- 
dat2[plate.id == "MW1076-38" & experiment.date == "20160317", unique(wllq)] 
# 0 -> confirmation that all wllq is 0 for this plate, where recording was too short
rm(dat2)

# get cytotox data
cytodat <- get_ToxCast_cytodat()
# OUTPUT --------------------------------------------------------- 
# lots of output, see notebook
# ---------------------------------------------------------------- 
str(cytodat)

# checkout the NA values
cytodat[is.na(rval),.N, by = c("plate.id")]
# flatfile <- "L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv"
# dat <- read.csv(flatfile, stringsAsFactors = F)
# allNA_AB <- dat[is.na(dat$AB_.DEAD_RUN1) | is.na(dat$AB_.DEAD_RUN2) | is.na(dat$AB_.DEAD_RUN3),
#     c("EXPERIMENT_DATE","MEA_PLATE_ID_RUN1","MEA_PLATE_ID_RUN2","MEA_PLATE_ID_RUN3","AB_WELL_ID","AB_.DEAD_RUN1","AB_.DEAD_RUN2","AB_.DEAD_RUN3")]
# 
# allNA_LDH <- dat[is.na(dat$LDH_.DEAD_RUN1) | is.na(dat$LDH_.DEAD_RUN2) | is.na(dat$LDH_.DEAD_RUN3),
#                 c("EXPERIMENT_DATE","MEA_PLATE_ID_RUN1","MEA_PLATE_ID_RUN2","MEA_PLATE_ID_RUN3","LDH_WELL__ID","LDH_.DEAD_RUN1","LDH_.DEAD_RUN2","LDH_.DEAD_RUN3")]
# rm(dat)
# load(file = paste0("output/",dataset_title,"_cytodat_from_conc_resp_log_",as.character.Date(Sys.Date()),".RData"))
# cytodat[experiment.date == "20151027" & plate.id == "MW1076-38" & grepl("AB",acsn)]

# get the trt and conc data from the flat file
trt_conc_dat <- get_trt_conc_dat_toxcast2016()

# fix up some typos before merging together
fix_typos_toxcast2016(cytodat, trt_conc_dat)

# merge the data sets together
cytodat <- merge(trt_conc_dat, cytodat, by = c("apid","coli","rowi","experiment.date","plate.id"), all.y = T)

# update AB values to set negative rval's to 0
cytodat[grepl("AB",acsn) & rval < 0, rval := 0.0]

# Adding in treatments for 20160607, since this was not included in flat file
# values taken from Conc_Response_Log_JS.xlsx
cytodat[experiment.date == "20160607" & coli == 1 & rowi %in% c(1,2,3), treatment := "DMSO"]
cytodat[experiment.date == "20160607" & coli == 1 & rowi %in% c(4,5), treatment := "BIC"]
cytodat[experiment.date == "20160607" & coli == 1 & rowi %in% c(6), treatment := "LYSIS"]
cytodat[experiment.date == "20160607" & coli %in% c(2:8) & rowi == 1, treatment := "TX008702"]
cytodat[experiment.date == "20160607" & coli %in% c(2:8) & rowi == 2, treatment := "TX015415"]
cytodat[experiment.date == "20160607" & coli %in% c(2:8) & rowi == 3, treatment := "TX015526"]
cytodat[experiment.date == "20160607" & coli %in% c(2:8) & rowi == 4, treatment := "TX010369"]
cytodat[experiment.date == "20160607" & coli %in% c(2:8) & rowi == 5, treatment := "TX003365"]
cytodat[experiment.date == "20160607" & coli %in% c(2:8) & rowi == 6, treatment := "TX000926"]
cytodat[experiment.date == "20160607" & coli == 2, conc := 40] # the aliquot conc's for each of these are 20, so dont' have to worry about conc correciton
cytodat[experiment.date == "20160607" & coli == 3, conc := 10]
cytodat[experiment.date == "20160607" & coli == 4, conc := 3]
cytodat[experiment.date == "20160607" & coli == 5, conc := 1]
cytodat[experiment.date == "20160607" & coli == 6, conc := 0.3]
cytodat[experiment.date == "20160607" & coli == 7, conc := 0.1]
cytodat[experiment.date == "20160607" & coli == 8, conc := 0.03]

# save the updated cytodat
save(cytodat, file = paste0("output/",dataset_title,"_cytodat_from_conc_resp_log_",as.character.Date(Sys.Date()),".RData")) # save the updated cytodat

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat3_2020-06-08.RData is ready.
# ---------------------------------------------------------------- 
rm(cytodat)
rm(trt_conc_dat)
rm(dat2)

# load dat3 and finalize it
dat4 <- get_latest_dat(lvl = "dat3", dataset_title)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]
str(dat4)


# FINALIZE WLLQ

# set wllq to zero where rval is NA
dat4[wllq==1 & is.na(rval),.N] 
# 4634
dat4[is.na(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is NA; "))]

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.

# from 'ToxCast MEA data Outliers and data check.docx'

# 2010317 MW 1076-37 Columns 7 and 8 were dosed incorrectly, can you exclude wells A7-F7 and A8-F8 from the analysis.
for (l in LETTERS[1:6]) {
  updateWllq(dat4, date = "20160317", plate = "MW1076-37", well = paste0(l,"7"), wllq_note = "Mis-dosed", override_check = override_wllq_checks)
  updateWllq(dat4, date = "20160317", plate = "MW1076-37", well = paste0(l,"8"), wllq_note = "Mis-dosed", override_check = override_wllq_checks)
}

# 20160531 MW 1048-15 Columns 1 and 2 were dosed incorrectly, can you exclude wells A1-F1 and A1-F1 from the analysis.
for (l in LETTERS[1:6]) {
  updateWllq(dat4, date = "20160531", plate = "MW1048-15", well = paste0(l,"1"), wllq_note = "Mis-dosed", override_check = override_wllq_checks)
  updateWllq(dat4, date = "20160531", plate = "MW1048-15", well = paste0(l,"2"), wllq_note = "Mis-dosed", override_check = override_wllq_checks)
}


# VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS

# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = "treatment"]
unique(dat4$treatment)

# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
boxplot(rval ~ treatment, dat4[wllq == 1 & !grepl("TX",treatment) & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
stripchart(rval ~ treatment, dat4[wllq == 1 & !grepl("TX",treatment) & grepl("firing_rate",acsn)], 
           ylab = "percent change in mean firing rate", vertical = T, pch = 1, method = "jitter")
title(main = paste0("Percent Change in Mean Firing rate\nin Control wells of ",dataset_title," MEA acute Experiments"))
# RESPONSE:
# yes, it appears that the BIC, LYSIS were added before the second treatment
# rename the treatment in the wells as needed
dat4[grepl("firing",acsn) & treatment == "LYSIS" & rval > -90] # I think this is just a random well that did not completely lyse the cells

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Visually confirm if correct, adjust where needed

# for Cell Titer Blue assay:
stripchart(rval ~ treatment, dat4[wllq==1 & !grepl("TX",treatment) & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "CellTiter Blue Blank-Corrected Fluorescence Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0 & !grepl("TX",treatment) & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", add=T, col = "red")

#let's checkout the few stray wells
dat4[grepl("AB",acsn) & treatment == "LYSIS" & rval > 10000]


# for LDH assay:
stripchart(rval ~ treatment, dat4[wllq==1 & !grepl("TX",treatment) & grepl("(LDH)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "LDH Blank-Corrected Optical Density Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0 & !grepl("TX",treatment) & grepl("(LDH)",acsn)],
           vertical = T, pch = 1, method = "jitter", add=T, col = "red")
# note that the added stripchart does nto follow the same category bins, sadly

# yep, looks like there is LYSIS in the lysis wells. There is plenty of noise though, too. 
dat4[grepl("(Lysis)|(LYSIS)",treatment),unique(treatment)] # all are all caps
dat4[, unique(treatment)]


# CALCULATE PERCENT OF TOTAL LDH (verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)
print(dat4[acsn == "NHEERL_MEA_acute_LDH" & grepl("LYSIS",treatment) & wllq == 1, median(rval), by = "apid"][order(apid)])


# ASSIGN WLLT
# for neural stats endpoints:
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), unique(treatment)]
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "DMSO", wllt := "n"]
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "LYSIS", wllt := "x"] # only positve control for cytotox assays
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "BIC", wllt := "p"] # gain of signal positive control
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(treatment)]
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]

# for cytotox endpoints
dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), unique(treatment)]
dat4[treatment == "LYSIS" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "p"]
dat4[treatment == "DMSO" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "n"]
dat4[treatment == "BIC" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "z"]
dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(treatment)]
dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]
dat4[,unique(wllt)]
# "n" "t" "p" "z" "m"


# CHECK CONC'S (conc's for controls can just make those follow the treatments)
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?

# update conc for control wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] NA
# I'm just gonna gues 0.002, as in Toxcast compounds. bc it doesn't really matter
dat4[treatment == "DMSO", conc := "0.002"]

# bic
dat4[treatment == "BIC", .N, by = "conc"]
#    conc    N
# 1: <NA> 7140
# based on lab notebook, this should always be 25 (there can be mix ups in calc file)
dat4[treatment == "BIC", conc := "25"]

# lysis
dat4[grepl("LYSIS",treatment), .N, by = c("treatment","conc")]
#   treatment conc    N
# 1:     LYSIS <NA> 3570
dat4[treatment == "LYSIS", conc := "100"]

# any other compounds to update?

# make conc's numeric
dat4[, sort(unique(conc))]
dat4[, conc := as.numeric(conc)]
dat4[, sort(unique(conc))]


# ASSIGN SPIDS
spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = "EPA_SAMPLE_ID", new = "spid")
setnames(spidmap, old = "BOTTLE_ID", new = "treatment")
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1] "DMSO"  "BIC"   "LYSIS"
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] "BIC"   "DMSO"  "LYSIS"
dat4[grepl("DMSO",treatment), spid := "DMSO"]
dat4[treatment == "BIC", spid := "Bicuculline"]
dat4[treatment == "LYSIS", spid := "Tritonx100"]
unique(dat4$spid) # confirm no NA spids


# ASSIGN ACID
dat4 <- add_acid(dat4)


# check that all data is there, nothing is missing
unique(dat4$acsn)
# all 17 endpoints present
unique(dat4$wllq) # any NA?
# [1] 
length(unique(dat4$plate.id)) # correct number of plates?
# 1 0
sort(unique(dat4$conc))
check.points <- dcast(dat4[, .N, by = c("acsn","plate.id","experiment.date")], plate.id + experiment.date ~ acsn, value.var = "N")
setnames(check.points, old = names(check.points), new = sub("NHEERL_MEA_acute_","",names(check.points)))
check.points
# 210 plates = 70 experiment dates * 3 plates
check.points[, lapply(.SD, function(x) any(x!=48)), .SDcols = setdiff(names(check.points), c("plate.id","experiment.date"))]
# False for every endpoint - > there are 48 points for every plate for each endpoint

# visualizations/confirmation
boxplot(rval ~ acsn, dat4[wllq == 1 & !grepl("AB",acsn)])
boxplot(rval ~ acsn, dat4[wllq == 1 & grepl("AB",acsn)]) # the only non-normalized endpoint at the moment
plot(dat4[wllq == 1 & !grepl("AB",acsn), .(log10(conc), rval)], xlab = "log10(conc)", ylab = "rval (percent change in activity)")
title(main = paste0("All Points (except AB) for ",dataset_title))

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)

# save dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acsn, acid, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))

# save a copy of dat4 with just the mc0 columns
assign(paste0(dataset_title,"_mc0"), value = dat4[, .(spid, apid, rowi, coli, conc, acsn, wllt, wllq, srcf, rval)])
save(list = c(paste0(dataset_title,"_mc0")), file = file.path(main.output.dir, paste0("output/",dataset_title,"_mc0_",as.character.Date(Sys.Date()),".RData")))

# you're done!
