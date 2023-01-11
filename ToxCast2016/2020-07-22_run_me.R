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
source('supplemental-scripts/read_conc_response_log_js.R')

# loading some information for the funtions to reference
get_acsn_map() # load the acsn map with the appropriate endpoints

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  # selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
  all_files <- getToxCastFiles(start.dir) # short cut fun for this data set
  writeLogFile(all_files, main.output.dir, dataset_title, files_type = "neural_stats")
  length(all_files) # 414
}
# [1] "skipping L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/Experiment Date/12-2-15 GE HealthCare"
# [1] "skipping L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/Experiment Date/6-7-16"
# [1] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/ToxCast2016_neural_stats_files_log_2020-06-18.txt is ready."
# [1] 414
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}

# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(run.type.tag.location, main.output.dir, dataset_title)
# RESULT --------------------------------------------------------- 
# Reading from ToxCast2016_neural_stats_files_log_2020-06-18.txt...
# Got 414 files.
# All files are named correctly.
# Empty data.table (0 rows and 2 cols): filenames,run.type.tags
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# RESULT --------------------------------------------------------- 
# ToxCast2016_check_summary_2020-06-18.txt is ready
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT --------------------------------------------------------- 
# Reading from ToxCast2016_neural_stats_files_log_2020-06-18.txt...
# Got 414 files.
# Reading data from files...
# Processed TC_MW 1090-24_20151230_20160113_13_00(000)_Neural Statistics Compiler(000).csv
# Processed TC_MW 1090-24_20151230_20160113_13_01(000)_Neural Statistics Compiler(000).csv
# ...
# TC_MW 1076-38_20160303_20160317_14_01(001)_Neural Statistics Compiler(000).csv will be removed. Recording length is 544.25
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat1_2020-06-18.RData is ready.
# ---------------------------------------------------------------- 

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
str(dat1)
dat1[, .N/length(unique(dat1$acsn)), by = "wllq_notes"]

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
# Loading...
# ToxCast2016_dat1_2020-06-18.RData 
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat2_2020-06-18.RData is ready.
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
names(dat2)
rm(dat2)

# get cytotox data
cytodat <- get_ToxCast_cytodat() # from read_conc_response_log_js.R
# OUTPUT --------------------------------------------------------- 
# (see notebook)
# ---------------------------------------------------------------- 
str(cytodat)

# checkout the NA values
cytodat[is.na(rval),.N, by = c("plate.id")]
# comparing with what was in flat file:
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
cytodat2 <- fix_typos_toxcast2016(cytodat, trt_conc_dat)
cytodat <- cytodat2

# save the updated cytodat
save(cytodat, file = paste0("output/",dataset_title,"_cytodat_from_conc_resp_log_",as.character.Date(Sys.Date()),".RData")) # save the updated cytodat
# load(file = paste0("output/",dataset_title,"_cytodat_from_conc_resp_log_2020-06-19.RData"))

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Loading...
# ToxCast2016_dat2_2020-06-18.RData 
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/output/ToxCast2016_dat3_2020-06-19.RData is ready.
# Warning message:
#   In combineNeuralAndCyto(cytodat, main.output.dir, dataset_title) :
#   The following date_plate's are only found in cytodat (and not in dat2): 20151215_MW1089-8, 20151215_MW1090-8
# Wllq will be set to 1 for these plates.
# ---------------------------------------------------------------- 
rm(cytodat)
rm(trt_conc_dat)


# load dat3 and finalize it
dat4 <- get_latest_dat(lvl = "dat3", dataset_title)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]
str(dat4)


# FINALIZE WLLQ

# set wllq to zero where rval is NA
dat4[wllq==1 & is.na(rval),.N] 
# 4570
dat4[is.na(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is NA; "))]

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.

# from 'ToxCast MEA data Outliers and data check.docx':
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

# These wells were set to NA in the flat file
# Kathleen determined that these wells were most likely contaminated with some of the LDH positive control, and should be removed
updateWllq(dat4, date = "20151222", plate = "MW1060-36", well = "E3", wllq_note = "Most likely contaminated with LDH positive control", acsns = c("NHEERL_MEA_acute_LDH"), override_check = override_wllq_checks)
updateWllq(dat4, date = "20151222", plate = "MW1060-36", well = "F3", wllq_note = "Most likely contaminated with LDH positive control", acsns = c("NHEERL_MEA_acute_LDH"), override_check = override_wllq_checks)

# These DMSO wells are extremely off in the CellTiter Blue assay. Most likely that some contents spilled out.
# but the rest of the values on the plate are okay. Don't want to use these DMSO wells as part of normalization
updateWllq(dat4, date = "20151027", plate = "MW1076-38", well = "A1", wllq_note = "Less than half of typical value, likely that some well contents spilled out. Don't want to include for normalization", acsns = c("NHEERL_MEA_acute_AB"), override_check = override_wllq_checks)
updateWllq(dat4, date = "20151027", plate = "MW1076-38", well = "B1", wllq_note = "Less than half of typical value, likely that some well contents spilled out. Don't want to include for normalization", acsns = c("NHEERL_MEA_acute_AB"), override_check = override_wllq_checks)
updateWllq(dat4, date = "20151027", plate = "MW1076-38", well = "C1", wllq_note = "Less than half of typical value, likely that some well contents spilled out. Don't want to include for normalization", acsns = c("NHEERL_MEA_acute_AB"), override_check = override_wllq_checks)



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
dat4[grepl("firing",acsn) & treatment == "LYSIS" & rval > -90] # I think this is just a random well that did not completely lyse the cells
dat4[grepl("firing",acsn) & treatment == "BIC" & rval < -60] 

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
dat4[grepl("LDH",acsn) & treatment == "DMSO" & wllq == 1 & rval > 1] # 3 are from teh same apid...
dat4[grepl("LDH",acsn) & (treatment == "DMSO" | conc < 0.12) & wllq == 1 & apid == "20160405", median(rval)]
# 0.08233333 # I think the bval from this apid will be okay

# yep, looks like there is LYSIS in the lysis wells. There is plenty of noise though, too. 
dat4[grepl("(Lysis)|(LYSIS)",treatment),unique(treatment)] # all are all caps
dat4[, unique(treatment)]


# PREPARE THE LDH DATA (verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)
print(dat4[acsn == "NHEERL_MEA_acute_LDH" & grepl("LYSIS",treatment) & wllq == 1, median(rval), by = "apid"][order(apid)])


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


# ASSIGN WLLT
# make sure all non-tested spids are in 
# "DMSO" "Media"         "Picrotoxin"    "Tetrodotoxin" "Tritonx100" "Bicuculline"
# make sure there are no NA spids
dat4[,unique(spid)]
dat4 <- assign_wllt(dat4)
# treatment        spid CellTiter Blue LDH MEA endpoints
# 1:       BIC Bicuculline              z   z             p
# 2:      DMSO        DMSO              n   n             n
# 3:     LYSIS  Tritonx100              p   p             v
dat4[,unique(wllt)]
# "p" "z" "n" "v" "t"


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


# ASSIGN ACID
# dat4 <- add_acid(dat4) # need to register new acnm's, skip for now


# check that all data is there, nothing is missing
unique(dat4$acsn)
# all 17 endpoints present
unique(dat4$wllq) # any NA?
# [1] 
length(unique(dat4$plate.id)) # correct number of plates?
# 175
length(unique(dat4$experiment.date))
# 69
sort(unique(dat4$conc))
check.points <- dcast(dat4[, .N, by = c("acsn","plate.id","experiment.date")], plate.id + experiment.date ~ acsn, value.var = "N")
setnames(check.points, old = names(check.points), new = sub("NHEERL_MEA_acute_","",names(check.points)))
check.points
# 209 plates = 69 experiment dates * 3 plates + 2 cytotox plates that were named differently for 20151215
check.points[1:50] # 48 points per plate for every endpoint
check.points[51:100] # 48 points per plate for every endpoint
check.points[101:150] # 48 points per plate for every endpoint, except fro MW1089-8 20151215 only has 48 pts for cyto endpoints, none for MEA
check.points[151:200] # 48 points per plate for every endpoint, except for MW1090-8 20151215 only has 48 pts for cyto endpoints, none for MEA and MW1089-90 20151215 has no cyto pts
check.points[200:209] # 48 points per plate for every endpoint
# I guess I missed it, but just search check.points and confirmed MW1090-6 20151215 has 48 pts for all MEA endpoints, none for cyto endpoints

# visualizations/confirmation
boxplot(rval ~ acsn, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acsn)])
boxplot(rval ~ acsn, dat4[wllq == 1 & grepl("AB",acsn)])
boxplot(rval ~ acsn, dat4[wllq == 1 & grepl("LDH",acsn)])
plot(dat4[wllq == 1 & !grepl("(AB)|(LDH)",acsn), .(log10(conc), rval)], xlab = "log10(conc)", ylab = "rval (percent change in activity)")
title(main = paste0("All Neural Stats Points where wllq=1 for ",dataset_title))

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)

# save dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acsn, acid, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))

# you're done!
