###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project TSCA_APCRA"
dataset_title <- "APCRA2019" # e.g. "name2020"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/" # where the dataset_title folder will be created
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- "L:/Lab/NHEERL_MEA/Project TSCA_APCRA/EPA_18235_EPA-Shafer_84_20181129.xlsx"
use_sheet <- "EPAOrder313 - Mosaic18235_13802" # sheet name in spidmap_file
override_wllq_checks <- TRUE # set to TRUE if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- FALSE
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

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(path = "../mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)

# loading acsn_acnm map
acsn_map <- as.data.table(read.csv(file.path(root_output_dir,"neural_stats_acsn_to_tcpl_acnm_map.csv")))
acsn_map <- acsn_map[, .(acsn, acnm)]

cat(paste0(dataset_title, " MEA Acute TCPL Level 0 Data Prep Running Log\nDate: ",as.character.Date(Sys.Date()),"\n"))
cat("\nLevel 0 - Gather and Check Files:\n")

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}

# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(run.type.tag.location, main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Reading from APCRA2019_neural_stats_files_log_2020-05-21.txt...
# Got 94 files.
# The following files appear to be named incorrectly:
#   filenames run.type.tags
# 1: TC_20190417_MW67-3707_13_00(000).csv            00
# 2: TC_20190417_MW67-3707_15_01(000).csv            01
# ----------------------------------------------------------------

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title),
         error = function(e){
           closeAllConnections()
           e } )
# OUTPUT ---------------------------------------------------------
# APCRA2019_check_summary_2020-06-04.txt is ready.
# ----------------------------------------------------------------

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location)
# OUTPUT ---------------------------------------------------------
# Reading from APCRA2019_neural_stats_files_log_2020-05-21.txt...
# Got 94 files.
# Reading data from files...
# Processed TC_20190313_MW1236-16_13_00(000).csv
# Processed TC_20190313_MW1236-16_13_01(000).csv
# Processed TC_20190313_MW1236-17_13_00(000).csv
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/output/APCRA2019_dat1_2020-06-04.RData is ready.
# ----------------------------------------------------------------

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
# str(dat1)
print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
cat("I noticed that there is a plate with 'No Barcode'. Will rename that now, then resave dat1.\n")
dat1[grepl("No Barcode",plate.id), unique(srcf)]
# [1] "TC_20190320_MW1237-11_15_00(000).csv" "TC_20190320_MW1237-11_15_01(000).csv" I think Kathleen renamed these files to include the plate id
dat1[plate.id == "MW1237-11"] # empty, confirmign that this plate.id has not been used
dat1[grepl("No Barcode",plate.id), plate.id := "MW1237-11"]
save(dat1, file = paste0("output/",dataset_title,"_dat1_",as.character.Date(Sys.Date()),".RData")) # resave updated dat1
rm(dat1)


# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Loading...
# APCRA2019_dat1_2020-06-19.RData
#
# Collapsing treated and baseline data...
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/output/APCRA2019_dat2_2020-06-19.RData is ready.
# ----------------------------------------------------------------

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT ---------------------------------------------------------
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's
# -100.000  -39.898   -6.097   -7.725    8.168 3926.560      972
# ----------------------------------------------------------------
dat2[rval > 2500 & wllq == 1, .(acnm, apid, plate.id, rval, wllq)] # all from NHEERL_MEA_acute_burst_percentage_std
dat2[is.na(rval), .N, by = "wllq"]
rm(dat2)

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Reading from APCRA2019_calculations_files_log_2020-05-21.txt...
# Got 16 files.
# Reading data from files...
#
# 20190313_Calculations_Group_1_checked.xlsxNew names:
#   * `` -> ...1
# * `` -> ...2
# * `` -> ...3
# ...
# ----------------------------------------------------------------

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Loading...
# APCRA2019_dat2_2020-06-19.RData
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/output/APCRA2019_dat3_2020-06-19.RData is ready.
# Warning message:
#   In combineNeuralAndCyto(cytodat, main.output.dir, dataset_title) :
#   The following date_plate's are only found in cytodat (and not in dat2): 20190402_MW1236-24
# Wllq will be set to 1 for these plates.
# ----------------------------------------------------------------
rm(cytodat)


# load dat3 and finalize it
cat("\n\nLevel 4 - Finalize well ID information:\n")
dat4 <- get_latest_dat(lvl = "dat3", dataset_title)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]


# FINALIZE WLLQ
cat("\nFinalize Wllq:")
# set wllq to zero where rval is NA
cat("\nNA rval's:",dat4[wllq==1 & is.na(rval),.N])
#
cat("\nInf rval's (baseline==0):",dat4[wllq==1 & is.infinite(rval),.N])
# 
dat4[is.na(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is NA; "))]
dat4[is.infinite(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is Inf; "))]
cat("\nWell quality set to 0 for these rval's.\n")

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.
# for example, updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "C6", wllq_note = "Contamination")
updateWllq(dat4, date = "20190416", plate = "MW66-9613", well = "D6", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190425", plate = "MW66-9817", well = "D6", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190425", plate = "MW66-9818", well = "F3", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190502", plate = "MW67-3707", well = "B8", wllq_note = "Excessive foaming", acnms = c("CCTE_Shafer_MEA_acute_AB"), override_check = override_wllq_checks)
updateWllq(dat4, date = "20190502", plate = "MW67-3708", well = "B8", wllq_note = "Excessive foaming", acnms = c("CCTE_Shafer_MEA_acute_AB"), override_check = override_wllq_checks)
updateWllq(dat4, date = "20190509", plate = "MW67-3714", well = "B3", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190516", plate = "MW68-0719", well = "A7", wllq_note = "Contamination", override_check = override_wllq_checks)

# start a pdf to save the summary graphs
graphics.off()
pdf(file = file.path(main.output.dir, paste0(dataset_title, "_summary_figures_report_",as.character.Date(Sys.Date()),".pdf")), width = 10, height = 8)

# VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS

cat("\nVerifying control compound labels:\n")
# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = "treatment"][order(N)]
dat4[, original_treatment := treatment]
dat4[grepl("TTX",original_treatment), treatment := "TTX"]
dat4[grepl("DMSO",original_treatment), treatment := "DMSO"]
dat4[grepl("PICRO",original_treatment), treatment := "PICRO"]

# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","LYSIS_Control","LYSIS_1","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# based on conversation with Kathleen, I know that the Controls were added after the second recording in the first culture (Mar 13)
dat4[experiment.date %in% c("20190326","20190328"), unique(treatment)]
dat4[experiment.date %in% c("20190326","20190328") & grepl("LYSIS",treatment) & grepl("firing",acnm)] # all well F6, LYSIS_Control
dat4[experiment.date %in% c("20190326","20190328") & treatment %in% c("TTX","PICRO","LYSIS_Control") & !grepl("(AB)|(LDH)",acnm),
     `:=` (treatment = "Media")]
# RESPONSE:
# yes, it appears that the PICRO, TTX, LYSIS were added before the second treatment after the first culture
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","LYSIS_Control","LYSIS_1","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "Control Compounds from First Culture Updated")


# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Visually confirm if correct, adjust where needed

# for Cell Titer Blue assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","LYSIS_Control","LYSIS_1","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")

dat4[rowi == 6 & coli == 1 & grepl("AB",acnm), unique(treatment)] # [1] "LYSIS_Control" "LYSIS_1"   


# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","LYSIS_Control","LYSIS_1","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acnm)]
view_activity_stripchart(plotdat)
# I think that all of these are correct.
# based on the values of the Lysis_Control points, I don't think these are just Media
# I think it is very likely in the first culture that PICRO, TTX were added after the second recording, 
# but while the plates still on the MAESTRO, so that they could see the activity. Which means TTx,PICRO would be added before the LDH media extracted
# I don't know about the Lysis_Control well for sure, but I have a very reasonable hunch that is does contain Lysis
# I definitely think that the LYSIS_1 wells contain Lysis for the LDH assay

# final treatment updates:
cat("Confirm that the rest of these treatments look normal (nothing NA, 0, etc):\n")
cat(dat4[, unique(treatment)], sep = ", ")
cat("\n")


# ASSIGN SPIDS
cat("\nAssign spid's:\n")
cat("Using spidmap file:",spidmap_file,"\n")
# standardizing the trt now
setnames(dat4, old = "treatment", new = "treatment_string")
dat4[, unique(treatment_string)]
dat4[, treatment := unlist(lapply(treatment_string, function(x) strsplit(x, split = "_")[[1]][1]))]
dat4[, unique(treatment)]
dat4[grepl("(YSIS)|(ysis)",treatment_string), unique(treatment_string)]
# [1] "LYSIS_1"       "LYSIS_Control" "Lysis"         "2 * ½ Lysis"
dat4[treatment == "LYSIS", treatment := "Lysis"]
dat4[, unique(treatment)]

spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = c("EPA_SAMPLE_ID","ALIQUOT_WELL_ID"), new = c("spid","treatment"))
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1] "Media"       "DMSO"        "TTX"         "PICRO"       "Lysis"       "2 * ½ Lysis"
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] "2 * ½ Lysis" "DMSO"        "Lysis"       "Media"       "PICRO"       "TTX"
dat4[grepl("DMSO",treatment), spid := "DMSO"]
dat4[treatment == "Media", spid := "Media"]
dat4[treatment == "PICRO", spid := "Picrotoxin"]
dat4[treatment == "TTX", spid := "Tetrodotoxin"]
dat4[grepl("Lysis",treatment), spid := "Tritonx100"]
dat4[grepl("Lysis",treatment), unique(conc), by = "treatment"]
unique(dat4$spid) # confirm no NA spids
dat4[, .(paste0(unique(original_treatment), collapse=",")), by = "spid"]
# # confirm have correct spids by matching what is in flat file
# flatfile <- as.data.table(read_excel(path = "L:/Lab/NHEERL_MEA/Project TSCA_APCRA/APCRA_MEA_Data_With_Sync_Index_&_Cytotoxicity.xlsx"))
# head(flatfile)
# setdiff(unique(dat4$spid), unique(flatfile$EPA_SAMPLE_ID))
# # [1] "Tritonx100"   "Media"        "Picrotoxin"   "Tetrodotoxin"
# "TTX" %in% unique(flatfile$EPA_SAMPLE_ID) # TRUE
# "PICRO" %in% unique(flatfile$EPA_SAMPLE_ID) # TRUE
# "LYSIS" %in% unique(flatfile$EPA_SAMPLE_ID) # TRUE
# # don't expect Media, since I added that for the first culture
# "MEDIA" %in% unique(flatfile$EPA_SAMPLE_ID) # FALSE, that's okay though
# setdiff(unique(flatfile$EPA_SAMPLE_ID), unique(dat4$spid))
# # [1] "TTX"   "PICRO" "LYSIS" Got it. So there are no differences/missing treated compounds, check.
if(any(is.na(unique(dat4$spid)))) {
  warning(paste0("The following treatments don't have a corresponding spid:", paste0(dat4[is.na(spid), unique(treatment)],collapse=", ")))
} else {
  cat("No spids are NA.\n")
}
cat("Number of unique spids:",dat4[,length(unique(spid))],"\n")


# PREPARE LDH P WELLS (verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)


# ASSIGN WLLT
dat4 <- assign_wllt(dat4)


# CHECK CONC'S (conc's for controls can just make those follow the treatments)
cat("\nFinalize Concentrations:\n")
dat4[, conc_original := conc]
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?
# "½ Lysis" "0.03"    "0.1"     "0.3"     "1"       "3"       "10"      "30"      "Control" "Lysis"   "25"

# update conc for control wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] "Control"
dat4[treatment == "DMSO", conc := "0.0015"]

# picro
dat4[treatment == "PICRO", unique(conc), by = "experiment.date"]
# based on lab notebook, this should always be 25 (there can be mix ups in calc file)
dat4[treatment == "PICRO", conc := "25"]

# ttx
dat4[treatment == "TTX", unique(conc), by = "experiment.date"]
# based on lab notebooks, this should always be 1
dat4[treatment == "TTX", conc := "1"]

cat("\nConcentration Corrections:\n")

# update concentrations using stock concentrations
load("samples_stkc_invitrodb_2020-06-12.RData")
cat("The following treatment have char conc. Will be set to NA:\n")
print(suppressWarnings(dat4[is.na(as.numeric(conc)), .N, by = c("spid","treatment","conc")]))
dat4[, conc := suppressWarnings(as.numeric(conc))]

samples[abs(stkc - 20) > 1]
# spid  chid    stkc stkc_unit tested_conc_unit
# 1: EPAPLT0154G04 44520 15.6503        mM               uM
# 2: EPAPLT0154G07 40734 10.0000        mM               uM
dat4[spid %in% c("EPAPLT0154G04","EPAPLT0154G07"), unique(conc)]
# 0.03  0.10  0.30  1.00  3.00 10.00 30.00 # there is not way these spids were conc-corrected
dat4 <- merge(dat4, samples[, .(spid, stkc)], by = c("spid"), all.x = T)
dat4[wllt == "t", conc := signif(stkc/20 * conc, 3)] # adapted from TcPL SOP
cat("conc's corrected for all samples using the file samples_stkc_invitrodb_2020-06-12.RData\n")

dat4 <- assign_common_conc(dat4, check_conc_correction = FALSE)


# ASSIGN ACID
# cat("\nAssign ACId:\n")
# dat4 <- add_acid(dat4) # holding off, need to register new acid's


# check that all data is there, nothing is missing
data_checks(dat4)

# closing graphics after last plots
graphics.off()

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)
cat("(note that the wllq is not quite final -\nwllq will be updated for outlier DMSO wells will before creating lvl 0 snapshot)\n")

# save dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acnm, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# you're done!
