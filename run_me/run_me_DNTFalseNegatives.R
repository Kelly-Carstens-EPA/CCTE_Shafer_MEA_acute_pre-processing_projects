rm(list = ls())
# ----------------------------------------------------------------------- #
# USER INPUT
# ----------------------------------------------------------------------- #
start.dir <- "L:/Lab/NHEERL_MEA/Project - DNT  False Negatives/20210818_False Negatives_Acute"
dataset_title <- "DNTFalseNegatives" # e.g. "name2020"
select.neural.stats.files <- T # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- T # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 3 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- ""
use_sheet <- "" # sheet name in spidmap_file
# optional adjustment; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl" # where the dataset_title folder will be created
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- TRUE
standard_analysis_duration_requirement <- TRUE # default should be true
# ----------------------------------------------------------------------- #
# END USER INPUT
# ----------------------------------------------------------------------- #

library(data.table)
library(openxlsx)

# set up folders and working directory
if (!dir.exists(file.path(root_output_dir,dataset_title))) dir.create(file.path(root_output_dir,dataset_title))
setwd(file.path(root_output_dir,dataset_title))
main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(path = "../mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)

# loading acsn_acnm map
acsn_map <- as.data.table(read.csv(file.path(root_output_dir,"neural_stats_acsn_to_tcpl_acnm_map.csv")))
acsn_map <- acsn_map[, .(acsn, acnm)]


# Level 0 - Gather and Check Files ----------------------------------------

cat(paste0(dataset_title, " MEA Acute TCPL Level 0 Data Prep Running Log\nDate: ",as.character.Date(Sys.Date()),"\n"))
cat("\nLevel 0 - Gather and Check Files:\n")

# Scan for readme's that might affect dosing, wllq
txt.files <- list.files(path = start.dir, pattern = '\\.txt', recursive = T, full.names = T)
readmes <- txt.files[grepl('read( )*me',tolower(txt.files))]
for (readme in readmes) {
  cat(dirname(readme),'\n')
  cat(scan(readme, what = character(), sep = '\n', quiet = T), sep = '\n')
  cat('\n')
}
length(readmes) # 0

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
# OUTPUT -------------------------------------------------------- #
# Reading from DNTFalseNegatives_neural_stats_files_log_2022-03-21.txt...
# Got 6 files.
# The following files appear to be named incorrectly:
# (all of them listed - we will have to find the run type another way)
# (expected _ between plate file name and 0's and 1's part)
# --------------------------------------------------------------- #

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT -------------------------------------------------------- #
# DNTFalseNegatives_check_summary_2022-03-21.txt is ready.
# --------------------------------------------------------------- #


# Level 1 - Extract data from files and melt ------------------------------

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT -------------------------------------------------------- #
# Level 1 - Extract All Data:
#
# Reading from DNTFalseNegatives_neural_stats_files_log_2022-03-21.txt...
# Got 6 files.
# Reading data from files...
# Processed AC_20210818_MW75-9208(000)(001).csv 
# Processed AC_20210818_MW75-9208(001)(001).csv 
# Processed AC_20210818_MW75-9209(000)(001).csv 
# Processed AC_20210818_MW75-9209(001)(001).csv 
# Processed AC_20210818_MW75-9210(000)(001).csv 
# Processed AC_20210818_MW75-9210(001)(001).csv 
# 
# DNTFalseNegatives_dat1_2022-03-21.RData is ready.
# Summary of dates/plates with wllq=0 at Level 1:
#   Empty data.table (0 rows and 3 cols): experiment.date,plate.id,wllq_set_to_zero
# Warning messages:
#   1: In fileTodat1(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                         
#                         run type cannot be determined for AC_20210818_MW75-9208(000)(001).csv.
#                       No wllq checks will be done for this recording.
#                       2: In fileTodat1(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                                             
#                                             run type cannot be determined for AC_20210818_MW75-9208(001)(001).csv.
#                                           No wllq checks will be done for this recording.
#                                           3: In fileTodat1(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                                                                 
#                                                                 run type cannot be determined for AC_20210818_MW75-9209(000)(001).csv.
#                                                               No wllq checks will be done for this recording.
#                                                               4: In fileTodat1(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                                                                                     
#                                                                                     run type cannot be determined for AC_20210818_MW75-9209(001)(001).csv.
#                                                                                   No wllq checks will be done for this recording.
#                                                                                   5: In fileTodat1(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                                                                                                         
#                                                                                                         run type cannot be determined for AC_20210818_MW75-9210(000)(001).csv.
#                                                                                                       No wllq checks will be done for this recording.
#                                                                                                       6: In fileTodat1(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                                                                                                                             
#                                                                                                                             run type cannot be determined for AC_20210818_MW75-9210(001)(001).csv.
#                                                                                                                           No wllq checks will be done for this recording.
# --------------------------------------------------------------- #

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
# view all experiment.date's and plate.id's. Are there any NA/missing labels?
# wllq_notes  V1
# 1:            288
dat1[, .N, by = .(experiment.date, plate.id)]
#    experiment.date  plate.id    N
# 1:        20210831 MW75-9208 4224
# 2:        20210831 MW75-9209 4224
# 3:        20210831 MW75-9210 4224
# yep, all present, none are NA

# Manually assign run type
dat1[, .N, by = .(srcf, run_type)]
#                                   srcf            run_type    N
# 1: AC_20210818_MW75-9208(000)(001).csv MW75-9208(000)(001) 2112
# 2: AC_20210818_MW75-9208(001)(001).csv MW75-9208(001)(001) 2112
# 3: AC_20210818_MW75-9209(000)(001).csv MW75-9209(000)(001) 2112
# 4: AC_20210818_MW75-9209(001)(001).csv MW75-9209(001)(001) 2112
# 5: AC_20210818_MW75-9210(000)(001).csv MW75-9210(000)(001) 2112
# 6: AC_20210818_MW75-9210(001)(001).csv MW75-9210(001)(001) 2112
dat1[, srcf_tag3_run_type := run_type] # saving currently pulled out tag in another column
library(stringi)
dat1[stri_detect(srcf_tag3_run_type, fixed = '(000)(001)'), run_type := 'baseline']
dat1[stri_detect(srcf_tag3_run_type, fixed = '(001)(001)'), run_type := 'treated']
dat1[, .N, by = .(srcf_tag3_run_type, run_type)]
#     srcf_tag3_run_type run_type    N
# 1: MW75-9208(000)(001) baseline 2112
# 2: MW75-9208(001)(001)  treated 2112
# 3: MW75-9209(000)(001) baseline 2112
# 4: MW75-9209(001)(001)  treated 2112
# 5: MW75-9210(000)(001) baseline 2112
# 6: MW75-9210(001)(001)  treated 2112

# Manually update wllq for baseline wells
# Modified from fileTodat1 Mar 21, 2022
# SET THE WELL QUALITY

# for baseline recordings, do 2 checks for wllq

dat1[run_type == "baseline", wllq := 1] # set the default wllq
dat1[run_type == "baseline", wllq_notes := ""]

# if nAE is less than 10 or is NA, set wllq=0
low_ae_wells <- dat1[run_type == "baseline" & acsn == "Number of Active Electrodes" & (activity_value < 10 | is.na(activity_value)), well]
dat1[run_type == "baseline" & well %in% low_ae_wells, `:=` (wllq = 0, wllq_notes = "Baseline # of AE < 10; ")]

# if the MFR is very low or near the theoretical upper limit, remove that well
# see the script mfr_baseline_cutoff_investigation.R 
# or the notbeook 'MEA Acute Pre-Process for TCPL', Tab "Development", Page "Mean Firing Rate Baseline Cutoff"
# for more details
mfr_upper_threshold <- 3.4036511 # this is the 95th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
mfr_lower_threshold <- 0.6377603 # this is the 5th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
high_mfr_wells <- dat1[run_type == "baseline" & acsn == "Mean Firing Rate (Hz)" & activity_value > mfr_upper_threshold, well]
dat1[run_type == "baseline" & well %in% high_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR > ",mfr_upper_threshold," Hz; "))]
low_mfr_wells <- dat1[run_type == "baseline" & acsn == "Mean Firing Rate (Hz)" & (activity_value < mfr_lower_threshold | is.na(activity_value)), well]
dat1[run_type == "baseline" & well %in% low_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR < ",mfr_lower_threshold," Hz; "))]

# For treated, don't assign wllq for treated wells (yet)
dat1[run_type == "tested", `:=` (wllq = NA_integer_, wllq_notes = "")]

# for baseline or treated, if recording length is very short or very  long, remove it
if (standard_analysis_duration_requirement) {
  # if (abs(analysis_duration - 2400) > 1400) cat(basename(filei),"will be removed. Recording length is",analysis_duration,"\n")
  dat1[analysis_duration < 1000, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length < 1000 s; "))]
  dat1[analysis_duration > 3800, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length > 3800 s; "))]    
}

dat1[, .N, by = .(run_type, wllq, wllq_notes)]
#    run_type wllq                                           wllq_notes    N
# 1: baseline    1                                                      5676
# 2: baseline    0                              Baseline # of AE < 10;   132
# 3: baseline    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;   528
# 4:  treated   NA                                                      6336

# Save updated dat1
setkey(dat1, NULL)
save(dat1, file = file.path('output',paste0(dataset_title, '_dat1_',as.character.Date(Sys.Date()),'.RData')))
rm(dat1)


# Level 2 - Collapse treated and baseline recordings ----------------------

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title, main.dir = root_output_dir)
# OUTPUT -------------------------------------------------------- #
# Level 2 - Collapse Data by Plate ID:
# 
# Loading...
# DNTFalseNegatives_dat1_2022-03-21.RData 
# 
# Collapsing treated and baseline data...
# 20210831_MW75-9208
# Baseline stats file name: AC_20210818_MW75-9208(000)(001).csv
# Treated stats file name: AC_20210818_MW75-9208(001)(001).csv
# 
# 20210831_MW75-9209
# Baseline stats file name: AC_20210818_MW75-9209(000)(001).csv
# Treated stats file name: AC_20210818_MW75-9209(001)(001).csv
# 
# 20210831_MW75-9210
# Baseline stats file name: AC_20210818_MW75-9210(000)(001).csv
# Treated stats file name: AC_20210818_MW75-9210(001)(001).csv
# 
# DNTFalseNegatives_dat2_2022-03-21.RData is ready.
# --------------------------------------------------------------- #

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT -------------------------------------------------------- #
# DNTFalseNegatives_dat2_2022-03-21.RData 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -100.01  -82.64  -41.36     Inf    0.00     Inf     948 
# --------------------------------------------------------------- #
rm(dat2)


# Level 3 - Get cytotoxicity data and merge -------------------------------

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT -------------------------------------------------------- #
# Load Cytotoxicity Data:
# 
# Reading from DNTFalseNegatives_calculations_files_log_2022-03-21.txt...
# Got 1 files.
# Reading data from files...
# 
# 20210818_Calculations_False Negative.xlsx
# AB 
# MW75-9208 MW75-9209 MW75-9210 
# some values are negative. These will be set to 0
# LDH 
# MW75-9208 MW75-9209 MW75-9210 
# some values are negative. These will be set to 0
# There are no NA values in cytodat.
# 
# cytodat is ready
# --------------------------------------------------------------- #

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT -------------------------------------------------------- #
# Level 3 - Combine Cyto and Neural Stats Data; Initialize treatment, conc, and wllq
# 
# Loading...
# DNTFalseNegatives_dat2_2022-03-21.RData 
# Error in combineNeuralAndCyto(cytodat, main.output.dir, dataset_title) : 
#   The following date_plate's are only found in dat2 (and not in cytodat): 20210831_MW75-9208, 20210831_MW75-9209, 20210831_MW75-9210
# In addition: Warning message:
# In combineNeuralAndCyto(cytodat, main.output.dir, dataset_title) :
#   The following date_plate's are only found in cytodat (and not in dat2): 20210826_MW75-9208, 20210826_MW75-9209, 20210826_MW75-9210
# Wllq will be set to 1 for all wells on these LDH/AB plates.
# --------------------------------------------------------------- #
# Seems like culture was on 20210818
# Experiment was 20210831
rm(cytodat)



# Level 4 - Finalize well ID information ----------------------------------

# load dat3 and finalize it
cat("\n\nLevel 4 - Finalize well ID information:\n")
dat4 <- get_latest_dat(lvl = "dat3", dataset_title)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]


# * FINALIZE WLLQ ---------------------------------------------------------
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
# for example, updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "C6", wllq_note = "Contamination", override_check = override_wllq_checks)

# start a pdf to save the summary graphs
graphics.off()
pdf(file = file.path(main.output.dir, paste0(dataset_title, "_summary_figures_report_",as.character.Date(Sys.Date()),".pdf")), width = 10, height = 8)


# * VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS -----

cat("\nVerifying control compound labels:\n")
# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = "treatment"]
dat4[grepl("DMSO",treatment), treatment := "DMSO"]

# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# RESPONSE:
# yes/no, it appears that the PICRO, TTX, LYSIS were added before the second treatment
# rename the treatment in the wells as needed

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to refect this

# for Cell Titer Blue assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# make updates if needed
# dat4[, AB.trt.finalized := FALSE] # set this to TRUE for individual plates as you update as needed
# 
# # for every other culture, the "Media" well in F1 contains Lysis at the time of the AB reading (or could change by well F1 vs by the name "Media"...)
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", .(plate.id, experiment.date, rowi, coli, wllq, rval, wllq_notes)] # all are in row 6, col 1
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", `:=`(treatment = "Lysis",conc = 10, AB.trt.finalized = TRUE)]

# # view updated stripchart
# plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
# view_activity_stripchart(plotdat, title_additions = "Media renamed to Lysis")

# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")

# looks like media wells really do just contain Media
# actually makes some sense based on the assay - 
# first, 50uL of culture Media from each well in MEA plate is transfered to LDH plate (so F1 just contains Media in LDH plate)
# then 20uL of Media is added to F1 in the MEA plate. Then (all?) of the contents are removed and mixed
# a Media/Alamar Blue mixture is added to eadch well of MEA plate
# then culture Media/Blue mixutre is taken from MEA plates to CTB plates
# all this to say, I understand now why the 
# The only time when well F1 would have Lysis in the LDH assay is the same time when Lysis is added before the second recording,
# which was already clearly marked in the Group 1 file. 
# So LDH well treatments should usually = neural stats well treatments, if Lysis, PICRO, etc. were added before the second recording

# final treatment updates:
cat("Confirm that the rest of these treatments look normal (nothing NA, 0, etc):\n")
cat(dat4[, unique(treatment)], sep = ", ")
cat("\n")



# * ASSIGN SPIDS ----------------------------------------------------------

cat("\nAssign spid's:\n")
cat("Using spidmap file:",spidmap_file,"\n")
spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = "NCCT ID", new = "spid")
setnames(spidmap, old = "Chemical ID", new = "treatment")
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1]   
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] 
dat4[grepl("DMSO",treatment), spid := "DMSO"]
dat4[treatment == "Media", spid := "Media"]
dat4[treatment == "PICRO", spid := "Picrotoxin"]
dat4[treatment == "TTX", spid := "Tetrodotoxin"]
dat4[grepl("Lysis",treatment), spid := "Tritonx100"]
dat4[grepl("Lysis",treatment), unique(conc), by = "treatment"]
unique(dat4$spid) # confirm no NA spids
if(any(is.na(unique(dat4$spid)))) {
  stop(paste0("The following treatments don't have a corresponding spid:", dat4[is.na(spid), unique(treatment)]))
} else {
  cat("No spids are NA.\n")
}
cat("Number of unique spids:",dat4[,length(unique(spid))],"\n")



# * PREPARE LDH P WELLS  --------------------------------------------------
# (must verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)



# * ASSIGN WLLT -----------------------------------------------------------
dat4 <- assign_wllt(dat4)



# * CHECK CONC'S ----------------------------------------------------------
cat("\nFinalize Concentrations:\n")
dat4[, conc_original := conc]
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?

# update conc for DMSO, PICRO, TTX, BIC, and full Lysis wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] "Control"
# Use the percent DMSO by volume?
# dat4[treatment == "DMSO", conc := "0.001"]

# picro
dat4[treatment == "PICRO", .N, by = "conc"]
# 
# based on lab notebook, this is usually 25
# dat4[treatment == "PICRO", conc := "25"]

# ttx
dat4[treatment == "TTX", .N, by = "conc"]
# 
# based on lab notebook, this is usually 1
# dat4[treatment == "TTX", conc := "1"]

cat("\nConcentration Corrections:\n")
# any other compounds to update??
# need to do concentration correction??
cat("CHANGES MADE/rationale")
# cat("The following treatment have char conc. Will be set to NA:\n")
# print(suppressWarnings(dat4[is.na(as.numeric(conc)), .N, by = c("spid","treatment","conc")]))
# dat4[, conc := suppressWarnings(as.numeric(conc))]

# final updates, view conc's, make table of control conc's
dat4 <- assign_common_conc(dat4)



# * ASSIGN ACID -----------------------------------------------------------
cat("\nAssign ACId:\n")
cat("(not doing this for now, since new acnm's need to be registered)\n")
# dat4 <- add_acid(dat4) # holding off, need to register new acid's



# Final checks and save ---------------------------------------------------

# check that all data is there, nothing is missing, view plots
data_checks(dat4)

# closing graphics after last plots
graphics.off()

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)
cat("(note that the wllq is not quite final -\nwllq will be updated for outlier DMSO wells will before creating lvl 0 snapshot)\n")

# Check for the expected number of technical replicates
dat4[wllt == 't', .(length(unique(paste0(apid,rowi,coli)))), by = .(spid, conc)][V1 != 3]
# do you except these cases to have more than or less than 3 replicates?
# Were some samples repeated, and only certain repeats meant to be included?

# save dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acnm, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# you're done!