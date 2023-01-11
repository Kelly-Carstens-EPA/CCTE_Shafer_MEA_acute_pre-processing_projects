rm(list = ls())
# ----------------------------------------------------------------------- #
# USER INPUT
# ----------------------------------------------------------------------- #
start.dir <- "L:/Lab/NHEERL_MEA/Project TSCA 2019/Acute DMSO Addition Study/20220112 Culture"
dataset_title <- "BaselineDeclineAnalysis" # e.g. "name2020"
select.neural.stats.files <- T # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 6 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- ""
use_sheet <- "" # sheet name in spidmap_file
# optional adjutsment; usually can use defaults:
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
# Note: on branch "experimental1"

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
# nothing - no readmes

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  # selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
  
  # I just want all of the CSV files in my folder of interest
  file_names <- list.files(start.dir, pattern = '\\.csv', full.names = T, recursive = T)
  writeLogFile(file_names, output.dir = main.output.dir, dataset_title, files_type = 'neural_stats')
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}

# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(run.type.tag.location, main.output.dir, dataset_title)
# OUTPUT -------------------------------------------------------- #
# 
# Reading from BaselineDeclineAnalysis_neural_stats_files_log_2022-02-11.txt...
# Got 80 files.
# The following files appear to be named incorrectly:
# ...
# --------------------------------------------------------------- #
# meh, I'm not going to worry about this for now

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT -------------------------------------------------------- #
# BaselineDeclineAnalysis_check_summary_2022-02-11.txt is ready.
# --------------------------------------------------------------- #
# No files are missing parameter data
# all files are not about 40 mintues, as expected


# Level 1 - Extract data from files and melt ------------------------------

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT -------------------------------------------------------- #
# 
# Level 1 - Extract All Data:
#   
#   Reading from BaselineDeclineAnalysis_neural_stats_files_log_2022-02-11.txt...
# Got 80 files.
# Reading data from files...
# Processed AC_20220112_20220128_MW78-6311_16_00(000)(000).csv 
# Processed AC_20220112_20220128_MW78-6311_16_00(001)(000).csv 
# ...
# Processed AC_78-6311_20220208_OldMaestro(005)_Statistics Compiler(000).csv 
# Processed AC_78-6311_20220208_OldMaestro(006)_Statistics Compiler(000).csv 
# 
# BaselineDeclineAnalysis_dat1_2022-02-11.RData is ready.
# There were 27 warnings (use warnings() to see them)
warnings()
# Warning messages:
#   1: In fileToLongdat(new_files[i], run.type.tag.location[i],  ... :
#                         no plate.id found forAC_20220112_MW 78-6309_16_00(003)_Statistics Compiler(000).csv
#    ...                   
# --------------------------------------------------------------- #

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
# # print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
# # view all experiment.date's and plate.id's. Are there any NA/missing labels?
# dat1[, .N, by = .(plate.id)]
# #     plate.id     N
# # 1: MW78-6311 33792
# # 2: MW78-6309 44352
# # 3: MW78-6310 33792
# # 4:      <NA> 55728
# dat1[is.na(plate.id), .N, by = .(srcf)] # ah, looks like teh old maestro is formatted differently
# library(stringi)
# dat1[is.na(plate.id), plate.id := stri_extract(srcf, regex = 'MW[ ]*[0-9\\-]{7}')]
# dat1[, plate.id := gsub(' ','',plate.id)]
# dat1[, .N, by = .(plate.id)]
# dat1[is.na(plate.id), plate.id := stri_extract(srcf, regex = '[ ]*[0-9\\-]{7}')]
# dat1[, .N, by = .(plate.id)]
# dat1[!grepl('MW',plate.id), plate.id := paste0('MW',plate.id)]
# 
# # I need to add an ID for which machine was used, bc the same plates were ran on different machines
# dat1[, .N, by = .(setting_axis.version)]
# #    setting_axis.version     N
# # 1:             3.4.1.15 54912
# # 2:              3.5.1.2 57024
# # 3:              2.5.2.1 55728
# # good to know, but I don't know if this correlates with the maestro used
# file_names <- read_files(check.dir = file.path(root_output_dir, dataset_title), files_type = 'neural_stats')
# files.tb <- data.table(full_srcf = file_names)
# files.tb[, srcf := basename(file_names)]
# dat1 <- merge(dat1, files.tb, by = 'srcf', all.x = T)
# # get the 7th subfolder name
# dat1[, maestro_type_foldername := stri_extract(full_srcf, regex = paste0(rep('[^\\/]*[\\/]{1}',7),collapse=""))]
# dat1[, maestro_type_foldername := basename(maestro_type_foldername)]
# dat1[, .N, by = .(maestro_type_foldername)]
# #    maestro_type_foldername     N
# # 1:           Maestro Pro 2 57024
# # 2:           Maestro Pro 1 54912
# # 3:                 Maestro 55728
# # huzzah!!
# setkey(dat1, NULL)
# save(dat1, file = file.path(root_output_dir, dataset_title, 'output', paste0(dataset_title,'_dat1_',as.character.Date(Sys.Date()),'.RData')))

# Loading next day
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)

# Determine the run_type
# Since no treatment added, all of this is baseline
dat1[, run_type := 'baseline']

# Determine the wllq
dat1 <- set_baseline_wllq(dat1, standard_analysis_duration_requirement = F)
dat1[, .N, by = .(run_type, wllq, wllq_notes)]
#    run_type wllq                                           wllq_notes      N
# 1: baseline    0                        Baseline MFR > 3.4036511 Hz;   15072
# 2: baseline    1                                                      116506
# 3: baseline    0                              Baseline # of AE < 10;    1892
# 4: baseline    0                        Baseline MFR < 0.6377603 Hz;   24162
# 5: baseline    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;   10032
dat1[wllq == 0, .N]/nrow(dat1) # 30.5% wllq 0... hmm.

# Do I have the length of the recording time?
names(dat1)
dat1[, .N, by = .(experiment_start_time)]

# What I want:
# - every recording from the same date and plate to have a unifying id

# Looks like the experiment start time is constanst, original file tiem varies with fiel start time
# I want a column that gives the time in seconds/minutes after teh experiment start time
typeof(dat1$original_file_time) # character

dat1[, original_file_time_posix := as.numeric(as.POSIXlt(original_file_time, format = c('%m/%d/%Y %H:%M:%S'))), by = .(srcf)]
dat1[, experiment_start_time_posix := as.numeric(as.POSIXlt(experiment_start_time, format = c('%m/%d/%Y %H:%M:%S'))), by = .(srcf)]

dat1[, .N, by = .(original_file_time, original_file_time_posix)]

dat1[, sec_after_exp_start_time := original_file_time_posix - experiment_start_time_posix]
dat1[, .N, by = .(apid, plate.id, experiment_start_time_posix, original_file_time, sec_after_exp_start_time)]

dat1[, min_after_exp_start_time := sec_after_exp_start_time/60]
dat1[, min_after_exp_start_time_round := signif(sec_after_exp_start_time/60,2)]
dat1[, .N, by = .(min_after_exp_start_time_round)][order(-N)]
# ya know, maybe i can let this variable be continuous

setkey(dat1, NULL)
save(dat1, file = file.path(root_output_dir, dataset_title, 'output', paste0(dataset_title,'_dat1_',as.character.Date(Sys.Date()),'.RData')))


# Update Feb 15, 2022
# Was informed by Kathleen that for Day 27, 
# wells in right half of plate (columns 5-8) contained 10uM of HEPES buffer
# Day 27 (according to the folder under 'Maestro') = 20220208
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
# BaselineDeclineAnalysis_dat1_2022-02-14.RData 
dat1[, .N, by = .(experiment.date)][order(experiment.date)]
dat1[, buffer := ifelse(experiment.date == '20220208' & coli %in% c(5:8), '10uM HEPES', 'none')]
dat1[, .N, by = .(buffer)]
#        buffer      N
# 1:       <NA> 145656
# 2: 10uM HEPES  22008
# overwrite existing version
setkey(dat1, NULL)
save(dat1, file = file.path(root_output_dir, dataset_title, 'output', paste0(dataset_title,'_dat1_2022-02-14.RData')))


# STOPPED HERE
# Just need level 1 for this analysis

# Level 2 - Collapse treated and baseline recordings ----------------------

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title, main.dir = root_output_dir)
# OUTPUT -------------------------------------------------------- #
# 
# --------------------------------------------------------------- #

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT -------------------------------------------------------- #
# 
#
# --------------------------------------------------------------- #
rm(dat2)


# Level 3 - Get cytotoxicity data and merge -------------------------------

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT -------------------------------------------------------- #
# 
# --------------------------------------------------------------- #

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT -------------------------------------------------------- #
# 
# --------------------------------------------------------------- #
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

# save dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acnm, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# you're done!
