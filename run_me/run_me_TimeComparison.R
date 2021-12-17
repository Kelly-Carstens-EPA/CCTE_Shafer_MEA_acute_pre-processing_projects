rm(list = ls())
###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/TSCA_2019/Equalibration Time Comparison"
dataset_title <- "TimeComparison" # e.g. "name2020"
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- ""
use_sheet <- "" # sheet name in spidmap_file
# optional adjutsment; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl" # where the dataset_title folder will be created
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- TRUE
standard_analysis_duration_requirement <- FALSE # default should be true
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(openxlsx)

# set up folders and working directory
if (!dir.exists(file.path(root_output_dir,dataset_title))) dir.create(file.path(root_output_dir,dataset_title))
# setwd(file.path(root_output_dir,dataset_title))
setwd(file.path(dataset_title))
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

# Get file smyself
neural.stats.files <- list.files(path = 'L:/Lab/NHEERL_MEA/Project TSCA 2019/Equalibration Time Comparison', pattern = '\\.csv', recursive = T, full.names = T)
writeLogFile(neural.stats.files, main.output.dir, dataset_title, files_type = "neural_stats")

# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(run.type.tag.location, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Reading from TimeComparison_neural_stats_files_log_2021-09-13.txt...
# Got 108 files.
# All files are named correctly.
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# TimeComparison_check_summary_2021-09-13.txt is ready.
# ---------------------------------------------------------------- 
# only warnings pertain to analysis duration, this is okay


# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT --------------------------------------------------------- 
# Level 1 - Extract All Data:
# 
# Reading from TimeComparison_neural_stats_files_log_2021-09-13.txt...
# Got 108 files.
# Reading data from files...
# Processed AC_20210324_MW75-8015_15_00_40.csv 
# Processed AC_20210324_MW75-8015_15_01_15.csv 
# Processed AC_20210324_MW75-8015_15_01_20.csv 
# Processed AC_20210324_MW75-8015_15_01_25.csv 
# Processed AC_20210324_MW75-8015_15_01_30.csv 
# Processed AC_20210811_MW75-9204_15_01_30.csv 
# ...
# Processed AC_20210811_MW75-9204_15_01_40.csv 
# 
# TimeComparison_dat1_2021-09-13.RData is ready.
# Summary of dates/plates with wllq=0 at Level 1:
#   experiment.date  plate.id                                wllq_set_to_zero
# 1:        20210408 MW75-8015                            D1,D5,E6,E7,F5,F7,F8
# 2:        20210408 MW75-8016                A7,C2,C4,C5,D6,E5,E6,E7,F6,F7,F8
# 3:        20210408 MW75-8017       C1,C2,C6,D1,D4,D6,D7,E5,E6,E7,E8,F6,F7,F8
# 4:        20210622 MW75-9111                            A1,A2,A5,A7,F1,F2,F3
# 5:        20210622 MW75-9112                      A2,A3,A5,A7,A8,F2,F3,F4,F6
# 6:        20210622 MW75-9113                            A5,E5,E6,E7,F6,F7,F8
# 7:        20210713 MW78-4401                      D6,D7,E5,E6,E7,E8,F6,F7,F8
# 8:        20210713 MW78-4402                      A2,D7,E6,E7,E8,F2,F6,F7,F8
# 9:        20210713 MW78-4406                            D6,D7,E6,E7,F6,F7,F8
# 10:        20210720 MW78-4417             A1,A2,A3,A7,A8,B2,E5,E6,E7,F6,F7,F8
# 11:        20210720 MW78-4419          D5,D6,D7,D8,E5,E6,E7,E8,F1,F5,F6,F7,F8
# 12:        20210720 MW78-6201       A3,D5,D6,D7,D8,E5,E6,E7,E8,F1,F5,F6,F7,F8
# 13:        20210729 MW75-9114 A1,A4,A6,C7,C8,D7,E5,E6,E7,E8,F1,F2,F5,F6,F7,F8
# 14:        20210729 MW75-9115                               A3,A6,E4,E5,F1,F4
# 15:        20210729 MW75-9116                   C2,C3,C6,D3,D6,D7,E5,E7,F1,F7
# 16:        20210826 MW75-9202          C3,C4,C5,C6,C7,D1,D3,D4,D5,D6,E8,F5,F8
# 17:        20210826 MW75-9203             A1,A3,A5,B2,B4,B5,D3,E3,F1,F4,F5,F6
# 18:        20210826 MW75-9204                                  D2,D3,D4,E2,F8
# ---------------------------------------------------------------- 

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
# view all experiment.date's and plate.id's. Are there any NA/missing labels?
rm(dat1)
str(dat1)


# STOPPE DHERE

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title, main.dir = root_output_dir)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
# 
#
# ---------------------------------------------------------------- 
rm(dat2)

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
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
# for example, updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "C6", wllq_note = "Contamination", override_check = override_wllq_checks)

# start a pdf to save the summary graphs
graphics.off()
pdf(file = file.path(main.output.dir, paste0(dataset_title, "_summary_figures_report_",as.character.Date(Sys.Date()),".pdf")), width = 10, height = 8)

# VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS

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


# ASSIGN SPIDS
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


# PREPARE LDH P WELLS (must verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)


# ASSIGN WLLT
dat4 <- assign_wllt(dat4)


# CHECK CONC'S
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


# ASSIGN ACID
cat("\nAssign ACId:\n")
cat("(not doing this for now, since new acnm's need to be registered)\n")
# dat4 <- add_acid(dat4) # holding off, need to register new acid's


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