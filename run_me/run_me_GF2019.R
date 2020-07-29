###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project - Glufosinate/Acute Assay" # starting directory for selecting input files
dataset_title <- "GF2019" # e.g. "name2020"
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- "spidmap_2020-06-22.xlsx"
use_sheet <- "Sheet1" # sheet name in spidmap_file
# optional adjustsment; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/" # where the dataset_title folder will be created
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
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
# Reading from GF2019_neural_stats_files_log_2020-06-21.txt...
# Got 6 files.
# All files are named correctly.
# Empty data.table (0 rows and 2 cols): filenames,run.type.tags
# ----------------------------------------------------------------

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title),
         error = function(e){
           closeAllConnections()
           e } )
# OUTPUT ---------------------------------------------------------
# GF2019_check_summary_2020-06-21.txt is ready.
# ----------------------------------------------------------------

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT ---------------------------------------------------------
# Reading from GF2019_neural_stats_files_log_2020-06-21.txt...
# Got 6 files.
# Reading data from files...
# Processed AC_20190911_MW69-3820_13_00(000)(000).csv
# Processed AC_20190911_MW69-3820_13_01(001)(000).csv
# Processed AC_20190911_MW69-3902_13_00(000)(000).csv
# Processed AC_20190911_MW69-3902_13_01(001)(000).csv
# Processed AC_20190911_MW69-3910_13_00(000)(000).csv
# Processed AC_20190911_MW69-3910_13_01(001)(000).csv
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/GF2019/output/GF2019_dat1_2020-06-21.RData is ready.
# experiment.date  plate.id wllq_set_to_zero
# 1:        20190924 MW69-3910               B1
# ----------------------------------------------------------------

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
# str(dat1)
dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"]
# view all experiment.date's and plate.id's. Are they any NA/missing labels?
unique(dat1$apid)
unique(dat1$plate.id)
rm(dat1)

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Loading...
# GF2019_dat1_2020-06-21.RData ...
# ----------------------------------------------------------------

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT ---------------------------------------------------------
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's
# -100.000   -2.452    3.290    5.719   16.639  697.163      104
# ----------------------------------------------------------------
rm(dat2)

# continuing 6/22/2020

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Reading from GF2019_calculations_files_log_2020-06-21.txt...
# Got 1 files.
# Reading data from files...
# 20190911_Acute Calculations_Glufosinate.xlsx
# AB
# MW69-3820 MW69-3902 MW69-3910
# some values are negative. These will be set to 0
# LDH
# MW69-3820 MW69-3902 MW69-3910
# cytodat is ready
# ----------------------------------------------------------------

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Loading...
# GF2019_dat2_2020-06-21.RData
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/GF2019/output/GF2019_dat3_2020-06-22.RData is ready.
# ----------------------------------------------------------------
rm(cytodat)

# load dat3 and finalize it
cat("\n\nLevel 4 - Finalize well ID information:\n")
dat4 <- get_latest_dat(lvl = "dat3", dataset_title)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]


# FINALIZE WLLQ

# set wllq to zero where rval is NA
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

graphics.off()
pdf(file = file.path(main.output.dir, paste0(dataset_title, "_summary_figures_report_",as.character.Date(Sys.Date()),".pdf")), width = 10, height = 8)

# VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS

cat("\nVerifying control compound labels:\n")
# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = "treatment"]
dat4[grepl("DMSO",treatment), treatment := "Water"] # per conversation with Kathleen 7/24, the control wells are water in this data set.

# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
stripchart(rval ~ treatment, dat4[wllq == 1 & treatment %in% c("DMSO","Water","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"], 
           ylab = "percent change in mean firing rate", vertical = T, pch = 1, method = "jitter")
title(main = paste0("Percent Change in Mean Firing rate\nin Control wells of ",dataset_title," MEA acute Experiments"))
# RESPONSE:
# yes, it appears that the PICRO, TTX were added before the second treatment
dat4[grepl("Lysis",treatment), unique(acnm)] # only LDH. So no LYSIS was added before hand
# definitely looks like the 3 "Media" wells were definitely just Media, no LYSIS


# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to refect this

# for Cell Titer Blue assay
plotdat <- dat4[treatment %in% c("DMSO","Water","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# looks like "Lysis" was added to the Media wells
dat4[treatment == "Media" & grepl("AB",acnm), treatment := "Lysis"]

# view updated stripchart
plotdat <- dat4[treatment %in% c("DMSO","Water","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "Media renamed to Lysis")

# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","Water","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acnm)]
view_activity_stripchart(plotdat)

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
setnames(spidmap, old = "SPID", new = "spid")
setnames(spidmap, old = "Treatment name in Calc files", new = "treatment")
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1] "DMSO"        "TTX"         "PICRO"       "Media"       "Lysis"       "2 * ? Lysis"
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] "1:250 LDH"  "1:2500 LDH" "Lysis"      "Media"      "PICRO"      "TTX"        "Water"      "½ Lysis" 
dat4[grepl("Water",treatment), spid := "Water"]
dat4[treatment == "Media", spid := "Media"]
dat4[treatment == "PICRO", spid := "Picrotoxin"]
dat4[treatment == "TTX", spid := "Tetrodotoxin"]
dat4[grepl("Lysis",treatment), spid := "Tritonx100"]
dat4[grepl("Lysis",treatment), unique(conc), by = "treatment"]
unique(dat4$spid) # confirm no NA spids
if(any(is.na(unique(dat4$spid)))) {
  warning(paste0("The following treatments don't have a corresponding spid:", dat4[is.na(spid), unique(treatment)]))
} else {
  cat("No spids are NA.\n")
}
cat("Number of unique spids:",dat4[,length(unique(spid))],"\n")


# PREPARE LDH P WELLS (must verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)


# ASSIGN WLLT

# make sure all non-tested spids are in 
# "DMSO" "Media"         "Picrotoxin"    "Tetrodotoxin" "Tritonx100" "Bicuculline"
# make sure there are no NA spids
dat4[,unique(spid)]
dat4 <- assign_wllt(dat4)
dat4[spid == "Media" & grepl("(AB)|(LDH)",acnm)]


# CHECK CONC'S (conc's for controls can just make those follow the treatments)
cat("\nFinalize Concentrations:\n")
dat4[, conc_original := conc]
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?

# update conc for control wells
# dmso
dat4[treatment == "Water",unique(conc)]
# [1] "Control"
# Guessing 0.1% DMSO, dont' know for sure
dat4[treatment == "Water", conc := "0.001"]

# picro
dat4[treatment == "PICRO", .N, by = "conc"]
# conc  N
# 1:   25 51
# all 25, no need to change

# ttx
dat4[treatment == "TTX", .N, by = "conc"]
# conc  N
# 1:    1 51
# all 1, no need to change

cat("\nConcentration Corrections:\n")
# any other compounds to update for treated wells?
# confirmed the stkc conc's are all exactly 20 for these compounds
# See samples_stkc_invitrodb_2020-06-22.Rdata
cat("None, confirmed the stkc conc's are all exactly 20 for these compounds\n")

dat4 <- assign_common_conc(dat4, check_conc_correction = FALSE)


# ASSIGN ACID
# cat("\nAssign ACId:\n")
# dat4 <- add_acid(dat4) # need to register the new acnm's, skip this step for now


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