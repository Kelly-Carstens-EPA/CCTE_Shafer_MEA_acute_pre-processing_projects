###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA"
dataset_title <- "DNT2019"
spidmap_file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
use_sheet <- "MEA Acute Conc Res"
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
# optional adjustments; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/" # where the dataset_title folder will be created
override_wllq_checks <- TRUE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- FALSE
###################################################################################
# END USER INPUT
###################################################################################

# load packages
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
# Reading from DNT2019_neural_stats_files_log_2020-05-18.txt...
# Got 78 files.
# All files are named correctly.
# Empty data.table (0 rows and 2 cols): filenames,run.type.tags
# ----------------------------------------------------------------

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title),
         error = function(e){
           closeAllConnections()
           e } )
# OUTPUT ---------------------------------------------------------
# DNT2019_check_summary_2020-06-18.txt is ready.
# ----------------------------------------------------------------

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location)
# OUTPUT ---------------------------------------------------------
# ----------------------------------------------------------------

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
# str(dat1)
# names(dat1)
dat1[grepl("firing",acsn) & is.na(activity_value), .(unique(wllq), unique(wllq_notes)), by = "run_type"]
# run_type V1                                                                        V2
# 1: baseline  0 # of AE less than 10 in baseline recording; Baseline MFR < 0.3310055 Hz;
# 2:  treated NA
# good, the is.na() check is working
# dat1[wllq == 0, .(wllq_set_to_zero = paste0(sort(unique(well)),collapse=",")), by = c("plate.id","experiment.date")][order(experiment.date, plate.id)]
print(dat1[, .N/length(unique(dat1$acsn)), by = "wllq_notes"])
rm(dat1)


# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Loading...
# DNT2019_dat1_2020-06-18.RData
#
# Collapsing treated and baseline data...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_dat2_2020-06-18.RData is ready.
# ----------------------------------------------------------------

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT ---------------------------------------------------------
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's
# -100.000  -33.571   -4.183   -9.387    6.367 1999.582     1470
# ----------------------------------------------------------------
rm(dat2)

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Reading from DNT2019_calculations_files_log_2020-06-04.txt...
# Got 13 files.
# Reading data from files...
#
# 20190515_Calculations_DNT Group_1-DONE.xlsxNew names:
#   * `` -> ...1
# * `` -> ...2
# * `` -> ...3
# ----------------------------------------------------------------

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT ---------------------------------------------------------
# Loading...
# DNT2019_dat2_2020-06-18.RData
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_dat3_2020-06-18.RData is ready.
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
# The following wells were contaminated, based on the lab notebook
updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "C6", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "F1", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190530", plate = "MW68-0809", well = "D7", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190530", plate = "MW68-0809", well = "E1", wllq_note = "Contamination", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190530", plate = "MW68-0810", well = "B5", wllq_note = "Contamination", override_check = override_wllq_checks)

# the following wells did not receive the full dose, based on the lab notebook
updateWllq(dat4, date = "20190730", plate = "MW68-0817", well = "E5", wllq_note = "Did not get full dose", override_check = override_wllq_checks)
updateWllq(dat4, date = "20190730", plate = "MW68-0817", well = "F5", wllq_note = "Did not get full dose", override_check = override_wllq_checks)

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
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# RESPONSE:
# yes, it appears that the PICRO, TTX, LYSIS were added before the second treatment

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to refect this

# for Cell Titer Blue assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH","TTX/Lysis","Media/Lysis","PICRO/Lysis") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")

# Most F1 wells say Media, but should be Lysis for AB
dat4[grepl("AB",acnm) & rowi == 6 & coli == 1 & !(treatment %in% c("Media","Lysis")), .(srcf, plate.id, treatment)]
# srcf  plate.id treatment
# 1:         20190515_Calculations_DNT Group_2.xlsx MW68-0809     PICRO - see note on this plate below. Control wells shifted, this well not lysed
# 2: 20190717_Calculations_DNT Group_5 Repeata.xlsx MW68-0817     PICRO - Lab notebook and calc file confirms this, see below. This well still used for lysis
# 3: 20190717_Calculations_DNT Group_5 Repeata.xlsx MW68-0818       TTX - Lab notebook and calc file confirms this, see below. This well still used for lysis
# 4: 20190814_Calculations_DNT Group_1 Repeat_update20200722.xlsx MW69-3808     PICRO
# 5: 20190814_Calculations_DNT Group_1 Repeat_update20200722.xlsx MW69-3809       TTX

# In 20190717_Calculations_DNT Group_5 Repeata.xlsx , exp date 20190730. Note in this file:  **Controls are rotating but the same F1 well is lysed across 3 plates.

# In these last 2 cases, does it look like lysis was added to these wells, or to the Media only wells?
dat4[plate.id == "MW69-3808" & coli == 1 & grepl("AB",acnm), .(conc, treatment, rval, wllq, rowi, coli)]
# conc treatment       rval wllq rowi coli
# 1: Control      DMSO 22018.6667    1    1    1
# 2: Control      DMSO 19979.6667    1    2    1
# 3: Control      DMSO 18909.6667    1    3    1
# 4:      10     Media 22566.6667    1    4    1
# 5:       1       TTX 21734.6667    1    5    1
# 6:      25     PICRO   171.6667    1    6    1
# def looks like well F1 was lysed!
dat4[plate.id == "MW69-3809" & coli == 1 & grepl("AB",acnm), .(conc, treatment, rval, wllq, rowi, coli)]
# conc treatment     rval wllq rowi coli
# 1: Control      DMSO 23402.33    1    1    1
# 2: Control      DMSO 24479.33    1    2    1
# 3: Control      DMSO 23299.33    1    3    1
# 4:      25     PICRO 25474.33    1    4    1
# 5:      10     Media 24561.33    1    5    1
# 6:       1       TTX     0.00    1    6    1
# again, F6 was definitely lysed, not the Media well! Don't have to do anything special here

# from the lab notebook, G2 extras experiment date 20190530 "TTX wells lysed post recording to be used as Total LDH" (instead of Media wells in F1, since these contaminated)
dat4[grepl("20190530", apid) & grepl("AB",acnm) & treatment == "TTX", .(plate.id, rowi, coli, wllq, wllq_notes, rval, treatment)]
# plate.id rowi coli wllq wllq_notes  rval treatment
# 1: MW68-0807    4    1    1                0       TTX
# 2: MW68-0809    4    1    1                0       TTX
# 3: MW68-0810    4    1    1            20066       TTX
# This makes sense - 
# where rval=0, well was probably lysed. 
# In only the first 2 plates, the media well was contaminated.
# So we only the first 2 plates use a well other than F1 for their lysis well
dat4[grepl("AB",acnm) & experiment.date == "20190530" & plate.id %in% c("MW68-0807","MW68-0809") & rowi == 4 & coli == 1, treatment := paste0(treatment, "/Lysis")]

# for every F1 well, change the treatment to Lysis (or TTX/Lysis)
# Except to the F1 wells in "MW68-0807","MW68-0809", where D1 was lysed instead
dat4[grepl("AB",acnm) & rowi == 6 &coli == 1 & !(experiment.date == "20190530" & plate.id %in% c("MW68-0807","MW68-0809")),
     treatment := ifelse(treatment %in% c("Media","Lysis"), "Lysis", paste0(treatment,"/Lysis"))]

# view updated stripchart
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH","TTX/Lysis","Media/Lysis","PICRO/Lysis") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "Lysis wells updates")
# looks great! Only wells containing Lysis or wllq==0 are near 0


# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH","TTX/Lysis","Media/Lysis","PICRO/Lysis") & grepl("(LDH)",acnm)]
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
# other note: where treatment == 0, these are Media wells
cat("Where treatment == 0, wells contained only Media\n")
print(dat4[treatment == "0", .N, by = "apid"])
# apid   N
# 1: 20191008 357
dat4[treatment == "0", `:=`(treatment = "Media")]

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
spidmap$treatment <- as.character(spidmap$treatment) # since these treatments are labelled numerically, were read as integers
# to transform the NCCT ID into the EPA Sample ID, need to add "EPAPLT0". This is how the SPIDs are created in the Calculations files, Dosing Plate tab
spidmap[, spid := paste0("EPAPLT0",spid)]
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# "DMSO"        "TTX"         "PICRO"       "Media"       "Lysis"       "TTX/Lysis"   "PICRO/Lysis" "2 * ½ Lysis"
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] "2 * ½ Lysis" "DMSO"        "Lysis"       "Media"       "PICRO"       "PICRO/Lysis" "TTX"         "TTX/Lysis"  
dat4[grepl("DMSO",treatment), spid := "DMSO"]
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


# PREPARE P WELLS FROM LDH
# (must be done after finalize wllq, treatment in ea well)
dat4 <- prepare_LDH_p_wells(dat4)


# ASSIGN WLLT
dat4 <- assign_wllt(dat4)


# CHECK CONC'S (conc's for controls can just make those follow the treatments)
cat("\nFinalize Concentrations:\n")
dat4[, conc_original := conc]
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done in the calculatiosn files already?

# update conc for control wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] "Control"
dat4[treatment == "DMSO", conc := "0.001"]

# picro
dat4[treatment == "PICRO", .N, by = "conc"]
# conc    N
# 1:   25 1663
# 2:   10   90
# based on lab notebook, this should always be 25 (there can be mix ups in calc file)
dat4[treatment == "PICRO", conc := "25"]

# ttx
dat4[treatment == "TTX", .N, by = "conc"]
# conc    N
# 1:    1 1751
# based on lab notebook, this should always be 1
dat4[treatment == "TTX", conc := "1"]

# final updates, view, and check
cat("Concentration Corrections:\n")
cat("The following treatment have char conc. Will be set to NA:\n")
print(suppressWarnings(dat4[is.na(as.numeric(conc)), .N, by = c("spid","treatment","conc")]))
dat4[, conc := suppressWarnings(as.numeric(conc))]
# from first time I ran assign_common_conc
# The following concentrations for the following compounds might need to be corrected:
#   spid   treatment                                                  source_concs stock_conc                    spidmap_guess_concs
#   2: EPAPLT0167D05          18 0.03,0.0315,0.1,0.105,0.3,0.315,1,1.05,3,3.15,10,10.5,30,31.5    21.0365 0.0316,0.105,0.316,1.05,3.16,10.5,31.6
# stripchart(rval ~ conc, dat4[spid == "EPAPLT0167D05" & grepl("firing_rate_mean$",acnm)], vertical = T, method = "jitter", main = "MFR rval vs unchanged conc for EPAPLT0167D05")

# first, I'm going to "de-correct" the concs from the culture date that were supposedly correctd already
dat4[spid == "EPAPLT0167D05", conc := signif(conc, digits = 1)]
# now, conc-correct everything
dat4[spid == "EPAPLT0167D05", conc := signif((spidmap[spid == "EPAPLT0167D05", c(Conc)]/20.0)*conc, 3)]
# dr plot to confirm
# stripchart(rval ~ conc, dat4[spid == "EPAPLT0167D05" & grepl("firing_rate_mean$",acnm)], vertical = T, method = "jitter", main = "MFR rval vs updated conc for EPAPLT0167D05")

dat4 <- assign_common_conc(dat4)


# ASSIGN ACID
cat("\nAssign ACId:\n")
cat("(not doing this for now, since new acnm's need to be registered)\n")
# dat4 <- add_acid(dat4) # holding off, need to register new acid's


# check that all data is there, nothing is missing
data_checks(dat4)

# closing graphics after last plots
graphics.off()

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)
cat("(note that the wllq is not quite final -\nwllq will be updated for outlier DMSO wells will before creating lvl 0 snapshot)\n")

# save the dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acnm, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# done!
