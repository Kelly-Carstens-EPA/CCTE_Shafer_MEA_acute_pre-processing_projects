###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA"
dataset_title <- "TSCA2019" # e.g. "name2020"
select.neural.stats.files <- T # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- T # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- ""
use_sheet <- "" # sheet name in spidmap_file
# optional adjutsment; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/single_point_screen" # where the dataset_title folder will be created
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- TRUE
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(readxl)

# set up folders and working directory
if (!dir.exists(file.path(root_output_dir,dataset_title))) dir.create(file.path(root_output_dir,dataset_title))
setwd(file.path(root_output_dir,dataset_title))
main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(path = "../../mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)

# loading acsn_acnm map
acsn_map <- as.data.table(read.csv(file.path("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl","neural_stats_acsn_to_tcpl_acnm_map.csv")))
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
# Lots of "problems"
# settign guess_run_type_later to TRUE below
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# TSCA2019_check_summary_2020-11-16.txt is ready.
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location, guess_run_type_later = TRUE)
# OUTPUT --------------------------------------------------------- 
# Level 1 - Extract All Data:
#   
#   Reading from TSCA2019_neural_stats_files_log_2020-11-16.txt...
# Got 54 files.
# Reading data from files...
# TSCA2019_dat1_2020-11-16.RData is ready.
# Summary of dates/plates with wllq=0 at Level 1:
#   Empty data.table (0 rows and 3 cols): experiment.date,plate.id,wllq_set_to_zero
# ---------------------------------------------------------------- 

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title, main.dir = root_output_dir)
print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
# view all experiment.date's and plate.id's. Are there any NA/missing labels?
dat1[, .N, by = .(experiment.date, plate.id)]

# assign the run_type now
dat1[, .(length(unique(run_type))), by = .(plate.id, experiment.date)][V1 != 2] # empty, good
setnames(dat1, old = "run_type", new = "file_ext")
dat1[, baseline_extension := sort(file_ext)[1], by = .(plate.id, experiment.date)]
dat1[, unique(baseline_extension), by = .(plate.id, experiment.date)]
dat1[, run_type := ifelse(file_ext == baseline_extension, "baseline","treated")]
dat1[, .(unique(file_ext)), by = .(plate.id, experiment.date, run_type)] # looks good!
dat1[, c("file_ext","baseline_extension") := list(NULL, NULL)]
dat1[, .(length(unique(run_type))), by = .(plate.id, experiment.date)][V1 != 2] # empty!

# SET THE WELL QUALITY - since this didn't happen in previous function since I did not define run_type properly
dat1[run_type == "treated", `:=`(wllq = NA_integer_, wllq_notes = "")] # will be assigned in lvl 2
dat1[run_type == "baseline", `:=`(wllq = 1, wllq_notes = "")] # initialize, will make adjustments below

# if nAE is less than 10 or is NA, set wllq=0
low_ae_qry <- dat1[run_type == "baseline" & acsn == "Number of Active Electrodes" & (activity_value < 10 | is.na(activity_value)), .(run_type, experiment.date, plate.id, well)]
setkey(dat1, run_type, experiment.date, plate.id, well)
dat1[J(low_ae_qry), `:=` (wllq = 0, wllq_notes = "Baseline # of AE < 10; ")]

# if the MFR is very low or near the theoretical upper limit, remove that well
# see the script mfr_baseline_cutoff_investigation.R 
# or the notbeook 'MEA Acute Pre-Process for TCPL', Tab "Development", Page "Mean Firing Rate Baseline Cutoff"
# for more details
mfr_upper_threshold <- 3.4036511 # this is the 95th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
mfr_lower_threshold <- 0.6377603 # this is the 5th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
high_mfr_wells <- dat1[run_type == "baseline" & acsn == "Mean Firing Rate (Hz)" & activity_value > mfr_upper_threshold, .(run_type, experiment.date, plate.id, well)]
dat1[J(high_mfr_wells), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR > ",mfr_upper_threshold," Hz; "))]
low_mfr_wells <- dat1[run_type == "baseline" & acsn == "Mean Firing Rate (Hz)" & (activity_value < mfr_lower_threshold | is.na(activity_value)), .(run_type, experiment.date, plate.id, well)]
dat1[J(low_mfr_wells), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR < ",mfr_lower_threshold," Hz; "))]

# for baseline or treated, if recording length is very short or very  long, remove it
dat1[analysis_duration < 1000, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length < 1000 s; "))]
dat1[analysis_duration > 3800, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length > 3800 s; "))]

dat1[, .N, by = .(wllq, wllq_notes, run_type)] # looks good
setkey(dat1, NULL)
save(dat1, file = file.path(main.output.dir, "output",paste0(dataset_title,"_dat1_",as.character.Date(Sys.Date()),".RData")))
rm(dat1)

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title, main.dir = root_output_dir)
# OUTPUT --------------------------------------------------------- 
# TSCA2019_dat2_2020-11-17.RData is ready.
# ---------------------------------------------------------------- 

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title, main.dir = root_output_dir)
dat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -100.00   -21.51     0.00    15.43    21.53 32870.08     1542 
# ---------------------------------------------------------------- 
rm(dat2)

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# # Load Cytotoxicity Data:
#   
#   Reading from TSCA2019_calculations_files_log_2020-11-17.txt...
# Got 9 files.
# Reading data from files...
# 
# 20191023_Calculations_TSCA dat5_Set 1.xlsx
# AB 
# ...
# 20201021_Calculations_TSCA dat5_Repeats.xlsx
# AB 
# MW71-7101 MW71-7102 MW71-7103 
# LDH 
# MW71-7101 MW71-7102 MW71-7103 
# some values are negative. These will be set to 0
# There are no NA values in cytodat.
# 
# cytodat is ready
# ---------------------------------------------------------------- 

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Level 3 - Combine Cyto and Neural Stats Data; Initialize treatment, conc, and wllq
# 
# Loading...
# TSCA2019_dat2_2020-11-17.RData 
# TSCA2019_dat3_2020-11-17.RData is ready.
# ---------------------------------------------------------------- 
rm(cytodat)

# load dat3 and finalize it
cat("\n\nLevel 4 - Finalize well ID information:\n")
dat4 <- get_latest_dat(lvl = "dat3", dataset_title, main.dir = root_output_dir)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]


# FINALIZE WLLQ
cat("\nFinalize Wllq:")
# set wllq to zero where rval is NA
cat("\nNA rval's:",dat4[wllq==1 & is.na(rval),.N])
# NA rval's: 1542
cat("\nInf rval's (baseline==0):",dat4[wllq==1 & is.infinite(rval),.N])
# Inf rval's (baseline==0): 0

# decide what to do with NA's

# want to know - was the baseline value NA, or just the treated value?
# (I just for NA MFR or AE, but not for the other endpoints)
dat1 <- get_latest_dat(lvl = "dat1", dataset_title, main.dir = root_output_dir)
na_baseline_summary <- dat1[run_type == 'baseline' & is.na(activity_value), .(experiment.date, plate.id, coli, rowi, acnm)]
setkey(dat4, experiment.date, plate.id, coli, rowi, acnm)
dat4[J(na_baseline_summary), .N, by = .(wllq)] # all currently have wllq == 0
dat4[J(na_baseline_summary), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline activity value is NA; "))] # all currently have wllq == 0
rm(dat1)

dat4[is.na(rval), `:=` (wllq_notes = paste0(wllq_notes, "rval is NA; "))]
dat4[is.infinite(rval), `:=` (wllq_notes = paste0(wllq_notes, "rval is Inf; "))]
dat4[(is.na(rval) | is.infinite(rval)) & wllq == 1, .N, by = .(acnm)][order(-N)]

# set NA rval to 0 for these endpoints and where the only wllq_notes is "rval is NA; "
acnm_set_na_to_zero <- paste0('CCTE_Shafer_MEA_acute_',c('firing_rate_mean_weighted',
                                                         'network_burst_duration_mean',
                                                         'per_network_burst_mean_spikes_per_electrode_mean',
                                                         'network_burst_number',
                                                         'burst_percentage_mean',
                                                         'spike_number',
                                                         'firing_rate_mean',
                                                         'burst_number',
                                                         'burst_duration_mean',
                                                         'per_burst_spike_number_mean',
                                                         'burst_percentage_mean',
                                                         'per_network_burst_spike_number_mean',
                                                         'per_network_burst_electrodes_number_mean',
                                                         'network_burst_percentage',
                                                         'active_electrodes_number',
                                                         'bursting_electrodes_number'))
dat4[acnm %in% acnm_set_na_to_zero, length(unique(acnm))] # 15 - ah I missed one. whatever rn
length(unique(acnm_set_na_to_zero)) # [1] 15 ah, I just duplicated one
dat4[acnm %in% acnm_set_na_to_zero & wllq_notes == 'rval is NA; ', .N, by = .(acnm)]
# acnm  N
# 1:                        CCTE_Shafer_MEA_acute_firing_rate_mean_weighted 55
# 2:                      CCTE_Shafer_MEA_acute_network_burst_duration_mean 40
# 3:                         CCTE_Shafer_MEA_acute_network_burst_percentage 40
# 4:         CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean 40
# 5: CCTE_Shafer_MEA_acute_per_network_burst_mean_spikes_per_electrode_mean 40
# 6:              CCTE_Shafer_MEA_acute_per_network_burst_spike_number_mean 40
# 7:                              CCTE_Shafer_MEA_acute_burst_duration_mean 46
# 8:                      CCTE_Shafer_MEA_acute_per_burst_spike_number_mean 46
# 9:                            CCTE_Shafer_MEA_acute_burst_percentage_mean  7

# where teh only wllq note is "rval is NA; " - that means that the activity value is NA only after treatment, not at baseline
# can set these to 0 for specified endpoints. Otherwise, set wllq to 0
dat4[acnm %in% acnm_set_na_to_zero & wllq_notes == "rval is NA; ", `:=` (rval = 0.0,wllq = 1)]
dat4[!(acnm %in% acnm_set_na_to_zero & wllq_notes == "rval is NA; ") & is.na(rval), `:=` (wllq = 0)]
dat4[!(acnm %in% acnm_set_na_to_zero & wllq_notes == "rval is NA; ") & is.infinite(rval), `:=` (wllq = 0)]

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
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# RESPONSE:
# yes/no, it appears that the PICRO, TTX, LYSIS were added before the second treatment
# rename the treatment in the wells as needed

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to refect this

# for Cell Titer Blue assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# make updates if needed
# dat4[, AB.trt.finalized := FALSE] # set this to TRUE for individual plates as you update as needed
# 
# # for every other culture, the "Media" well in F1 contains Lysis at the time of the AB reading (or could change by well F1 vs by the name "Media"...)
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", .(plate.id, experiment.date, rowi, coli, wllq, rval, wllq_notes)] # all are in row 6, col 1
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", `:=`(treatment = "Lysis",conc = 10, AB.trt.finalized = TRUE)]

# # view updated stripchart
# plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
# view_activity_stripchart(plotdat, title_additions = "Media renamed to Lysis")

# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acnm)]
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


# # ASSIGN SPIDS
# cat("\nAssign spid's:\n")
# cat("Using spidmap file:",spidmap_file,"\n")
# spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
# names(spidmap)
# setnames(spidmap, old = "NCCT ID", new = "spid")
# setnames(spidmap, old = "Chemical ID", new = "treatment")
# setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# # [1]   
# dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")
# 
# # assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
# dat4[is.na(spid),unique(treatment)]
# # [1] 
# dat4[grepl("DMSO",treatment), spid := "DMSO"]
# dat4[treatment == "Media", spid := "Media"]
# dat4[treatment == "PICRO", spid := "Picrotoxin"]
# dat4[treatment == "TTX", spid := "Tetrodotoxin"]
# dat4[grepl("Lysis",treatment), spid := "Tritonx100"]
# dat4[grepl("Lysis",treatment), unique(conc), by = "treatment"]
# unique(dat4$spid) # confirm no NA spids
# if(any(is.na(unique(dat4$spid)))) {
#   stop(paste0("The following treatments don't have a corresponding spid:", dat4[is.na(spid), unique(treatment)]))
# } else {
#   cat("No spids are NA.\n")
# }
# cat("Number of unique spids:",dat4[,length(unique(spid))],"\n")


# PREPARE LDH P WELLS (must verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)


# ASSIGN WLLT
# dat4 <- assign_wllt(dat4)


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
# dat4 <- assign_common_conc(dat4)


# ASSIGN ACID
cat("\nAssign ACId:\n")
cat("(not doing this for now, since new acnm's need to be registered)\n")
# dat4 <- add_acid(dat4) # holding off, need to register new acid's


# check that all data is there, nothing is missing, view plots
# data_checks(dat4)

# just looking at head of this fun
# check that all data is there, nothing is missing
cat("\nFinal Checks:\n")
cat("Number of unique acnm's present:",length(unique(dat4$acnm)),"\n")
cat("Wllq breakdown:\n")
print(dat4[, .N, by = "wllq"]) # note if wllq is NA anywhere
dat4[, date_plate := paste(experiment.date, plate.id, sep = "_")]
cat("Number of plates tested:", length(unique(dat4$date_plate)),"\n")
cat("Number of experiment dates:", length(unique(dat4$experiment.date)), "\n")
check.points <- dcast(dat4[, .N, by = c("acnm","date_plate")], date_plate ~ acnm, value.var = "N", fill = 0)
setnames(check.points, old = names(check.points), new = sub("CCTE_Shafer_MEA_acute_","",names(check.points)))

cat("LDH plates are expected to have ")
getMode <- function(x) {
  vals <- unique(x)
  num_instances <- sapply(vals, function(val) sum(x == val))
  vals[which(num_instances == max(num_instances))]
}
LDH_pts <- dat4[grepl("LDH",acnm), .N, by = c("experiment.date","plate.id")][, getMode(N)]
cat(LDH_pts, "points.\n")

cat(paste0("\nThe following plates don't have the expected number of points (48 for MEA & AB ",LDH_pts," for LDH):\n"))
standard_cols <- setdiff(names(check.points), c("date_plate","LDH"))
pts_flag <- FALSE
for (date_plate in unique(check.points$date_plate)) {
  
  if (check.points[date_plate, any(.SD != 48), .SDcols = c(standard_cols)]) {
    pts_flag <- TRUE
    MEA_pts <- check.points[date_plate, .(sort(unique(.SD))), .SDcols = setdiff(standard_cols, "AB")]
    print(check.points[date_plate, .(date_plate, AB, LDH, MEA_pts = paste0(sort(unique(unlist(MEA_pts))),collapse=","))])
  }
  else if (check.points[date_plate, c(LDH)] != LDH_pts) {
    pts_flag <- TRUE
    MEA_pts <- check.points[date_plate, .(sort(unique(.SD))), .SDcols = setdiff(standard_cols, "AB")]
    print(check.points[date_plate, .(date_plate, AB, LDH, MEA_pts = paste0(sort(unique(unlist(MEA_pts))),collapse=","))])
  }
}
if(!pts_flag) {
  cat("(all plates have the expected number of points for each assay component)\n")
}


# closing graphics after last plots
graphics.off()

# create a nice summary of wllq assignments for each well
# createWllqSummary(dat4, dataset_title)
cat("(note that the wllq is not quite final -\nwllq will be updated for outlier DMSO wells will before creating lvl 0 snapshot)\n")

# save dat4
dat4 <- dat4[, .(treatment, experiment.date, plate.id, apid, rowi, coli, conc, acnm, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# you're done!


# LEVEL 5 - make the hit calls
dat5 <- get_latest_dat("dat4",dataset_title, main.dir = root_output_dir)

unique(dat5$wllt)
# [1] NA  "x" "p uh oh
dat5[treatment == "DMSO", wllt := 'n']
unique(dat5$treatment)
dat5[treatment %in% c('Media','PICRO','TTX'), wllt := 'p']
dat5[!(treatment %in% c('DMSO','Media','Lysis','2 * ½ Lysis','PICRO','TTX')), wllt := 't']
dat5[wllt == 'n', .N, by = .(treatment, conc)] # only 1 control type

# find the median of controls and Excel Mad for each acid
dat5[, cntl_med := median(rval[wllt == 'n' & wllq == 1]), by = .(acnm)]
dat5[, cntl_mad := mad(rval[wllt == 'n' & wllq == 1], constant = 1), by = .(acnm)] # note that excel uses diff constant than in R!!

dat5[, lower_bnd := cntl_med - 2*cntl_mad]
dat5[, upper_bnd := cntl_med + 2*cntl_mad]

# make the hit calls
dat5[, hitc_dn := as.numeric(median(rval[wllq == 1]) < unique(lower_bnd)), by = .(acnm, treatment)]
dat5[, hitc_up := as.numeric(median(rval[wllq == 1]) > unique(upper_bnd)), by = .(acnm, treatment)]

dat5[, .(length(unique(treatment))), by = .(hitc_up, hitc_dn)]

dat5[is.na(hitc_dn), .(sum(!is.na(rval) & wllq == 1)), by = .(treatment, acnm)][V1 != 0]
# gotcha, so every na hitc means that there were no usable wells to maek the hit call for that acnm

length(unique(dat5$treatment)) # 354 (including controls)
dat5[hitc_up ==1 | hitc_dn == 1, .(length(unique(treatment)))] # 317! lol!
dat5[hitc_up ==1 | hitc_dn == 1 & !grepl("(AB)|(LDH)",acnm), .(length(unique(treatment)))] # 315

# Let's subset to jsut the 15 acnm we are interested in
old_acnm_list <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/deprecated_acsn_map.csv"))
use_acnms <- sub("NHEERL_","CCTE_Shafer_",old_acnm_list$tcpl_acsn)
dat5[(hitc_up ==1 | hitc_dn == 1) & acnm %in% use_acnms, .(length(unique(treatment)))] # 292...still a ton
dat5[(hitc_up ==1 | hitc_dn == 1) & acnm %in% use_acnms, .(length(unique(treatment))), by = .(acnm)][order(V1)]

# comparing with seline's results
wmfr_hits <- dat5[(hitc_up ==1 | hitc_dn == 1) & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted", c(unique(treatment))]
length(wmfr_hits) #131.. huh, Seline got 171...
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted", unique(.SD), .SDcols = c('cntl_med','cntl_mad','lower_bnd','upper_bnd')]
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & wllq == 1 & wllt == 'n', .N, by = .(apid)]
# apid N
# 1: 20191105 5
# 2: 20191107 6
# 3: 20191119 6
# 4: 20191121 6
# 5: 20191203 8
# 6: 20191205 2
# 7: 20191217 9
# 8: 20191218 8
# 9: 20201103 7
# okay, so I clearly removed many more wells than she did. Due to MFR restrictions?
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & grepl("Baseline MFR",wllq_notes) & wllt == 'n', .N, by = .(apid)]

stripchart(V1 ~ treatment, dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & wllq == 1, median(rval), by = .(treatment)], vertical = T, pch = 1, method = 'jitter')
abline(h = dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted", c(unique(cntl_med))], lty = "dashed")
abline(h = dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted", c(unique(lower_bnd), unique(upper_bnd))])

# if I use Seline's cutoff, do I get teh same result?
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted", `:=`(lower_bnd_test = -41.225,
                                                                     upper_bnd_test = 35.5013)]
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted", `:=`(hitc_up_test = as.numeric(median(rval[wllq == 1]) > unique(upper_bnd_test)),
                                                                     hitc_dn_test = as.numeric(median(rval[wllq == 1]) < unique(lower_bnd_test))), 
                                                                     by = .(acnm, treatment)]
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & (hitc_up_test == 1 | hitc_dn_test == 1), length(unique(treatment))] # 143... still not the same
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & (is.na(hitc_up_test) | is.na(hitc_dn_test)), length(unique(treatment))] # 17

# If I totally remove the MFR baseline wllq requirement, do I get teh same result?
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & (grepl("Baseline MFR",wllq_notes) & !grepl("(rval is NA)|(Baseline activity value is NA)|(Baseline # of AE)",wllq_notes)), .N,by=.(wllq_notes)]
# wllq_notes  N
# 1: Baseline MFR > 3.4036511 Hz;  64
# 2: Baseline MFR < 0.6377603 Hz;  64
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & (grepl("Baseline MFR",wllq_notes) & !grepl("(rval is NA)|(Baseline activity value is NA)|(Baseline # of AE)",wllq_notes)), 
     `:=`(wllq = 1, 
          wllq_notes = paste0("Reset wllq bc only MFR flag; ",wllq_notes))]
dat5[, cntl_med := median(rval[wllt == 'n' & wllq == 1]), by = .(acnm)]
dat5[, cntl_mad := mad(rval[wllt == 'n' & wllq == 1], constant = 1), by = .(acnm)] # note that excel uses diff constant than in R!!
dat5[, lower_bnd := cntl_med - 2*cntl_mad]
dat5[, upper_bnd := cntl_med + 2*cntl_mad]
dat5[, hitc_dn := as.numeric(median(rval[wllq == 1]) < unique(lower_bnd)), by = .(acnm, treatment)]
dat5[, hitc_up := as.numeric(median(rval[wllq == 1]) > unique(upper_bnd)), by = .(acnm, treatment)]
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & (hitc_up == 1 | hitc_dn == 1), length(unique(treatment))] # 135. 4 more than before
dat5[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted" & (is.na(hitc_up_test) | is.na(hitc_dn_test)), length(unique(treatment))] # 17 same.
# okay, so why are my results still different than Seline's? What are we doing differently?


# Check out this "minimal hitting set" (might not be minimal, used a greedy algorithm)
# asid acid                                                           acnm
# 1:   20 2449                             CCTE_Shafer_MEA_acute_burst_number
# 2:   20 2452                 CCTE_Shafer_MEA_acute_interburst_interval_mean
# 3:   20 2454                     CCTE_Shafer_MEA_acute_burst_percentage_std
# 4:   20 2455      CCTE_Shafer_MEA_acute_per_network_burst_spike_number_mean
# 5:   20 2456       CCTE_Shafer_MEA_acute_per_network_burst_spike_number_std
# 6:   20 2457 CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean
# 7:   20 2458                 CCTE_Shafer_MEA_acute_network_burst_percentage
# 8:   20 2459                   CCTE_Shafer_MEA_acute_cross_correlation_area
# 9:   20 2460                   CCTE_Shafer_MEA_acute_cross_correlation_HWHM
mhs_acnms <- c("CCTE_Shafer_MEA_acute_burst_number",
               "CCTE_Shafer_MEA_acute_interburst_interval_mean",
               "CCTE_Shafer_MEA_acute_burst_percentage_std",
               "CCTE_Shafer_MEA_acute_per_network_burst_spike_number_mean",
               "CCTE_Shafer_MEA_acute_per_network_burst_spike_number_std",
               "CCTE_Shafer_MEA_acute_per_network_burst_electrodes_number_mean",
               "CCTE_Shafer_MEA_acute_network_burst_percentage",
               "CCTE_Shafer_MEA_acute_cross_correlation_area",
               "CCTE_Shafer_MEA_acute_cross_correlation_HWHM")


dat5[(hitc_up ==1 | hitc_dn == 1) & acnm %in% mhs_acnms, .(length(unique(treatment)))] # "only" 276! Oh dear, that's a lot!!
dat5[(hitc_up ==1 | hitc_dn == 1) & acnm %in% mhs_acnms, .(length(unique(treatment))), by = .(acnm)][order(V1)]
dat5[(hitc_up ==1 | hitc_dn == 1) & (acnm %in% mhs_acnms | acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted"), .(length(unique(treatment)))] # 276, cool!

# just looking at the top 2 plus wMFR
dat5[(hitc_up ==1 | hitc_dn == 1) & (acnm %in% c("CCTE_Shafer_MEA_acute_per_network_burst_spike_number_mean",
                                                 "CCTE_Shafer_MEA_acute_network_burst_percentage",
                                                 "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted")), .(length(unique(treatment)))] # 192
dat5[(hitc_up ==1 | hitc_dn == 1) & (acnm %in% c("CCTE_Shafer_MEA_acute_per_network_burst_spike_number_mean",
                                                 "CCTE_Shafer_MEA_acute_network_burst_percentage",
                                                 "CCTE_Shafer_MEA_acute_cross_correlation_HWHM",
                                                 "CCTE_Shafer_MEA_acute_firing_rate_mean_weighted")), .(length(unique(treatment)))] # 233

# will consider alternatives for MHS
