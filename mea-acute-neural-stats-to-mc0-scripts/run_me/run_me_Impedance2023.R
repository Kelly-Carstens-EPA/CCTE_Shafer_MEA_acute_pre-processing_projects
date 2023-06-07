rm(list = ls())
# ----------------------------------------------------------------------- #
# USER INPUT
# ----------------------------------------------------------------------- #
start.dir <- "L:/Lab/NHEERL_MEA"
dataset_title <- "Impedance2023" # e.g. "Impedance2023"
select.neural.stats.files <- T # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- T # select new calculations files, or use the files in the most recent calculations_files_log?
select.impedance.files <- T # whatever is in the previous line
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
#spidmap_file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
#use_sheet <- "MEA Acute Conc Res" # sheet name in spidmap_file
# optional adjutsment; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl" # where the dataset_title folder will be created
script.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts"
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- TRUE
standard_analysis_duration_requirement <- TRUE # default should be true
# ----------------------------------------------------------------------- #
# END USER INPUT
# ----------------------------------------------------------------------- #

library(data.table)
library(openxlsx)
library(stringi)

# set up folders and working directory
if (!dir.exists(file.path(root_output_dir,dataset_title))) dir.create(file.path(root_output_dir,dataset_title))
setwd(file.path(root_output_dir,dataset_title))
main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(script.dir, pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)

# loading acsn_acnm map
acsn_map <- as.data.table(read.xlsx(file.path(root_output_dir,"neural_stats_acsn_to_tcpl_acnm_map.xlsx")))
acsn_map <- acsn_map[, .(acsn, acnm)]
#acsn_map[nrow(acsn_map)+1, ] <- c(acsn = "IMP", acnm = "CCTE_Shafer_MEA_acute_IMP") #added in endpoints in the excel sheet itself

# Level 0 - Gather and Check Files ----------------------------------------

cat(paste0(dataset_title, " MEA Acute TCPL Level 0 Data Prep Running Log\nDate: ",as.character.Date(Sys.Date()),"\n"))
cat("\nLevel 0 - Gather and Check Files:\n")

# Scan for readme's that might affect dosing, wllq
# skipping this step, no readmes
#txt.files <- list.files(path = start.dir, pattern = '\\.txt', recursive = T, full.names = T)
#readmes <- txt.files[grepl('read( )*me',tolower(txt.files))]
#for (readme in readmes) {
 # cat(dirname(readme),'\n')
 # cat(scan(readme, what = character(), sep = '\n', quiet = T), sep = '\n')
 #  cat('\n')
 #}

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}
if (select.impedance.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "impedance")
}

# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(run.type.tag.location, main.output.dir, dataset_title)
# OUTPUT -------------------------------------------------------- #
# Reading from _neural_stats_files_log_2023-05-16.txt...
#Got 10 files.
#All files are named correctly.
# --------------------------------------------------------------- #

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT -------------------------------------------------------- #
# _check_summary_2023-05-16.txt is ready.
# IM_20210922_MW501729A_13_01_(Treated)(000).csv analysis duration is 1796.25s, make sure to attach a note to it
# --------------------------------------------------------------- #


# Level 1 - Extract data from files and melt ------------------------------

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT -------------------------------------------------------- #
# Impedance2023_dat1_2023-05-19.RData is ready.
#Summary of dates/plates with wllq=0 at Level 1:
#  experiment.date  plate.id              wllq_set_to_zero
#1 :        20211012 MW501730A A4,A8,B8,C7,C8,D7,D8,E8,F7,F8
# --------------------------------------------------------------- #

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
# view all experiment.date's and plate.id's. Are there any NA/missing labels?
# OUTPUT -----------------------------------------------------------#
#                wllq_notes  V1
# 1:                         470
# 2: Baseline MFR < 0.5 Hz;   10
# ------------------------------------------------------------------ #
#thinking of removing all the impedance rows (will add on all timepoints later on)
dat1  <- dat1[acsn != "Resistance - Avg (kΩ)"]
dat1  <- dat1[acsn != "Resistance - Std (kΩ)"]
dat1  <- dat1[acsn != "Number of Covered Electrodes"]
dat1  <- dat1[acsn != "Weighted Mean Resistance (kΩ)"]
#adding wllq_note to plate MW501729A treated for being too short in recording time
dat1[srcf == "IM_20210922_MW501729A_13_01_(Treated)(000).csv", ]$wllq_notes <- "Analysis duration is 1796.25s, which is shorter than baseline"
save(dat1, file = paste0(main.output.dir, "/output/",dataset_title,"_dat1_",as.character.Date(Sys.Date()),".RData"))

dat1[, .N, by = .(experiment.date, plate.id)]
#experiment.date  plate.id    N
#1:        20210916 MW502627A 4224
#2:        20211005 MW501729A 4224
#3:        20211012 MW501730A 4224
#4:        20211019 MW501824A 4224
#5:        20211018     MWB76 4224

dat1[, .N, by = .(srcf, run_type)]
#                                                     srcf run_type    N
#1:     IM_20210901_MW502627A_15_00_(000)(001)_Baseline.csv baseline 2112
#2:      IM_20210901_MW502627A_15_01_(003)(001)_Treated.csv  treated 2112
#3:         IM_20210922_MW501729A_13_00_(Baseline)(000).csv baseline 2112
#4:          IM_20210922_MW501729A_13_01_(Treated)(000).csv  treated 2112
#5:      IM_20210929_MW501730A_13_00_Baseline(000)(000).csv baseline 2112
#6: IM_20210929_MW501730A_13_01_Treated_20min(000)(000).csv  treated 2112
#7:      IM_20211006_MW501824A_13_00_Baseline(000)(000).csv baseline 2112
#8: IM_20211006_MW501824A_13_01_Treated_20min(000)(000).csv  treated 2112
#9:          IM_20211006_MWB76_12_00_Baseline(000)(000).csv baseline 2112
#10:     IM_20211006_MWB76_12_01_Treated_20min(000)(000).csv  treated 2112
 
rm(dat1)


# Level 2 - Collapse treated and baseline recordings ----------------------

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title, main.dir = root_output_dir)
# OUTPUT -------------------------------------------------------- #
# 20210916_MW502627A
#Baseline stats file name: IM_20210901_MW502627A_15_00_(000)(001)_Baseline.csv
#Treated stats file name: IM_20210901_MW502627A_15_01_(003)(001)_Treated.csv

#20211005_MW501729A
#Baseline stats file name: IM_20210922_MW501729A_13_00_(Baseline)(000).csv
#Treated stats file name: IM_20210922_MW501729A_13_01_(Treated)(000).csv

#20211012_MW501730A
#Baseline stats file name: IM_20210929_MW501730A_13_00_Baseline(000)(000).csv
#Treated stats file name: IM_20210929_MW501730A_13_01_Treated_20min(000)(000).csv

#20211019_MW501824A
#Baseline stats file name: IM_20211006_MW501824A_13_00_Baseline(000)(000).csv
#Treated stats file name: IM_20211006_MW501824A_13_01_Treated_20min(000)(000).csv

#20211018_MWB76
#Baseline stats file name: IM_20211006_MWB76_12_00_Baseline(000)(000).csv
#Treated stats file name: IM_20211006_MWB76_12_01_Treated_20min(000)(000).csv

# --------------------------------------------------------------- #

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT -------------------------------------------------------- #
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-100.09  -55.69  -10.80     Inf   15.60     Inf    1959 
## --------------------------------------------------------------- #
rm(dat2)


# Level 3 - Get cytotoxicity data and merge -------------------------------

# get cytotox data
# calculations files are modified to have only data for one plate
cytodat <- getAllCytoData(main.output.dir, dataset_title, min.tag.phrase.count = 1)
#get error Error in valuesUnderTagPhrase(assay_dat_dt, tagPhrase = "Chemical", "treatment", : TagPhrase 'Chemical' not found.
#it was looking for TagPhrase for 3 times, while I have 1 plate on some calculation files
# OUTPUT -------------------------------------------------------- #
# Load Cytotoxicity Data:
#
#Reading from Impedance2023_calculations_files_log_2023-05-16.txt...
#Got 4 files.
#Reading data from files...
#IM_20210901_MW502627A_Calculations.xlsx
#AB 
#MW502627B 
#LDH 
#MW502627B 
#some values are negative. These will be set to 0
#IM_20210922_MW501729A_Calculations.xlsx
#AB 
#MW501729A 
#LDH 
#MW501729A 
#some values are negative. These will be set to 0
#IM_20210929_MW501730A_Calculations.xlsx
#AB 
#MW501730A 
#LDH 
#MW501730A
#IM_20211006_Impedance_Calculations.xlsx
#AB 
#MW501824A MW89-4608 MWB76 
#LDH 
#MW501824A MW89-4608 MWB76 
#some values are negative. These will be set to 0
#There are no NA values in cytodat.
#cytodat is ready
# --------------------------------------------------------------- #

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
# Plate MWB76 was dosed on 20211018, but MW501824A and MW89-4608 were dosed on 20211019
#need to change the experiment date for that two plates manually (both apid and experiment.date)
cytodat[plate.id == "MW501824A", ]$apid <- "20211019"
cytodat[plate.id == "MW501824A", ]$experiment.date <- "20211019"
cytodat[plate.id == "MW89-4608", ]$apid <- "20211019"
cytodat[plate.id == "MW89-4608", ]$experiment.date <- "20211019"

#add in impedance data then remove blank from it, add in different timepoint
# each timppoint should be one row, just like what we have in NFA, also calculate the AUC value for impedance
# append impedance dat before update well quality
# rval for impedance can be just the raw value with blank removed, auc cal function should be in the NFA script
imp_files <- list.files(path = "L:/Lab/NHEERL_MEA/Project - Impedance/Impedance Paper/prep for tcpl pipeline/Acute/IMP")
setwd("L:/Lab/NHEERL_MEA/Project - Impedance/Impedance Paper/prep for tcpl pipeline/Acute/IMP")
#will have a for loop to get all impedance data plate by plate and if for baseline or treated & timepoint
imp_dat <- data.frame()
for (i in 1:length(imp_files)) {
  file_scan <- scan(file = imp_files[i], what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  # get the index of the tag phrase 'Well Averages'
  well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
  next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]) | file_col1[well.averages.rowi:length(file_col1)] == "")[1]
  imp <- as.data.table(read.table(imp_files[i], sep = ",", header = F, skip = (well.averages.rowi - 1), nrows = (next.blank.row.dist-1),
                                  stringsAsFactors = F))
  imp <- imp[c(1, nrow(imp)), 1:49] ##trim down in between, just need last and first row, no header 
  #need to transpose into column/row, then change well name into rowi# coli#
  imp <- transpose(imp)
  imp[1,1] <- "well"#change "Well Averages" into "well"
  imp[1,2] <- "wt.mean.resistance"
  imp <- janitor::row_to_names(imp, 1)
  #remove blank
  imp$blank <- "12.107"
  imp[ , 2] <- apply(imp[ , 2,drop=F], 2,function(x) as.numeric(as.character(x)))
  imp[ , 3] <- apply(imp[ , 3,drop=F], 2,function(x) as.numeric(as.character(x)))
  imp[, rval := imp$wt.mean.resistance - imp$blank]
  imp[, rowi := match(stri_extract_all(well, regex = '[A-Z]'), LETTERS)]
  imp[, coli := as.numeric(stri_extract_all(well, regex = '[0-9]'))]
  imp[, blank:= NULL]
  imp[, wt.mean.resistance := NULL]
  imp[, well := NULL]
  #assigning plate.id to match with treatment later
  imp[, acnm := acsn_map$acnm[53]]
  imp[, files_log := "Impedance2023_impedance_files_log_2023-05-22"]
  imp[, srcf := imp_files[i]]
  imp[, plate.id := strsplit(basename(imp_files[i]), split = "_")[[1]][3]]
  imp[, wllq := "1"]
  imp[, wllq_notes := ""]
  #add a column noting it is the baseline or treated
  run.type.tag <- strsplit(basename(imp_files[i]), split = "_")[[1]][5] #returns baseline or treated
  if (run.type.tag == "01") {
    imp[, run_type := "treated"]
    imp[, time.post.treatment := strsplit(imp_files[i], split = "_")[[1]][6]]
  } else {
    imp[, run_type := "baseline"]
    imp$time.post.treatment <- NA
  }
  imp_dat <- rbind(imp_dat, imp)
}
rm(imp)
#merge with cytodat to get treatment, conc, and experiment date info
#get collapseconc function from combineNeuralAndCyto script
well_id_dat <- cytodat[rowi %in% c(1:6) & coli %in% c(1:8), .(treatment = unique(treatment), conc = as.character(collapseConc(conc))), by = c("plate.id","rowi","coli", "experiment.date")]
imp_dat <- merge(imp_dat, well_id_dat, by = c("plate.id","rowi","coli"))
#remove picrotoxin data and misdosed wells
#remove 10min data and change 3h to 2h
imp_dat[treatment == "Picrotoxin", ] <- NULL
imp_dat <- subset(imp_dat, treatment!= "Picrotoxin")
rm_list <- which(imp_dat$treatment == "Tween 20"& imp_dat$conc == "0.02")
rm_list <- append(rm_list, which(imp_dat$treatment == "Tween 20"& imp_dat$conc == "6.0000000000000001E-3")) #  , "6.0000000000000001E-3", "0.06", "0.2", "0.6","2", "6")))
rm_list <- append(rm_list, which(imp_dat$treatment == "Tween 20"& imp_dat$conc == "0.06"))
rm_list <- append(rm_list, which(imp_dat$treatment == "Tween 20"& imp_dat$conc == "0.2"))
rm_list <- append(rm_list, which(imp_dat$treatment == "Tween 20"& imp_dat$conc == "0.6"))
rm_list <- append(rm_list, which(imp_dat$treatment == "Tween 20"& imp_dat$conc == "2"))
rm_list <- append(rm_list, which(imp_dat$treatment == "Tween 20"& imp_dat$conc == "6"))
rm_list <- append(rm_list, which(imp_dat$time.post.treatment == "10min"))
imp_dat <- imp_dat[-c(rm_list), ]
imp_dat[time.post.treatment == "3h"]$time.post.treatment <- "2h"
#will change 20 min into hour for easier calculation
imp_dat[time.post.treatment == "20min"]$time.post.treatment <- "1/3h"
#append imp@72h to cytodat, that will be included in dat3
imp72h <- imp_dat[time.post.treatment == "72h"]
imp72h$acnm <- "CCTE_Shafer_MEA_acute_resistance_mean_weighted_72h"
imp72h$run_type <- NULL
imp72h$time.post.treatment <- NULL
imp72h$wllq <- NULL
imp72h$wllq_notes <- NULL
imp72h$apid <- imp72h$experiment.date
cytodat <- rbind(cytodat, imp72h)
#export another set of imp_dat on it's own with all baseline&treated imp data, remove picrotoxin data and misdosed wells
save(imp_dat, file = paste0(main.output.dir,"/output/",dataset_title,"_imp_dat_",as.character.Date(Sys.Date()),".RData"))
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
#now dat3 has all neural, LDH, CTB, IMP@72h dat, all cyto are endpoint (all endpoint cyto can be compared)
#imp_dat all wllq=0 are removed prior to append to cytodat and combine with neural dat
#checked for negative rval. NONE (thought some rval might be negative because of blank removal)
# OUTPUT -------------------------------------------------------- #
# Level 3 - Combine Cyto and Neural Stats Data; Initialize treatment, conc, and wllq
#Loading...
#Impedance2023_dat2_2023-05-19.RData 
#Impedance2023_dat3_2023-05-19.RData is ready.
#Warning message:
#  In combineNeuralAndCyto(cytodat, main.output.dir, dataset_title) :
#  The following date_plate's are only found in cytodat (and not in dat2): 20211019_MW89-4608
#Wllq will be set to 1 for all wells on these LDH/AB plates.
# --------------------------------------------------------------- #
# neural_stats files were not in the list, did not get processed. that is because plate MW89-4608 has low overall activity, data from whole plate will be discarded. ignore warning
rm(cytodat)
rm(well_id_dat)
rm(imp72h)

#calculating AUC for impedance dat just like what we did for NFA?
#9 timpoints after treatment : 0h, 1/3h, 1h, 2h, 4h, 6h, 24h, 48h, 72h
#remove all baseline rows
imp_dat <- subset(imp_dat, run_type == "treated")
#check if each replicate has all 9 timepoints 
imp_dat <- unique(imp_dat) # eliminate duplicate data
#remove the "h" from time.post.treatment, doing it the dummy way
imp_dat[time.post.treatment == '0h', ]$time.post.treatment <- 0
imp_dat[time.post.treatment == '1/3h', ]$time.post.treatment <- as.numeric(1/3)
imp_dat[time.post.treatment == '1h', ]$time.post.treatment <- 1
imp_dat[time.post.treatment == '2h', ]$time.post.treatment <- 2
imp_dat[time.post.treatment == '4h', ]$time.post.treatment <- 4
imp_dat[time.post.treatment == '6h', ]$time.post.treatment <- 6
imp_dat[time.post.treatment == '24h', ]$time.post.treatment <- 24
imp_dat[time.post.treatment == '48h', ]$time.post.treatment <- 48
imp_dat[time.post.treatment == '72h', ]$time.post.treatment <- 72
imp_dat <- transform(imp_dat, time.post.treatment = as.numeric(imp_dat$time.post.treatment))
#imp_dat$time.post.treatment.h <- imp_dat$time.post.treatment
#imp_dat$time.post.treatment <- NULL
#save(imp_dat, file = paste0(main.output.dir,"/output/",dataset_title,"_imp_dat_",as.character.Date(Sys.Date()),".RData"))
## Split data frame by individual wells over time (interaction term speeds this up greatly for larger datasets)
# all_data_split <- split(all_data, by = c("date","Plate.SN","well","trt","dose"), drop = TRUE, sorted = TRUE) # want to verify that this is identical in the future
imp_dat <- as.data.frame(imp_dat)
imp_dat_split <- split(imp_dat, interaction(imp_dat$plate.id, imp_dat$rowi, imp_dat$coli, imp_dat$treatment, imp_dat$conc, drop=TRUE)) # Split data into bins of single well across
rm_list <- which(unlist(lapply(imp_dat_split, nrow)) != 9)
imp_dat_split <- imp_dat_split[-rm_list]
if(sum(unlist(lapply(imp_dat_split, nrow)) != 9) > 0) {
  stop("Some chunks in imp_dat_split do not have exactly 4 data rows (1 from each DIV)\n(Remove or modify this error if not testing exactly 4 DIV)")
}
#run AUC function
require(pracma) #pracma package has trapz function that computes AUC based on trapezoidal geometry (no curve fitting)

endpoint_cols <- "rval"

out <- lapply(1:length(imp_dat_split), function(i) {
  
  imp_dat_split[[i]] <- imp_dat_split[[i]][order(imp_dat_split[[i]][,"time.post.treatment"]),]  # Make sure order of rows follows DIV time
  #print(imp_dat_split[[i]][, "time.post.treatment"]) it is correct order, no idea why it is not showing up in the imp_dat_split
  
  experiment.date <- imp_dat_split[[i]]$experiment.date[1]
  plate.id <- imp_dat_split[[i]]$plate.id[1]
  rowi <- imp_dat_split[[i]]$rowi[1]
  coli <- imp_dat_split[[i]]$coli[1]
  treatment <- imp_dat_split[[i]]$treatment[1]
  conc <- imp_dat_split[[i]]$conc[1]
  acnm <- imp_dat_split[[i]]$acnm[1]
  srcf <- imp_dat_split[[i]]$srcf[1]
  
  
  for (j in endpoint_cols) {
    param_name <- paste(j, "_auc", sep="") # create auc variable name
    #assign(param_name, round(trapz(append(imp_dat_split[[i]][,"DIV"], 2, after=0), append(imp_dat_split[[i]][,j], 0, after=0)),6), inherits=TRUE) # calculate auc, assign to variable name
    assign(param_name, round(trapz(imp_dat_split[[i]][,"time.post.treatment"], imp_dat_split[[i]][,j]),9), inherits=TRUE) # calculate auc, assign to variable name
  }
  
  # put vector of AUC values together
  c("experiment.date" = experiment.date, "plate.id" = as.character(plate.id), "rowi" = as.character(rowi), "coli" = as.character(coli), "treatment" = as.character(treatment), "conc" = conc, "acnm" = as.character(acnm), "srcf" = as.character(srcf), sapply(paste(endpoint_cols,"auc",sep = "_"), get, USE.NAMES = T))
})

##FIX!!! - (amy): not sure what needs to be fixed here
sum_table <- as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE) # Re-form data frame
# setnames(sum_table, old = paste0(assay_component_map$create_burst_ont_Data_endpoint,"_auc"), new = assay_component_map$tcpl_acsn)
sum_table$rval_auc <- as.numeric(sum_table$rval_auc)
# sum_table[,paste0(endpoint_cols,"_auc")] <- lapply(sum_table[,paste0(endpoint_cols,"_auc")], as.numeric)
#sum_table[sum_table[,"dose"]==sprintf("%.5f",0),"treatment"] <- ControlTreatmentName # control treatment name will be updated in tcpl_MEA_dev_AUC
sum_table$wllq_by_well <-"1"
sum_table$wllq_notes_by_well <- ""
save(sum_table, file = paste0(main.output.dir,"/output/",dataset_title,"_imp_AUC_",as.character.Date(Sys.Date()),".RData"))

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
# NA rval's: 1959
cat("\nInf rval's (baseline==0):",dat4[wllq==1 & is.infinite(rval),.N])
# Inf rval's (baseline==0): 2
dat4[is.na(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is NA; "))]
dat4[is.infinite(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is Inf; "))]
cat("\nWell quality set to 0 for these rval's.\n")


# Read in well quality table and merge with dat4
wllq.tb <- as.data.table(read.csv('wells_with_well_quality_zero_acute.csv'))
wllq.tb[, rowi := match(stri_extract_all(well, regex = '[A-Z]'), LETTERS)]
wllq.tb[, coli := as.numeric(stri_extract_all(well, regex = '[0-9]'))]
wllq.tb[, .N, by = .(well, rowi, coli)] # confirm this looks okay
dat4[, endpoint_type := 'mea']
dat4[grepl('LDH',acnm), endpoint_type := 'LDH']
dat4[grepl('AB',acnm), endpoint_type := 'CTB']
dat4[grepl('resistance',acnm), endpoint_type := 'IMP']
dat4[, in_dat4 := 1]
dat4 <- merge(dat4, wllq.tb, by = c('plate.id','rowi','coli','endpoint_type'), all = T,
              suffixes = c('.org','.wllqtb'))


# could have incorporate imp wllq=0 in the previous step (imp dat wllq=0 already removed manually)
# Check for data combinations that are in wllq.tb but not in dat4
#remove everything from MW89-4608
dat4[plate.id == "MW89-4608", ]$wllq.wllqtb <- 0
dat4[plate.id == "MW89-4608", ]$wllq_notes.wllqtb <- "Overall low activity across plate"
# if any rows not originally in dat4, check the data entry in wllq.tb

# Merge the wllq columns
dat4[, wllq := pmin(wllq.org, wllq.wllqtb, na.rm = T)]
dat4[is.na(wllq_notes.org), wllq_notes.org := '']
dat4[is.na(wllq_notes.wllqtb), wllq_notes.wllqtb := '']
dat4[, wllq_notes := paste0(wllq_notes.org, wllq_notes.wllqtb)]

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.
# for example, updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "C6", wllq_note = "Contamination", override_check = override_wllq_checks)

# start a pdf to save the summary graphs
graphics.off()
pdf(file = file.path(main.output.dir, paste0(dataset_title, "_summary_figures_report_",as.character.Date(Sys.Date()),".pdf")), width = 10, height = 8)


# * VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS -----


cat("\nVerifying control compound labels:\n")
# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = c("treatment","wllq")]
#dat4[grepl("DMSO",treatment), treatment := "DMSO"]

# Relabel "½ Lysis" wells to '1/2 Lysis'!!
# Need standard ascii characters
# This is the new treatment name that will be recognized
dat4[treatment == "½ Lysis", treatment := '1/2 Lysis']
dat4[treatment == "? Lysis", treatment := '1/2 Lysis']


# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
plotdat <- dat4[treatment %in% c("DMSO","H2O","Media","EtOH:DMSO","EtOH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# RESPONSE:
# yes/no, it appears that the PICRO, TTX, LYSIS were added before the second treatment
# rename the treatment in the wells as needed

###the lysis is added during the cytotoxicity assay, solvent controls(DMSO, H2O, etc) and treatments are added before 2nd recording

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to reflect this
# in these exp, all MEA treatment should be labelled just fine, lysis is always added to F1 even when there is solvent control in it
# will change it for CTB, since LDH is done with the media in the well containing treatment
dat4[endpoint_type == "CTB"& rowi == "6"&coli == "1", ]$treatment <- "Lysis"
dat4[endpoint_type == "CTB"& rowi == "6"&coli == "1", ]$conc <- "10"

# for Cell Titer Blue assay:
plotdat <- dat4[treatment %in% c("DMSO","Picrotoxin","Tetrodotoxin","EtOH:DMSO","EtOH", "H2O","Media","Lysis","1/2 Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# make updates if needed
# dat4[, AB.trt.finalized := FALSE] # set this to TRUE for individual plates as you update as needed
# 
# # for every other culture, the "Media" well in F1 contains Lysis at the time of the AB reading (or could change by well F1 vs by the name "Media"...)
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", .(plate.id, experiment.date, rowi, coli, wllq, rval, wllq_notes)] # all are in row 6, col 1
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", `:=`(treatment = "Lysis",conc = 10, AB.trt.finalized = TRUE)]

# # view updated stripchart
# plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","1/2 Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
# view_activity_stripchart(plotdat, title_additions = "Media renamed to Lysis")

# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","Picrotoxin","Tetrodotoxin","EtOH:DMSO","EtOH", "H2O","Media","Lysis","1/2 Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acnm)]
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
#spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
#names(spidmap)
#setnames(spidmap, old = "NCCT ID", new = "spid")
#setnames(spidmap, old = "Chemical ID", new = "treatment")
#setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1]   
#dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[, spid := dat4$treatment]
dat4[is.na(spid),unique(treatment)]
# [1] 
#dat4[grepl("DMSO",treatment), spid := "DMSO"]
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
#Number of unique spids: 15


# * PREPARE LDH P WELLS  --------------------------------------------------
# (must verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)
#Prepare LDH 'p' wells (using Lysis or Half Lysis wells):
#  Treatments assigned to wllt 'p' for each apid:
#  apid LDH_trts_in_p_wells N
#1: 20211005       2 * 1/2 Lysis 3
#2: 20211012       2 * 1/2 Lysis 3
#3: 20211019       2 * 1/2 Lysis 3
#4: 20210916       2 * 1/2 Lysis 3
#5: 20211018       2 * 1/2 Lysis 3

#Summary of median p wells by apid:
#  apid      pval
#1: 20211005 0.8706667
#2: 20211012 1.3366667
#3: 20211019 1.6907333
#4: 20210916 0.9490000
#5: 20211018 0.6420000

# * ASSIGN WLLT -----------------------------------------------------------
dat4 <- assign_wllt(dat4)
#Assign Wllt:
#  wllt will be set to 't' for the MEA components for the following spid's:
#H2O, Tween 20, Tributyltin Chloride, EtOH:DMSO, Deltamethrin, Glyphosate, EtOH, Lindane, Rotenone
#wllt will be set to 't' for the cytotoxicity components for the following spid's:
#  H2O, Tween 20, Tributyltin Chloride, EtOH:DMSO, Deltamethrin, Glyphosate, EtOH, Lindane, Rotenone

#Well Type Assignments for Control Compounds by assay component:
#  treatment       spid CellTiter Blue LDH MEA components
#1:          DMSO       DMSO              n   n              n
#2:         Media      Media              b   b              b
#3:    Picrotoxin Picrotoxin              z   z              p
#4: 2 * 1/2 Lysis Tritonx100              -   p              -
#  5:         Lysis Tritonx100              -   x              -
  
#  Unique of wllt:
#  [1] "t" "n" "z" "p" "b" "x"
#Warning message:
#  In assign_wllt(dat4) :
# Wllt is not defined for the following treatment with sample IDs:
#  EtOH:DMSO, Deltamethrin, Glyphosate, EtOH, Lindane, Rotenone
#Will set wllt:='t' for these compounds.

#H20, EtOH:DMSO, EtOH should have the same symbol as DMSO because they are all solvent
dat4[spid == "H2O", ]$wllt <- "n"
dat4[spid == "EtOH:DMSO", ]$wllt <- "n"
dat4[spid == "EtOH", ]$wllt <- "n"

# * CHECK CONC'S ----------------------------------------------------------
cat("\nFinalize Concentrations:\n")
dat4[, conc_srcf := conc]
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?

# update conc for DMSO, PICRO, TTX, BIC, and full Lysis wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] "Control"
# Use the percent DMSO by volume?
dat4[treatment == "DMSO", conc := "0.001"]
dat4[treatment == "EtOH:DMSO", conc := "0.001"]
dat4[treatment == "H2O", conc := "0.001"]
dat4[treatment == "EtOH", conc := "0.001"]
# picro
#dat4[treatment == "PICRO", .N, by = "conc"]
# 
# based on other lab notebook, this is usually 25
# dat4[treatment == "PICRO", conc := "25"]

# ttx
#dat4[treatment == "TTX", .N, by = "conc"]
# 
# based on other lab notebook, this is usually 1
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
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acnm, wllt, wllq, wllq_notes, rval, srcf, dat3, conc_srcf)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# you're done!
