###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA"
dataset_title <- "HESIConvulsant2018b" # e.g. "name2020"
select.neural.stats.files <- T # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- T # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 6 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- ""
use_sheet <- "" # sheet name in spidmap_file
# optional adjutsment; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl" # where the dataset_title folder will be created
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- 3 # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- TRUE
standard_analysis_duration_requirement <- FALSE # default should be true
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(openxlsx)
library(stringi)

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
run.type.tag.location <- checkFileNames(run.type.tag.location, main.output.dir, dataset_title, guess = T)
# OUTPUT --------------------------------------------------------- 
# Reading from HESIConsulsant2018b_neural_stats_files_log_2021-04-26.txt...
# Got 12 files.
# Store the run.type.tag.location.vector
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# HESIConsulsant2018b_check_summary_2021-04-26.txt is ready.
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT --------------------------------------------------------- 
# Level 1 - Extract All Data:
#   
#   Reading from HESIConvulsant2018b_neural_stats_files_log_2021-04-28.txt...
# Got 12 files.
# Reading data from files...
# Processed HESI_20180314_MW1207-15_12_00(000)_Neural Statistics Compiler(000).csv 
# Processed HESI_20180314_MW1207-15_12_02(000)_Neural Statistics Compiler(000).csv 
# Processed HESI_20180314_MW1207-16_12_00(000)_Neural Statistics Compiler(000).csv 
# Processed HESI_20180314_MW1207-16_12_02(000)_Neural Statistics Compiler(000).csv 
# Processed HESI_20180314_MW1207-17_12_00(001)_Neural Statistics Compiler(000).csv 
# Processed HESI_20180314_MW1207-17_12_02(000)_Neural Statistics Compiler(000).csv 
# Processed HS_Cortical_20180322_MW1207-18_12_00(000)_Neural Statistics Compiler(000).csv 
# Processed HS_Cortical_20180322_MW1207-18_12_02(000)_Neural Statistics Compiler(000).csv 
# Processed HS_Cortical_20180322_MW1207-19_12_00(000)_Neural Statistics Compiler(000).csv 
# Processed HS_Cortical_20180322_MW1207-19_12_02(000)_Neural Statistics Compiler(000).csv 
# Processed HS_Cortical_20180322_MW1207-20_12_00(000)_Neural Statistics Compiler(000).csv 
# Processed HS_Cortical_20180322_MW1207-20_12_02(000)_Neural Statistics Compiler(000).csv 
# 
# HESIConvulsant2018b_dat1_2021-04-28.RData is ready.
# Summary of dates/plates with wllq=0 at Level 1:
#   experiment.date  plate.id                                                                                                                 wllq_set_to_zero
# 1:        20180326 MW1207-15                                                                                                                         D1,E7,F8
# 2:        20180326 MW1207-16                                                                                                                      B5,B7,E8,F6
# 3:        20180326 MW1207-17                                                                                                                   B3,C7,D7,F1,F2
# 4:        20180403  20180322 A3,A4,A6,A7,A8,B1,B2,B3,B4,B5,B6,B7,B8,C1,C2,C3,C4,C5,C6,C7,C8,D1,D2,D3,D4,D5,D6,D7,D8,E1,E3,E4,E5,E6,E7,E8,F2,F3,F4,F5,F6,F7,F8
# Warning messages:
#   1: In fileToLongdat(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                         
#                         run type cannot be determined for HESI_20180314_MW1207-15_12_02(000)_Neural Statistics Compiler(000).csv.
#                       No wllq checks will be done for this recording.
#                       2: In fileToLongdat(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                                             
#                                             run type cannot be determined for HESI_20180314_MW1207-16_12_02(000)_Neural Statistics Compiler(000).csv.
#                                           No wllq checks will be done for this recording.
#                                           3: In fileToLongdat(new_files[i], run.type.tag.location[i], plate.id.tag.location = plate.id.tag.location,  :
#                                                                
# (rep for all treated plates)
# ---------------------------------------------------------------- 

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)

# Assing the wllt for the treated wells, where wllt tag is 02
dat1[, .N, by = .(run_type)]
# run_type     N
# 1: baseline 12384
# 2:  02(000) 12384
dat1[, .(length(unique(run_type))), by = .(experiment.date, plate.id)] # 2 for all
dat1[run_type == '02(000)', run_type := 'treated']
print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
# wllq_notes  V1
# 1:                                                      478
# 2:                        Baseline MFR < 0.6377603 Hz;   78
# 3:                              Baseline # of AE < 10;    2
# 4: Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz;   18
# view all experiment.date's and plate.id's. Are there any NA/missing labels?
dat1[, .N, by = .(experiment.date, plate.id)]
# experiment.date  plate.id     N
# 1:        20180326 MW1207-15  4128
# 2:        20180326 MW1207-16  4128
# 3:        20180326 MW1207-17  4128
# 4:        20180403  20180322 12384
# for second experiment date, plate id's are in 4th tag instead of 3rd
dat1[plate.id == '20180322', plate.id := stri_sub(srcf, from = 22, to = 30)]
dat1[, .N, by = .(experiment.date, plate.id)]
# experiment.date  plate.id    N
# 1:        20180326 MW1207-15 4128
# 2:        20180326 MW1207-16 4128
# 3:        20180326 MW1207-17 4128
# 4:        20180403 MW1207-18 4128
# 5:        20180403 MW1207-19 4128
# 6:        20180403 MW1207-20 4128
setkey(dat1, NULL)
save(dat1, file = file.path(main.output.dir, 'output',paste0(dataset_title,'_dat1_',as.character.Date(Sys.Date()),'.RData')))
rm(dat1)

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title, main.dir = root_output_dir)
# OUTPUT --------------------------------------------------------- 
# Level 2 - Collapse Data by Plate ID:
# 
# Loading...
# HESIConsulsant2018b_dat1_2021-04-26.RData 
# 
# Collapsing treated and baseline data...
# 20180326_MW1207-15
# Baseline stats file name: HESI_20180314_MW1207-15_12_00(000)_Neural Statistics Compiler(000).csv
# Treated stats file name: HESI_20180314_MW1207-15_12_02(000)_Neural Statistics Compiler(000).csv
# ...
# HESIConsulsant2018b_dat2_2021-04-26.RData is ready.
# ---------------------------------------------------------------- 

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -100.000  -23.064   -2.924    7.366   15.634 1643.937      366 
# ---------------------------------------------------------------- 
rm(dat2)

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 
# Found that the values in rows G3 and G6 for LDH are the averages of the 3 Lysis and half lysis wells, respectively
cytodat[grepl('LDH',acnm) & rowi == 7 & coli %in% c(3), `:=`(treatment = 'Lysis mean', conc = NA_real_)]
cytodat[grepl('LDH',acnm) & rowi == 7 & coli %in% c(6), `:=`(treatment = 'Half Lysis mean', conc = NA_real_)]
cytodat[grepl('AB',acnm), .N, by = .(rowi)] # AB only pulls data from rows 1-6
cytodat[, .N, by = .(treatment)] # all appear reasonable

# experiment date's not in calc file (body nor filename) -> get from srcf instead
cytodat[, .N, by = .(experiment.date, plate.id)]
# experiment.date  plate.id   N
# 1:              NA MW1207-15 104
# 2:              NA MW1207-16 104
# 3:              NA MW1207-17 104
# 4:              NA MW1207-18 104
# 5:              NA MW1207-19 104
# 6:              NA MW1207-20 104
cytodat[, experiment.date := NULL]
cytodat[, culture.date := stri_extract(str = srcf, regex = '[0-9]{8}')]
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[, culture.date := stri_extract(str = srcf, regex = '[0-9]{8}')]
date.tb <- dat2[, unique(.SD), .SDcols = c('culture.date', 'experiment.date', 'plate.id')]
cytodat <- merge(cytodat, date.tb, by = c('culture.date','plate.id'), all.x = T)
cytodat[, .N, by = .(experiment.date, plate.id)]
# experiment.date  plate.id   N
# 1:        20180326 MW1207-15 104
# 2:        20180326 MW1207-16 104
# 3:        20180326 MW1207-17 104
# 4:        20180403 MW1207-18 104
# 5:        20180403 MW1207-19 104
# 6:        20180403 MW1207-20 104
# cool, looks good now!
cytodat[, apid := experiment.date]
rm(list = c('dat2','date.tb'))

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Level 3 - Combine Cyto and Neural Stats Data; Initialize treatment, conc, and wllq
# 
# Loading...
# HESIConvulsant2018b_dat2_2021-04-28.RData 
# HESIConvulsant2018b_dat3_2021-04-28.RData is ready.
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
# NA rval's: 374
cat("\nInf rval's (baseline==0):",dat4[wllq==1 & is.infinite(rval),.N])
# Inf rval's (baseline==0): 0
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
plotdat <- dat4[treatment %in% c("DMSO","TTX","Bic","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# RESPONSE:
# yes, it appears that the PICRO, TTX, LYSIS were added before the second treatment

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to refect this

# for Cell Titer Blue assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","Bic","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
plotdat[treatment == 'DMSO' & rval < 500, .(plate.id, experiment.date, rowi, coli, acnm, wllq, rval, srcf, treatment, conc)]
#     plate.id experiment.date rowi coli                     acnm wllq     rval                                             srcf treatment conc
# 1: MW1207-15        20180326    6    1 CCTE_Shafer_MEA_acute_AB    1   0.0000               20180314 Culture Group 1 HESI.xlsx      DMSO    0
# 2: MW1207-16        20180326    6    1 CCTE_Shafer_MEA_acute_AB    1   0.0000               20180314 Culture Group 1 HESI.xlsx      DMSO    0
# 3: MW1207-17        20180326    6    1 CCTE_Shafer_MEA_acute_AB    0 417.3333               20180314 Culture Group 1 HESI.xlsx      DMSO    0
# 4: MW1207-18        20180403    6    1 CCTE_Shafer_MEA_acute_AB    1   0.0000 Group 2 HESI Calculations Cortical 20180322.xlsx      DMSO    0
# 5: MW1207-19        20180403    6    1 CCTE_Shafer_MEA_acute_AB    1 360.0000 Group 2 HESI Calculations Cortical 20180322.xlsx      DMSO    0
# 6: MW1207-20        20180403    6    1 CCTE_Shafer_MEA_acute_AB    1   0.0000 Group 2 HESI Calculations Cortical 20180322.xlsx      DMSO    0
# I have a feeling that something different happened in these wells... because this is just so low, and it's teh same well in all plates
# this is very likely supposed to be a lysis wells
dat4[grepl('AB',acnm) & rowi == 6 & coli == 1, `:=`(treatment = 'Lysis', conc = 'Lysis')]

# view updated stripchart
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","Bic","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "DMSO F1 Relabelled as Lysis")

# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","Bic","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
plotdat[treatment == 'Lysis', .(plate.id, experiment.date, rowi, coli, rval, treatment)][order(rval)]
# the big jump in Lysis values corresponds to diff plates, not diff e.g. wells -> so I don't think these should be different treatments

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
dat4[, spid := treatment]

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] empty
dat4[grepl("DMSO",treatment), spid := "DMSO"]
dat4[treatment == "Media", spid := "Media"]
dat4[treatment == "PICRO", spid := "Picrotoxin"]
dat4[treatment == "Bic", spid := "Bicuculline"]
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
# Number of unique spids: 16 

# PREPARE LDH P WELLS (must verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)
# Prepare LDH 'p' wells (using Lysis or Half Lysis wells):
#   Treatments assigned to wllt 'p' for each apid:
#        apid LDH_trts_in_p_wells N
# 1: 20180326         2 * ½ Lysis 9
# 2: 20180403         2 * ½ Lysis 9
# 
# Summary of median p wells by apid:
#        apid     pval
# 1: 20180326 1.078600
# 2: 20180403 2.208667

# ASSIGN WLLT
# Have to trick assign_Wllt fun into believing that the tested treatment names are spids by adding a numeric prefix
dat4[!(spid %in% c('DMSO','Picrotoxin','Bicuculline','Tetrodotoxin','Tritonx100')), spid := paste0('1',spid)]
dat4 <- assign_wllt(dat4)
# Assign Wllt:
#   wllt will be set to 't' for the MEA components for the following spid's:
# 1Pentylenetetrazole, 1Chlorpromazine, 1Phenytoin, 1Linopirdine, 1Amoxicillin, 1Strychnine, 1Pilocarpine, 1Amoxapine, 1Enoxacin, 14-Aminopiridine, 1Acetaminophen
# wllt will be set to 't' for the cytotoxicity components for the following spid's:
#   1Pentylenetetrazole, 1Chlorpromazine, 1Phenytoin, 1Linopirdine, 1Amoxicillin, 1Strychnine, 1Pilocarpine, 1Amoxapine, 1Enoxacin, 14-Aminopiridine, 1Acetaminophen
# 
# Well Type Assignments for Control Compounds by assay component:
#   treatment         spid CellTiter Blue LDH MEA components
# 1:             Bic  Bicuculline              z   z              p
# 2:            DMSO         DMSO              n   n              n
# 3:      Picrotoxin   Picrotoxin              z   z              p
# 4:             TTX Tetrodotoxin              x   x              p
# 5:     2 * ½ Lysis   Tritonx100              -   p              -
#   6: Half Lysis mean   Tritonx100              -   x              -
#   7:           Lysis   Tritonx100              p   x              -
#   8:      Lysis mean   Tritonx100              -   x              -
#   
#   Unique of wllt:
#   [1] "n" "t" "p" "z" "x"


# CHECK CONC'S
cat("\nFinalize Concentrations:\n")
dat4[, conc_original := conc]
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?

# # update conc for DMSO, PICRO, TTX, BIC, and full Lysis wells
# # dmso
# dat4[treatment == "DMSO",unique(conc)]
# # [1] "Control"
# # Use the percent DMSO by volume?
# # dat4[treatment == "DMSO", conc := "0.001"]
# 
# # picro
# dat4[treatment == "PICRO", .N, by = "conc"]
# # 
# # based on lab notebook, this is usually 25
# # dat4[treatment == "PICRO", conc := "25"]
# 
# # ttx
# dat4[treatment == "TTX", .N, by = "conc"]
# # 
# # based on lab notebook, this is usually 1
# # dat4[treatment == "TTX", conc := "1"]

dat4[is.na(conc), .N, by = .(treatment, acnm)]
# treatment                      acnm N
# 1:      Lysis mean CCTE_Shafer_MEA_acute_LDH 6
# 2: Half Lysis mean CCTE_Shafer_MEA_acute_LDH 6
# conc can stay NA here
dat4[is.na(as.numeric(conc)), .N, by = .(treatment, spid, acnm, conc)]
#          treatment       spid                      acnm    conc  N
# 1:           Lysis Tritonx100  CCTE_Shafer_MEA_acute_AB   Lysis  6
# 2:      Lysis mean Tritonx100 CCTE_Shafer_MEA_acute_LDH    <NA>  6
# 3: Half Lysis mean Tritonx100 CCTE_Shafer_MEA_acute_LDH    <NA>  6
# 4:           Lysis Tritonx100 CCTE_Shafer_MEA_acute_LDH   Lysis 18
# 5:     2 * ½ Lysis Tritonx100 CCTE_Shafer_MEA_acute_LDH ½ Lysis 18
dat4[is.na(as.numeric(conc)), conc := NA_real_]


cat("\nConcentration Corrections:\n")
# any other compounds to update??
# need to do concentration correction??
cat("CHANGES MADE/rationale")
# cat("The following treatment have char conc. Will be set to NA:\n")
# print(suppressWarnings(dat4[is.na(as.numeric(conc)), .N, by = c("spid","treatment","conc")]))
dat4[, conc := suppressWarnings(as.numeric(conc))]
dat4[, .N, by = .(conc)]

# final updates, view conc's, make table of control conc's
# dat4 <- assign_common_conc(dat4)
# (not run, since don't have spidmap and conc's to correct)

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
save(dat4, file = file.path(root_output_dir, dataset_title, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# you're done!
