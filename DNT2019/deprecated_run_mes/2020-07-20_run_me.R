###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA"
dataset_title <- "DNT2019"
setwd(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/",dataset_title)) # where you want output to go
spidmap_file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
use_sheet <- "MEA Acute Conc Res"
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
override_wllq_checks <- TRUE # set to TRUE if you have already verified your wllq updates
###################################################################################
# END USER INPUT
###################################################################################

# load packages
library(data.table)
library(readxl)

# source all function in folder 'mea-acute-neural-stats-to-mc0-scripts', except for the run_me.R template
scripts <- list.files(path = "../mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T)
scripts <- scripts[!grepl("run_me\\.R",scripts) & !grepl("wllt_conc_formalization\\.R",scripts)]
sapply(scripts, source)

# loading some information for the funtions to reference
get_acsn_map() # load the acsn map with the appropriate endpoints

main.output.dir <- getwd()

if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

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
# Reading from DNT2019_neural_stats_files_log_2020-05-18.txt...
# Got 78 files.
# Reading data from files...
# Processed TC_20190508_MW68-0808_13_00(000).csv
# Processed TC_20190508_MW68-0808_13_01(000).csv
# Processed TC_20190508_MW68-0811_13_00(000).csv
# Processed TC_20190508_MW68-0811_13_01(000).csv
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_dat1_2020-06-18.RData is ready.
# apid                                                                                            wllq_set_to_zero
# 1: 20190528                                  A2,A3,B2,B3,B4,B5,B6,B7,C3,C5,C6,C7,D1,D3,D4,D5,D6,D7,E1,E2,E3,E6,F2,F3,F7
# 2: 20190530 A1,A2,A3,A4,A5,A6,A8,B2,B3,B5,B6,B7,B8,C3,C4,C5,C6,C7,D3,D4,D5,D6,D7,E1,E2,E3,E6,E7,E8,F1,F2,F3,F4,F5,F6,F7
# 3: 20190611                                                                            A2,A3,A5,A6,A7,B1,E5,F2,F4,F5,F6
# 4: 20190613                                                 A1,A2,A5,A6,A7,A8,B1,B2,B4,C1,C8,D1,E1,E3,F1,F2,F3,F5,F7,F8
# 5: 20190618             A1,A2,A5,A7,A8,B2,B3,B4,B6,B7,C1,C2,C3,C4,C5,C6,C7,C8,D1,D2,D3,D5,D6,D7,E2,E3,E6,E7,F2,F3,F4,F7
# 6: 20190626                                                                         A1,A2,A3,A4,A6,A7,A8,F1,F2,F3,F4,F6
# 7: 20190627                A1,A2,A3,A4,A5,A6,A7,A8,B1,B2,B3,B4,B5,B6,B7,B8,C1,E1,E3,E4,E6,E7,E8,F1,F2,F3,F4,F5,F6,F7,F8
# 8: 20190730                                                                                              A4,A5,A8,E4,F1
# 9: 20190801                                                                                                       D1,E8
# 10: 20190827                                                                                              A7,C5,D4,E2,F4
# 11: 20190829                                                                               A3,A5,A6,A8,B8,C3,D3,F3,F5,F8
# 12: 20190926                                                                            A1,A2,A4,A5,A6,A8,B1,C8,E8,F1,F5
# 13: 20191008       A1,A2,A4,A5,B1,B2,B3,B4,B5,B6,C2,C4,C5,C6,C7,C8,D2,D3,D4,D5,D6,D7,D8,E3,E4,E5,E7,F1,F2,F3,F5,F6,F7,F8
# ---------------------------------------------------------------- 

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)
str(dat1)
names(dat1)
dat1[grepl("firing",acsn) & is.na(activity_value), .(unique(wllq), unique(wllq_notes)), by = "run_type"]
# run_type V1                                                                        V2
# 1: baseline  0 # of AE less than 10 in baseline recording; Baseline MFR < 0.3310055 Hz; 
# 2:  treated NA 
# good, the is.na() check is working
dat1[wllq == 0, .(wllq_set_to_zero = paste0(sort(unique(well)),collapse=",")), by = c("plate.id","experiment.date")][order(experiment.date, plate.id)]
dat1[, .N/length(unique(dat1$acsn)), by = "wllq_notes"]
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
dat2[is.na(rval), .N, by = "wllq"]
#    wllq    N
# 1:    1 1470
# 2:    0  288
names(dat2)
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
str(cytodat)
any(is.na(cytodat)) # FALSE

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Loading...
# DNT2019_dat2_2020-06-18.RData
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_dat3_2020-06-18.RData is ready.
# ---------------------------------------------------------------- 
rm(cytodat)


# load dat3, rename to dat4, then finalize it
dat4 <- get_latest_dat(lvl = "dat3", dataset_title)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]
str(dat4)

# FINALIZE WLLQ

# set wllq to zero where rval is NA
dat4[wllq==1 & is.na(rval),.N] 
# [1] 1470
dat4[is.na(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is NA; "))]

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


# VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS

# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = "treatment"]
dat4[grepl("DMSO",treatment), treatment := "DMSO"]

# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
boxplot(rval ~ treatment, dat4[wllq == 1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
stripchart(rval ~ treatment, dat4[wllq == 1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("firing_rate",acsn)], 
           ylab = "percent change in mean firing rate", vertical = T, pch = 1, method = "jitter")
title(main = paste0("Percent Change in Mean Firing rate\nin Control wells of ",dataset_title," MEA acute Experiments"))
# RESPONSE:
# yes, it appears that the PICRO, TTX, LYSIS were added before the second treatment


# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to refect this

# for Cell Titer Blue assay:
stripchart(rval ~ treatment, dat4[wllq==1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "CellTiter Blue Blank-Corrected Fluorescence Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", add=T, col = "red")

dat4[, AB.trt.finalized := FALSE]

# The 2 instances where "Media" wells (which should actually contian Lysis for AB) are significantly greater than 0
dat4[treatment == "Media" & grepl("AB",acsn), .(num_high = sum(rval>10000), num_low = sum(rval<10000)), by = c("apid","srcf")]
# the only apid with 2 plates where Media appears to be higher: 20190717_Calculations_DNT Group_5 Repeata.xlsx , exp date 20190730
# note in this file:  **Controls are rotating but the same F1 well is lysed across 3 plates
dat4[grepl("20190730",apid) & grepl("AB",acsn) & rowi == 6 & coli==1, .(apid, plate.id, treatment, rval, conc)]
#        apid  plate.id treatment    rval conc
# 1: 20190730 MW68-0814     Media 613.333   10
# 2: 20190730 MW68-0817     PICRO 450.000   25
# 3: 20190730 MW68-0818       TTX 350.667    1
dat4[grepl("20190730",apid) & grepl("AB",acsn) & rowi == 6 & coli==1, `:=`(treatment = paste0(treatment,"/Lysis"), conc = 10)] # will set spid to just Tritonx100 for AB
dat4[grepl("20190730",apid) & grepl("AB",acsn), AB.trt.finalized := TRUE] # don't need to change the treatment for any other wells here

# from the lab notebook, G2 extras experiment date 20190530 "TTX wells lysed post recording to be used as Total LDH" (instead of Media wells in F1, since these contaminated)
dat4[grepl("20190530", apid) & grepl("AB",acsn) & treatment %in% c("Media","TTX"), .(plate.id, rowi, coli, wllq, wllq_notes, rval, treatment)]
# plate.id      rowi coli wllq                                                                               wllq_notes  rval treatment
# 1: MW68-0807    4    1    1                                                                                              0       TTX
# 2: MW68-0807    6    1    0 # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz; Contamination;      0     Media
# 3: MW68-0809    4    1    1                                                                                              0       TTX
# 4: MW68-0809    5    1    0 # of AE less than 10 in baseline recording; Baseline MFR < 0.6377603 Hz; Contamination;      0     Media
# 5: MW68-0810    4    1    1                                                                                          20066       TTX
# 6: MW68-0810    6    1    0                                                            Baseline MFR > 3.4036511 Hz;      0     Media
# It looks like TTX was lysed in MW68-0807 and MW68-0809, but not in MW68-0810. This makes sense, since there was no contamination in MW68-0810
# Futhermore, I am willing to make that guess, since the Lysis wells do not directly affect the AB values (or even LDH, since we will use the 1/2 Lysis wells regardless)
dat4[grepl("20190530", apid) & plate.id %in% c("MW68-0807","MW68-0809") & grepl("AB",acsn) & treatment == "TTX", `:=`(treatment = paste0(treatment,"/Lysis"), conc = 10)]
dat4[grepl("20190530", apid) & plate.id %in% c("MW68-0807","MW68-0809") & grepl("AB",acsn), AB.trt.finalized := TRUE] # don't need to change the treatment for any other wells on these 2 plates

# for every other culture, the "Media" well in F1 contains Lysis at the time of the AB reading (or could change by well F1 vs by the name "Media"...)
dat4[AB.trt.finalized == FALSE & grepl("AB",acsn) & treatment == "Media", .(plate.id, experiment.date, rowi, coli, wllq, rval, wllq_notes)] # all are in row 6, col 1
dat4[AB.trt.finalized == FALSE & grepl("AB",acsn) & treatment == "Media", `:=`(treatment = "Lysis",conc = 10, AB.trt.finalized = TRUE)]

# view updated stripchart
stripchart(rval ~ treatment, dat4[wllq==1&treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH","TTX/Lysis","Media/Lysis","PICRO/Lysis") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "CellTiter Blue Blank-Corrected Fluorescence Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0& treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH","TTX/Lysis","Media/Lysis","PICRO/Lysis") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", col = "red", add = T)
# looks great! Only wells containing Lysis or wllq==0 are near 0


# for LDH assay:
stripchart(rval ~ treatment, dat4[wllq==1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "LDH Blank-Corrected Optical Density Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acsn)],
           vertical = T, pch = 1, method = "jitter", add=T, col = "red")
# note that the added stripchart does nto follow the same category bins, sadly

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

# other note: where treatment == 0, these are Media wells
dat4[treatment == "0", .N, by = "apid"]
# apid   N
# 1: 20191008 357
dat4[treatment == "0", `:=`(treatment = "Media", conc = 10)]


# PREPARE P WELLS FROM LDH
# (must be done after finalize wllq, treatment in ea well)
dat4 <- prepare_LDH_p_wells(dat4)
# Treatments assigned to wllt 'p' for each apid:
#   apid LDH_trts_in_p_wells N
# 1: 20190530         2 * ½ Lysis 9
# 2: 20190528         2 * ½ Lysis 9
# 3: 20190730         2 * ½ Lysis 9
# 4: 20190611         2 * ½ Lysis 9
# 5: 20190613         2 * ½ Lysis 9
# 6: 20190618         2 * ½ Lysis 9
# 7: 20190626         2 * ½ Lysis 9
# 8: 20190627         2 * ½ Lysis 9
# 9: 20190801         2 * ½ Lysis 9
# 10: 20190827         2 * ½ Lysis 9
# 11: 20190829         2 * ½ Lysis 9
# 12: 20190926         2 * ½ Lysis 9
# 13: 20191008         2 * ½ Lysis 9
dat4[wllt == "p", .(pval = median(rval)), by = "apid"]


# ASSIGN SPIDS
spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = "NCCT ID", new = "spid")
setnames(spidmap, old = "Chemical ID", new = "treatment")
spidmap$treatment <- as.character(spidmap$treatment) # since these treatments are labelled numerically, were read as integers
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1] "DMSO"        "TTX"         "PICRO"       "Media"       "Lysis"       "TTX/Lysis"   "Media/Lysis" "PICRO/Lysis" "2 * ½ Lysis"
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")
# to transform the NCCT ID into the EPA Sample ID, need to add "EPAPLT0". This is how the SPIDs are created in the Calculations files, Dosing Plate tab
dat4[!is.na(spid), spid := paste0("EPAPLT0",spid)]

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] "2 * ½ Lysis" "DMSO"        "Lysis"       "Media"       "Media/Lysis" "PICRO"       "PICRO/Lysis" "TTX"         "TTX/Lysis"  
dat4[grepl("DMSO",treatment), spid := "DMSO"]
dat4[treatment == "Media", spid := "Media"]
dat4[treatment == "PICRO", spid := "Picrotoxin"]
dat4[treatment == "TTX", spid := "Tetrodotoxin"]
dat4[grepl("Lysis",treatment), spid := "Tritonx100"]
dat4[grepl("Lysis",treatment), unique(conc), by = "treatment"]
unique(dat4$spid) # confirm no NA spids


# ASSIGN WLLT

# make sure all non-tested spids are in 
# "DMSO" "Media"         "Picrotoxin"    "Tetrodotoxin" "Tritonx100" "Bicuculline"
# make sure there are no NA spids
dat4[,unique(spid)]
dat4 <- assign_wllt(dat4)
# treatment         spid CellTiter Blue  LDH MEA endpoints
# 1:        DMSO         DMSO              n    n             n
# 2:       Media        Media              b    b             b
# 3:       PICRO   Picrotoxin              z    z             p
# 4:         TTX Tetrodotoxin              x    x             p
# 5: 2 * ½ Lysis   Tritonx100           <NA>    p          <NA>
# 6:       Lysis   Tritonx100              p    x             v
# 7: Media/Lysis   Tritonx100              p <NA>          <NA>
# 8: PICRO/Lysis   Tritonx100              p <NA>          <NA>
# 9:   TTX/Lysis   Tritonx100              p <NA>          <NA>
dat4[,unique(wllt)]
# "t" "p" "n" "v" "x" "b" "z"

# CHECK CONC'S (conc's for controls can just make those follow the treatments)
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done in the calculatiosn files already?

# update conc for control wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] "Control"
# I'm just gonna gues 0.002, as in Toxcast compounds. bc it doesn't really matter
dat4[treatment == "DMSO", conc := "0.002"]

# picro
dat4[treatment == "PICRO", .N, by = "conc"]
# conc   N
# 1:   25 594
# 2:   10  51
# 3:    1  17
# based on lab notebook, this should always be 25 (there can be mix ups in calc file)
dat4[treatment == "PICRO", conc := "25"]

# ttx
dat4[treatment == "TTX", .N, by = "conc"]
# conc   N
# 1:    1 628
# 2:   10  17
# 3:   25  17
# based on lab notebook, this should always be 1
dat4[treatment == "TTX", conc := "1"]

# media
dat4[treatment == "Media", .N, by = "conc"]
# conc   N
# 1:   10 873
# 2:   25  48
# 3:    1  16
# I am pretty sure Media should always be 10
dat4[treatment == "Media", conc := "10"]

# lysis
dat4[grepl("Lysis",treatment), .N, by = c("treatment","conc")]
# treatment    conc   N
# 1:       Lysis      10  82
# 2:   TTX/Lysis      10   3
# 3: Media/Lysis      10   1
# 4: PICRO/Lysis      10   1
# 5:       Lysis   Lysis 117
# 6: 2 * ½ Lysis ½ Lysis 117
# Any Lysis wells will be 10, 1/2 Lysis wells will be labelled 5 (since these wells are half Lysis half Media dilution)
dat4[grepl("Lysis",treatment) & !grepl("½",treatment), conc := "10"]
dat4[treatment == "2 * ½ Lysis", conc := NA]

# make conc's numeric
dat4[, sort(unique(conc))]
dat4[, conc := as.numeric(conc)]
dat4[, sort(unique(conc))]


# ASSIGN ACID
dat4 <- add_acid(dat4)


# check that all data is there, nothing is missing
unique(dat4$acsn)
# all 17 are there
unique(dat4$wllq) # any NA?
# [1] 1 0
length(unique(dat4$plate.id)) # correct number of plates?
# 39 plates, 29*2 = 78 files originally selected
sort(unique(dat4$conc))
check.points <- dcast(dat4[, .N, by = c("acsn","plate.id")], plate.id ~ acsn, value.var = "N")
setnames(check.points, old = names(check.points), new = sub("NHEERL_MEA_acute_","",names(check.points)))
check.points
# 39 plates = 13 experiment dates * 3 plates
# each endpoint has 48 values from each plate,
# except for LDH, which has 54 points = 48 + (3 Lysis + 3 ½ Lysis wells)

# visualizations/confirmation
boxplot(rval ~ acsn, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acsn)])
boxplot(rval ~ acsn, dat4[wllq == 1 & grepl("AB",acsn)], main = "All CellTiter Blue Blank-corrected values")
boxplot(rval ~ acsn, dat4[wllq == 1 & grepl("LDH",acsn)], main = "All LDH Blank-corrected values")
plot(dat4[wllq == 1 & !grepl("AB",acsn), .(log10(conc), rval)], xlab = "log10(conc)", ylab = "rval (percent change in activity)")
title(main = paste0("All Neural Stats Points where wllq=1 for ",dataset_title))

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)

# save the dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acsn, acid, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))

# done!
