###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project TSCA_APCRA"
dataset_title <- "APCRA2019"
setwd(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/",dataset_title)) # where you want output to go
select.neural.stats.files <- T # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- T # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
threshold <- 1*60 # how many seconds off of 40 minutes should signal a flag?
parameter_set_type <- "post_july_2016" # for backwards compatibility
###################################################################################
# END USER INPUT
###################################################################################

# load packages
library(data.table)
library(readxl)
library(pracma)
library(tcpl)

# source functions from master scripts
source('../mea-acute-neural-stats-to-mc0-scripts/gather_files-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/check-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/data_prep-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/collapsePlateData.R')
source('../mea-acute-neural-stats-to-mc0-scripts/acute_cytotox_prep06.R')
source('../mea-acute-neural-stats-to-mc0-scripts/combineNeuralAndCyto.R')

# loading some information for the funtions to reference
load("../mea-acute-neural-stats-to-mc0-scripts/standard_settings.RData")
get_acsn_map(type = parameter_set_type) # load the acsn map with the appropriate endpoints

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
checkFileNames(main.output.dir)
# OUTPUT --------------------------------------------------------- 
# Reading from APCRA2019_neural_stats_files_log_2020-05-21.txt...
# Got 94 files.
# The following files appear to be named incorrectly:
#   filenames run.type.tags
# 1: TC_20190417_MW67-3707_13_00(000).csv            00
# 2: TC_20190417_MW67-3707_15_01(000).csv            01
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title, check.settings = T, check.parameters = T, check.timing = T, threshold = threshold), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# APCRA2019_check_summary_2020-05-21.txt is ready.
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (alldat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, append = F, threshold = threshold,
               check.settings = F, check.parameters = T, check.timing = F)
# OUTPUT --------------------------------------------------------- 
# 
# Reading from APCRA2019_neural_stats_files_log_2020-05-21.txt...
# Got 94 files.
# Reading data from files...
# Processed TC_20190313_MW1236-16_13_00(000).csv
# Processed TC_20190313_MW1236-16_13_01(000).csv
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/output/APCRA2019_alldat1_2020-05-21.RData is ready.
# ---------------------------------------------------------------- 

# fix the plate that was missing a barcode
load("output/APCRA2019_alldat1_2020-05-21.RData")
alldat1[grepl("No Barcode",apid), apid := "20190404_MW1237-11"]
save(alldat1, file = "output/APCRA2019_alldat1_2020-05-21.RData")
rm(alldat1)

# collapse the plate data by calculating the percent change in activity (alldat2)
collapsePlateData(main.output.dir)
# OUTPUT --------------------------------------------------------- 
# Loading L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/output/APCRA2019_alldat1_2020-05-21.RData ...
# Collapsing treated and baseline data...
# 
# 20190326_MW1236-16
# Baseline stats file name: TC_20190313_MW1236-16_13_00(000).csv
# Treated stats file name: TC_20190313_MW1236-16_13_01(000).csv
# ...
# 20190516_MW68-0719
# Baseline stats file name: TC_20190501_MW68-0719_15_00(000).csv
# Treated stats file name: TC_20190501_MW68-0719_15_01(000).csv
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/output/APCRA2019_alldat2_2020-05-21.RData is ready.
# ---------------------------------------------------------------- 

# look at data so far
load(file = paste0("output/",dataset_title,"_alldat2_",as.character.Date(Sys.Date()),".RData"))
alldat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -100.000  -38.164   -6.027   -6.148    8.772 3926.560     1242 
# ---------------------------------------------------------------- 
rm(alldat2)

# get cytotox data
cytodat <- getCytoData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Reading from APCRA2019_calculations_files_log_2020-05-21.txt...
# Got 16 files.
# Reading data from files...
# ...
# 20190403_Calculations_Group_1_missingData(LDH).xlsxNew names:
# ..
# MW66-9604 AB
# MW66-9604 LDH
# valuemap contains NAs
# ...42 ...43 ...44 ...45 ...46 ...47 ...48 ...49
# 4  <NA>  <NA>    NA    NA    NA    NA    NA    NA
# 5  <NA>  <NA>    NA    NA    NA    NA    NA    NA
# 6  <NA>  <NA>    NA    NA    NA    NA    NA    NA
# 7  <NA>  <NA>    NA    NA    NA    NA    NA    NA
# 8  <NA>  <NA>    NA    NA    NA    NA    NA    NA
# 9  <NA>  <NA>    NA    NA    NA    NA    NA    NA
# Do you wish to continue anyways? (y/n): y [this data is truly missing]
# 20190417_Calculations_Group_2_checked.xlsx 
# MW67-3707 AB
# valuemap contains NAs
# ...42              ...43     ...44     ...45     ...46     ...47     ...48     ...49
# 4   0.96314784906768458 1.1961349287916605 1.3013875 1.2255910 1.0270610 1.2341249 1.2165982 1.1755341
# 5    1.0665651152547349  1.196914917045955 0.9536503 1.2411907 0.9179544 0.8136195 0.7774648        NA
# 6    1.1101985758332109 1.3151978417266186 1.2179287 1.1256607 1.2358226 1.2417872 1.1325429 1.2397684
# 7   0.82114410512406399 1.3033603729261489 1.0755579 1.1348370 1.1186408 0.8221076 1.0150400 0.9831064
# 8    1.0389443547203054 1.1587413742475408 1.1966855 1.0925800 1.2037972 1.0110024 1.1226784 1.1694777
# 9 3.7944134488327702E-2 1.1525014682131844 1.0221975 0.9338753 0.9982932 1.2170570 1.1903538 1.0464689
# Do you wish to continue anyways? (y/n): y [from lab notebook, this well was off]
# MW67-3707 LDH
# MW67-3708 AB
# valuemap contains NAs
# ...42               ...43    ...44    ...45    ...46     ...47    ...48    ...49
# 15    1.0116669711145174  1.3091787055936335 1.312594 1.140290 1.204621 1.3727554 1.140528 1.241154
# 16    1.0749656503403198 0.96095654867326918 1.349326 1.242346 1.038869 1.7106131 1.321688       NA
# 17    1.0136525005758035  1.0906116224952545 1.258746 1.189372 1.003725 1.1406073 1.010595 1.152798
# 18   0.96314063108068393  1.1523218781520279 1.345633 0.992725 1.035017 0.9633789 1.152004 1.191238
# 19    0.9365742468886753  1.1678884291285114 1.229758 1.040775 1.219711 1.2334109 1.347142 1.501100
# 20 5.4125533114660354E-2  1.1639570807951649 1.255689 1.120434 1.317915 1.1755526 1.355481 1.149105
# Do you wish to continue anyways? (y/n): y
# # ---------------------------------------------------------------- 
str(cytodat)

# combine the cytodat with alldat2, add trt, conc, and wllq to ea (alldat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/output/APCRA2019_alldat3_2020-05-21.RData is ready.
# Warning message:
#   In combineNeuralAndCyto(cytodat, main.output.dir, dataset_title) :
#   The following apid's are only found in cytodat (and not in alldat2): 20190402_MW1236-24
# Wllq will be set to 1 for these plates.
# ---------------------------------------------------------------- 
rm(cytodat)

# load alldat3 and finalize it
load(file = paste0("output/",dataset_title,"_alldat3_",as.character.Date(Sys.Date()),".RData"))


# FINALIZE WLLQ

# set wllq to zero where rval is NA
alldat3[, unique(wllq)] # [1] 1 0 (none are NA)
alldat3[wllq==1 & is.na(rval),.N] # 1290
alldat3[is.na(rval), wllq := 0]

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.
# Group 5 compounds left out at room temperature overnight...
# see supp script, I am going to keep this in

# 20190403 Culture, plate 3 had fungi growht in a well
alldat3[grepl("20190418",apid),unique(apid)]
# [1] "20190418_MW66-9801" "20190418_MW66-9803" "20190418_MW66-9804"
alldat3[apid == "20190418_MW66-9804" & rowi==4 & coli==6, unique(treatment)] # "E02_3"
stripchart(rval ~ acsn, alldat3[treatment == "E02_3"], vertical=T, method = "jitter",pch=1)
# I am not sure

# 20190417 Culture, plate 5 well d6

# 20190417 Culture , CTB wells
alldat3[grepl("MW67-3707",apid) & rowi==2 & coli==8 & grepl("AB",acsn), wllq:=0]
alldat3[grepl("MW67-3708",apid) & rowi==2 & coli==8 & grepl("AB",acsn), wllq:=0]

# 20190424 Culture, cloudiness in well
alldat3[grepl("20190509_MW67-3714",apid) & rowi==2 & coli==3, wllq:=0]

# outliers in LDH plate
alldat3[grepl("MW68-0704",apid) & rowi==1 & coli==8]


# ASSIGN SPIDS

spidmap <- as.data.table(read_excel("L:/Lab/NHEERL_MEA/Project TSCA_APCRA/EPA_18235_EPA-Shafer_84_20181129.xlsx"))
names(spidmap)
unique(spidmap$EPA_SAMPLE_ID)
# fix up treatments, so don't contain conc's
setnames(alldat3, old = "treatment", new = "treatment_original")
alldat3[, treatment := unlist(lapply(treatment_original, function(x) strsplit(x, split = "_")[[1]][1]))]
setdiff(unique(alldat3$treatment), spidmap$ALIQUOT_WELL_ID)
# [1] "TTX5"   "PICRO5" "DMSO"   "TTX3"   "PICRO3" "LYSIS"  "DMSOa"  "DMSOb"  "DMSOc"  "TTX"    "PICRO" 
# cool! so everything else is in this table
setnames(spidmap, old = c("EPA_SAMPLE_ID","ALIQUOT_WELL_ID"), new = c("spid","treatment"))

alldat3 <- merge(x = alldat3, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
alldat3[is.na(spid),unique(treatment)]
# [1] "DMSO"   "DMSOa"  "DMSOb"  "DMSOc"  "LYSIS"  "PICRO"  "PICRO3" "PICRO5" "TTX"    "TTX3"   "TTX5"  
alldat3[grepl("DMSO",treatment), spid := "DMSO"]
alldat3[grepl("PICRO",treatment), spid := "Picrotoxin"]
alldat3[grepl("TTX",treatment), spid := "Tetrodotoxin"]
alldat3[treatment == "LYSIS", spid := "Tritonx100"]
unique(alldat3$spid)
# visually confirm if the PICRO, TTX, LYSIS were added before the second treatment
# varies across experiments, sometimes across days
# if not, these wells only contained media for the MEA recordings
# boxplot(rval ~ spid, alldat3[spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
stripchart(rval ~ spid, alldat3[wllq == 1&spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("firing_rate",acsn)], 
           ylab = "percent change in mean firing rate", vertical=T, pch=1, method="jitter")
title(main = paste0("Percent Change in Mean Firing rate\nin Control wells of ",dataset_title," MEA acute Experiments"))
# RESPONSE:
# conversation with Kathleen: For the first culture, Mar 13, PICRO (and other controls) were added after the second recording
# but for the rest of the recordings, PICRO was added before the second recording
alldat3[grepl("Picrototoxin",spid) & grepl("(20190326)|(20190328)",apid) & !(grepl("(AB)|(LDH)",acsn)), `:=`(treatment="Media",spid="Media")]
# I assume the same for TTX
alldat3[grepl("Tetrodotoxin",spid) & grepl("(20190326)|(20190328)",apid) & !(grepl("(AB)|(LDH)",acsn)), `:=`(treatment="Media",spid="Media")]
# Let's check out the few large lysis wells
alldat3[grepl("Tritonx100",spid) & rval > -50 & wllq ==1 & grepl("firing_rate",acsn)]
# ya, LYSIS was most likely added after the seond recording here as well
alldat3[grepl("Tritonx100",spid) & grepl("(20190326)|(20190328)",apid) & !(grepl("(AB)|(LDH)",acsn)), `:=`(treatment="Media",spid="Media")]
# updated plot:
stripchart(rval ~ spid, alldat3[wllq == 1&spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("firing_rate",acsn)], 
           ylab = "percent change in mean firing rate", vertical=T, pch=1, method="jitter")
title(main = paste0("Percent Change in Mean Firing rate\nin Control wells of ",dataset_title," MEA acute Experiments"))
# that looks much better!


# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Visually confirm if correct
stripchart(rval ~ spid, alldat3[spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "CellTiter Blue Percent of Control Values for Control Compounds")
# There are no wells labelled "Media" for AB. Lysis is clearly positive control
# this looks good.

# determine if Media is Lysis for LDH assay
stripchart(rval ~ spid, alldat3[spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("(LDH)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "LDH Percent of Control Values for Control Compounds")
# again, no "Media" labelled wells. There is a big range on the Tritonx100 wells..
# does it depend on the culture, or just noise?
alldat3[spid == "Tritonx100"  & grepl("(LDH)",acsn) & wllq==1, min(rval,na.rm=T), by = "treatment_original"]
# my question:
# are some of the lysis wells, esp from teh first culture, are these actually just media?
# i'm just goign to leave it as is.


# MAKE SURE THAT CYTOTOX VALUES ARE ON SCALE OF 0-100 (versus 0-1)
alldat3[grepl("LDH",acsn), range(rval, na.rm=T)]
# [1] [1] -0.1434669  1.7420560
alldat3[grepl("LDH",acsn), rval := rval*100]
alldat3[grepl("AB", acsn), range(rval,na.rm=T)]
# [1] 0.000000 2.178872
alldat3[grepl("AB", acsn), rval := rval*100]

# MAKE AB VALUES %DEAD
alldat3[grepl("AB",acsn), median(rval), by = "treatment"] # LYSIS values are smallest
alldat3[grepl("AB",acsn), rval:=100-rval]


# ASSIGN WLLT
# for neural stats endpoints:
alldat3[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid == "DMSO", wllt := "n"]
alldat3[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid == "Tritonx100", wllt := "x"] # only positve control for cytotox assay
alldat3[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid == "Picrotoxin", wllt := "p"]
alldat3[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid == "Tetrodotoxin", wllt := "m"]
alldat3[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid == "Media", wllt := "media"]
alldat3[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(spid)]
alldat3[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]

# for cytotox endpoints
alldat3[spid == "Tritonx100" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "p"]
alldat3[spid == "Media" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "media"]
alldat3[spid == "Picrotoxin" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "z"] # same for BIC
alldat3[spid == "Tetrodotoxin" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "ttx"]
alldat3[spid == "DMSO" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "n"]
alldat3[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(spid)]
alldat3[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]
alldat3[,unique(wllt)]
# [1] "media" "t"     "n"     "x"     "p"     "z"     "m"     "ttx" 


# CHECK CONC'S (conc's for controls can just make those follow the treatments)

unique(alldat3$conc) # any NA's? any non-numeric? Any 0? does it look like conc correction was done?
alldat3[is.na(conc),unique(spid)] # none
alldat3[spid == "DMSO",unique(conc)]
# [1] "Control"
# dmso - I'm just gonna gues 0.002, as in Toxcast compounds. bc it doesn't really matter
alldat3[spid == "DMSO", conc := "0.002"]

# picro
alldat3[spid == "Picrotoxin", .N, by = "conc"]
# conc   N
# conc   N
# 1:      25 699
# 2: Control 204
# based on previous experiments, this should always be 25
alldat3[spid == "Picrotoxin", conc := "25"]

# ttx
alldat3[spid == "Tetrodotoxin", .N, by = "conc"]
# conc   N
# 1:       1 699
# 2: Control  24
# based on previous experiments, this should always be 1
alldat3[spid == "Tetrodotoxin", conc := "1"]

# media
alldat3[spid == "Media", .N, by = "conc"]
# conc   N
# 1: Control 270
# I am pretty sure Media should always be 10
alldat3[spid == "Media", conc := "10"]


# lysis
alldat3[spid == "Tritonx100", .N, by = "conc"]
# conc   N
# 1:       1 699
# 2: Control  12
# pretty sure this should always be 10
alldat3[spid == "Tritonx100", conc := "10"]

# skipping for now
# # get acid, via invitrodb
# tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
# shafer.assays <- tcplLoadAcid(fld = "asid",val=20,add.fld = "acsn")
# mea.acute <- shafer.assays[grepl("MEA_acute",acnm)]
# alldat3 <- merge(alldat3, mea.acute[, .(acsn,acid)], by = "acsn")


# check that all data is there, nothing is missing
check.points <- dcast(alldat3[, .N, by = c("acsn","apid")], apid ~ acsn, value.var = "N")
setnames(check.points, old = names(check.points), new = as.character(1:length(names(check.points))))
check.points
# 48 plates, 17 endpoints. 48 values for each plate and acsn except for 20190402_MW1236-24 - only has LDH/AB endpoints, as excepted
unique(alldat3$acsn)
# all 17 are there
unique(alldat3$wllq) # any NA?
# [1] 1 0
length(unique(alldat3$apid)) # correct number of plates?
# 48 plates, 48*2 = 96 files - 2 files excluded for 20190402_MW1236-24 = 94 files originally selected
unique(alldat3$conc)
# any NA rval where wllq == 1?

# visualizations - ? future probably

# save the mc0 data
assign(paste0(dataset_title,"_alldat4"), value = alldat3[, .(spid, apid, rowi, coli, conc, acsn, wllt, wllq, srcf, rval)])
save(APCRA2019_alldat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_alldat4_",as.character.Date(Sys.Date()),".RData")))

