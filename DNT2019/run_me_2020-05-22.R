###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA"
dataset_title <- "DNT2019"
setwd(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/",dataset_title)) # where you want output to go
spidmap_file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
use_sheet <- "Chemical List"
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
threshold <- 1*60 # how many seconds off of 40 minutes should signal a flag?
parameter_set_type <- "post_july_2016" # use "pre_july_2016" if your endpoints include 'Half Width at Half Height of Cross Correlation' instead of 'Width...'
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
# Reading from DNT2019_neural_stats_files_log_2020-05-18.txt...
# Got 78 files.
# All files are named correctly.
# Empty data.table (0 rows and 2 cols): filenames,run.type.tags
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title, check.settings = T, check.parameters = T, check.timing = T, threshold = threshold), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# DNT2019_check_summary_2020-05-19.txt is ready.
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (alldat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, append = F, threshold = threshold,
               check.settings = F, check.parameters = F, check.timing = F)
# OUTPUT --------------------------------------------------------- 
# Reading from DNT2019_neural_stats_files_log_2020-05-18.txt...
# Got 78 files.
# Reading data from files...
# Processed TC_20190508_MW68-0808_13_00(000).csv
# Processed TC_20190508_MW68-0808_13_01(000).csv
# Processed TC_20190508_MW68-0811_13_00(000).csv
# Processed TC_20190508_MW68-0811_13_01(000).csv
# ...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_alldat1_2020-05-19.RData is ready.
# apid                                         wllq_set_to_zero
# 1: 20190530_MW68-0807                                                 C4,D4,D5
# 2: 20190530_MW68-0809                                                       D6
# 3: 20190618_MW69-0106                                  A5,B7,C1,C3,C8,D5,F2,F7
# 4: 20190618_MW69-0107                   A1,A2,B2,B3,B7,C2,C7,D3,D5,E7,F2,F3,F4
# 5: 20190618_MW69-0108                   A7,A8,B4,C5,C6,C8,D1,D3,D5,D6,D7,E2,E3
# 6: 20190827_MW69-3808                                                    E2,F4
# 7: 20190829_MW69-3810                                                       C3
# 8: 20191008_MW70-2407 B2,B3,B4,B5,C4,C5,C7,D2,D3,D4,D5,D6,D8,E3,E5,F3,F5,F7,F8
# 9: 20191008_MW70-2408          B4,B5,B6,C2,C4,C5,C6,C7,D2,D3,D4,D5,D7,E4,E5,F7
# 10: 20191008_MW70-2409             B3,B5,B6,C2,C4,C5,C6,C7,C8,D3,D4,D5,D7,E7,F6
# ---------------------------------------------------------------- 

# collapse the plate data by calculating the percent change in activity (alldat2)
collapsePlateData(main.output.dir)
# OUTPUT --------------------------------------------------------- 
# Loading L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_alldat1_2020-05-19.RData ...
# Collapsing treated and baseline data...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_alldat2_2020-05-20.RData is ready.
# ---------------------------------------------------------------- 

# look at data so far
load(file = paste0("output/",dataset_title,"_alldat2_",as.character.Date(Sys.Date()),".RData"))
alldat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
# Min.    1st Qu.     Median     Mean  3rd Qu.     Max.     NA's 
# -100.000  -34.334   -4.515   -9.783    5.927 1999.582     1687 
# ---------------------------------------------------------------- 
alldat2[is.na(rval), .N, by = "wllq"]
# wllq    N
# 1:    1 1687
# 2:    0   71
rm(alldat2)

# get cytotox data
cytodat <- getCytoData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# Reading from DNT2019_calculations_files_log_2020-05-20.txt...
# Got 13 files.
# Reading data from files...
# 
# 20190515_Calculations_DNT Group_1-DONE_row_adjustments.xlsxNew names:
#   * `` -> ...1
# * `` -> ...2
# * `` -> ...3
# ...
# ---------------------------------------------------------------- 
str(cytodat)
any(is.na(cytodat)) # FALSE

# combine the cytodat with alldat2, add trt, conc, and wllq to ea (alldat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/output/DNT2019_alldat3_2020-05-20.RData is ready.
# ---------------------------------------------------------------- 
rm(cytodat)

# load alldat3 and finalize it
# load(file = paste0("output/",dataset_title,"_alldat3_",as.character.Date(Sys.Date()),".RData"))
load(file = paste0("output/",dataset_title,"_alldat3_2020-05-20.RData"))
str(alldat3)

# FINALIZE WLLQ

# set wllq to zero where rval is NA
alldat3[wllq==1 & is.na(rval),.N] 
# [1] 1687
alldat3[is.na(rval), wllq := 0]

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.
# The following wells were contaminated, based on teh lab notebook
alldat3[apid == "20190530_MW68-0807" & rowi==3 & coli==6, wllq:=0] # rval's were all NA already
alldat3[apid == "20190530_MW68-0807" & rowi==6 & coli==1, wllq := 0] # rval's were all NA already
alldat3[apid == "20190530_MW68-0809" & rowi==4 & coli==7, wllq:=0] # rval's were all NA already
alldat3[apid == "20190530_MW68-0809" & rowi==5 & coli==1, wllq:=0] # rval's were all NA already
alldat3[apid == "20190530_MW68-0810" & rowi==2 & coli==5, wllq:=0] # rval's were all NA already
# TTX wells were lysed post recording, for total LDH -? in all plates, or the the one where F1 was contaminated?
# will confirm when I look at the AB plots below

# the following wells did not receive teh full dose
alldat3[apid == "20190730_MW68-0817" & rowi==5 & coli==5, wllq:=0]
# verification for next well that this might be off
plot(alldat3[grepl("20190730",apid) & treatment == "69" & grepl("firing_rate",acsn), .(log(as.numeric(conc)), rval)])
points(alldat3[apid == "20190730_MW68-0817" & rowi==6 & coli==5 & grepl("firing_rate",acsn), .(log(as.numeric(conc)), rval)], col = "blue", pch=19)
alldat3[apid == "20190730_MW68-0817" & rowi==6 & coli==5, wllq := 0]


# ASSIGN SPIDS

spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = "NCCT ID...2", new = "spid")
setnames(spidmap, old = "Chemical ID...1", new = "treatment")
spidmap$treatment <- as.character(spidmap$treatment)
setdiff(unique(alldat3$treatment), unique(spidmap$treatment))
# [1] "TTX"   "PICRO" "DMSOa" "DMSOb" "DMSOc" "Lysis" "Media" "0" 
alldat3[treatment == "0", unique(apid)]
# [1] "20191008_MW70-2407" "20191008_MW70-2408" "20191008_MW70-2409"
alldat3[grepl("20191008",apid), unique(treatment)]
# [1] "DMSOa" "69"    "DMSOb" "75"    "DMSOc" "80"    "TTX"   "83"    "PICRO" "95"    "Media" "0"  
# from Lab Notebook and Calculations files, only 5 compounds were tested in this culture
# I asked Kathleen, and she said that the empty row is Media
alldat3 <- merge(x = alldat3, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
alldat3[is.na(spid),unique(treatment)]
# [1] "0"     "DMSOa" "DMSOb" "DMSOc" "Lysis" "Media" "PICRO" "TTX"  
alldat3[treatment == "0", spid := "Media"]
alldat3[grepl("DMSO",treatment), spid := "DMSO"]
alldat3[treatment == "Media", spid := "Media"]
alldat3[treatment == "PICRO", spid := "Picrotoxin"]
alldat3[treatment == "TTX", spid := "Tetrodotoxin"]
alldat3[treatment == "Lysis", spid := "Tritonx100"]
unique(alldat3$spid)
# visually confirm if the PICRO, TTX, LYSIS were added before the second treatment
# varies across experiments, sometimes across days
# if not, these wells only contained media for the MEA recordings
boxplot(rval ~ spid, alldat3[spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
title(main = paste0("Percent Change in Mean Firing rate\nin Control wells of ",dataset_title," MEA acute Experiments"))
# RESPONSE:
# yes, it appears that the PICRO, TTX, LYSIS were added before the second treatment

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Visually confirm if correct
stripchart(rval ~ spid, alldat3[spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("(AB)",acsn) & treatment != "0"],
           vertical = T, pch = 1, method = "jitter", main = "CellTiter Blue Percent of Control Values for Control Compounds")
# for AB, only 2 instances of Media are significantly greater than 0
alldat3[treatment == "Media" & grepl("AB",acsn), .(num_high = sum(rval>0.5), num_low = sum(rval<0.5)), by = c("apid","srcf")]
# the only place with 2 plates where Media appears to be higher:
# 20190717_Calculations_DNT Group_5 Repeata.xlsx , exp date 20190730
# note in this file:  **Controls are rotating but the same F1 well is lysed across 3 plates
# okay, so in every group except for 20190717_Calculations_DNT Group_5, I think it is safe to assume that "Media" wells were lysed
alldat3[!grepl("20190730",apid) & treatment == "Media" & grepl("AB",acsn), `:=`(treatment = "Lysis", spid = "Tritonx100")]
# for the F1 wells in 20190730
alldat3[grepl("20190730",apid) & grepl("AB",acsn) & rowi == 6 & coli==1, .(apid, treatment, rval, conc)]
# apid treatment       rval conc
# 1: 20190730_MW68-0814     Media 0.03348864   10
# 2: 20190730_MW68-0817     PICRO 0.01674564   10
# 3: 20190730_MW68-0818       TTX 0.02003046   10
alldat3[grepl("20190730",apid) & grepl("AB",acsn) & rowi == 6 & coli==1, `:=`(treatment = paste0(treatment,"/Lysis"), spid = "Tritonx100")]
# other wells in these plates may be labelled "Media". I believe that these really are just Media, based on the note, even in AB assay
# Are teh 3 low wells of TTX from the culture where TTX was lysed, because there was a contamination in well F1? (20190530)
alldat3[treatment == "TTX" & grepl("AB",acsn) & rval < 0.5] # 2 of the three are
# I will renamed these 2 to reflect the fact that Lysis was added
alldat3[treatment == "TTX" & grepl("AB",acsn) & apid == "20190530_MW68-0807" & rowi == 4 & coli==1, `:=`(treatment = paste0(treatment,"/Lysis"), spid = "Tritonx100")]
alldat3[treatment == "TTX" & grepl("AB",acsn) & apid == "20190530_MW68-0809" & rowi == 4 & coli==1, `:=`(treatment = paste0(treatment,"/Lysis"), spid = "Tritonx100")]


# determine if Media is Lysis for LDH assay
stripchart(rval ~ spid, alldat3[spid %in% c("DMSO","Picrotoxin","Tetrodotoxin","Media","Tritonx100") & grepl("(LDH)",acsn) & treatment != "0"],
           vertical = T, pch = 1, method = "jitter", main = "LDH Percent of Control Values for Control Compounds")
# clearly, the well labelled "Media" looks a lot like teh DMSO and other controls for LDH. Except for 1 well
alldat3[spid == "Media" & rval > 0.8 & grepl("LDH",acsn)]
# not sure what happened here. looking at the calc file, this is right. Maybe Lysis was accidentally added to this well
# I am not going to stress about it any more. It will be okay.
# leaving these wells labelled as "Media" for LDH acsn


# MAKE SURE THAT CYTOTOX VALUES ARE ON SCALE OF 0-100 (versus 0-1)
alldat3[grepl("LDH",acsn), range(rval)]
# [1] -0.1637042  0.9972912
alldat3[grepl("LDH",acsn), rval := rval*100]
alldat3[grepl("AB", acsn), range(rval)]
# [1] 0.00000 1.49744
alldat3[grepl("AB", acsn), rval := rval*100]

# MAKE AB VALUES %DEAD
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
alldat3[conc == "0",unique(spid)] # includes some tested spids
# # based on the calc file, I think the values just did not get entered. Will add now based on col's
# alldat3[conc =="0" & grepl("20190618",apid),unique(spid)] # all tested spids, no controls
# alldat3[conc =="0" & grepl("20190618",apid) & coli == "2", conc:="0.03"]
# alldat3[conc =="0" & grepl("20190618",apid) & coli == "3", conc:="0.1"]
# alldat3[conc =="0" & grepl("20190618",apid) & coli == "4", conc:="0.3"]
# alldat3[conc =="0" & grepl("20190618",apid) & coli == "5", conc:="1.0"]
# alldat3[conc =="0" & grepl("20190618",apid) & coli == "6", conc:="3"]
# alldat3[conc =="0" & grepl("20190618",apid) & coli == "7", conc:="10"]
# alldat3[conc =="0" & grepl("20190618",apid) & coli == "8", conc:="30"]
# 
# # wait a sec... this means that no conc correction was done!
# # I am still just going to disclude this data for now



alldat3[spid == "DMSO",unique(conc)]
# [1] "Control"
alldat3[conc == "0", unique(apid)]
alldat3[is.na(conc),unique(spid)] # none

# picro
alldat3[spid == "Picrotoxin", .N, by = "conc"]
# conc   N
# 1:   25 594
# 2:   10  51
# 3:    1  17
# based on lab notebook, this should always be 25
alldat3[spid == "Picrotoxin", conc := "25"]

# ttx
alldat3[spid == "Tetrodotoxin", .N, by = "conc"]
# conc   N
# 1:    1 628
# 2:   10  17
# 3:   25  17
# based on lab notebook, this should always be 1
alldat3[spid == "Tetrodotoxin", conc := "1"]

# media
alldat3[spid == "Media", .N, by = "conc"]
# conc   N
# 1:    0 357
# 2:   10 514
# 3:   25  48
# 4:    1  16
alldat3[spid == "Media" & grepl("20191008",apid), .N, by = "conc"]
# conc   N # this is plate with entire row of Media
# 1:    0 357
# 2:   10  48
# I am pretty sure Media should always be 10
alldat3[spid == "Media", conc := "10"]

# dmso - I'm just gonna gues 0.002, as in Toxcast compounds. bc it doesn't really matter
alldat3[spid == "DMSO", conc := "0.002"]

# lysis
alldat3[spid == "Tritonx100", .N, by = "conc"]
# conc  N
# 1:   10 81
# 2:   25  4
# 3:    1  2
# pretty sure this should always be 10
alldat3[spid == "Tritonx100", conc := "10"]

sort(as.numeric(unique(alldat3$conc)))
alldat3[conc == 0.0, .N, by = c("srcf","spid")] # all from 20190605_Calculations_DNT Group_5.xlsx
# lookign at this file, there are no conc's labelled at all. 
# was this essentially a single pont screen?
# do I see any kind of dose-response?
stripchart(rval ~ coli, alldat3[grepl("20190618",apid) & grepl("firing_rate",acsn)], vertical=T, method = "jitter", pch=1, 
           xlab = "column", ylab = "Percent Change in Mean Firing Rate", main = "Checking for dose-response in experiment 20190618")
# it really looks like the concentrations just followed a linear scale
# I am just going to leave these compounds with conc==0 for now, until I can confirm


# holding off on this, until I get NHEERL_MEA_acute_cross_correlation_WHM registered
# # get acid, via invitrodb
# tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
# shafer.assays <- tcplLoadAcid(fld = "asid",val=20,add.fld = "acsn")
# mea.acute <- shafer.assays[grepl("MEA_acute",acnm)]
# alldat3 <- merge(alldat3, mea.acute[, .(acsn,acid)], by = "acsn")


# check that all data is there, nothing is missing
check.points <- dcast(alldat3[, .N, by = c("acsn","apid")], apid ~ acsn, value.var = "N")
setnames(check.points, old = names(check.points), new = as.character(1:length(names(check.points))))
check.points
# 39 plates, 17 endpoints, 48 values each
unique(alldat3$acsn)
# all 17 are there
unique(alldat3$wllq) # any NA?
# [1] 1 0
length(unique(alldat3$apid)) # correct number of plates?
# 39 plates, 29*2 = 78 files originally selected
unique(alldat3$conc)


# visualizations - ? future probably

# save the mc0 data
assign(paste0(dataset_title,"_alldat4"), value = alldat3[, .(spid, apid, rowi, coli, conc, acsn, wllt, wllq, srcf, rval)])
save(DNT2019_alldat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_alldat4_",as.character.Date(Sys.Date()),".RData")))

# # May 20, 2020
# load(file = "output/DNT2019_alldat3_2020-05-19.RData")
# str(alldat3)
# unique(alldat3$acsn)
# alldat3[treatment == "Lysis"] # rval's are all NA, -100%, or nearly 0% survival
# alldat3[treatment == "Media", .(min_rval = min(rval,na.rm=T), med_rval = median(rval,na.rm=T), max_rval = max(rval,na.rm=T), num_na = sum(is.na(rval))), by = "acsn"]
# alldat3[treatment == "Lysis", .(min_rval = min(rval,na.rm=T), max_rval = max(rval,na.rm=T), num_na = sum(is.na(rval))), by = "acsn"]
# alldat3[treatment == "Media", .N, by = c("rowi","coli")]
# # did I mess somethign up here? I think only well F1 was supposed to be Media/Lysis
# # did I read from the calculations files incorrectly?
# alldat3[treatment == "Media" & rowi == 5 & coli ==1, unique(srcf)]
# # in "20190515_Calculations_DNT Group_2.xlsx" , Media was indeed move to well A5 in plate 2. I can believe that. Conc of 10 matches this as well
# # in "20190717_Calculations_DNT Group_5 Repeata.xlsx" - dosing plate does say that the media was moved around, but the conc's don't match up. Will ask K
# alldat3[treatment == "Media" & rowi == 4 & coli ==1, unique(srcf)] # same Group 5 repeat as above
# 
# # confirm that the differences in conc are from these same files
# alldat3[treatment == "Media" & conc != 10, unique(srcf)]
# # [7] "20190529_Calculations_DNT Group_3a.xlsx"                                                      
# # [8] "20190529_Calculations_DNT Group_4.xlsx"                                                       
# # [9] "20190717_Calculations_DNT Group_5 Repeata.xlsx"                                               
# # [10] "20190814_Calculations_DNT Group_1 Repeat.xlsx" 
# 
# # for TTX and PICRO, can i believe that these are just media during the recordings?
# alldat3[grepl("DMSO",treatment), .(min_rval = min(rval,na.rm=T), med_rval = median(rval,na.rm=T), max_rval = max(rval,na.rm=T), num_na = sum(is.na(rval))), by = "acsn"]
# alldat3[treatment == "PICRO", .(min_rval = min(rval,na.rm=T), med_rval = median(rval,na.rm=T), max_rval = max(rval,na.rm=T), num_na = sum(is.na(rval))), by = "acsn"]
# # hmm... median firing rate is 129% increase... but big range. I could believe that this is just media
# alldat3[treatment == "TTX", .(min_rval = min(rval,na.rm=T), med_rval = median(rval,na.rm=T), max_rval = max(rval,na.rm=T), num_na = sum(is.na(rval))), by = "acsn"]
# # hmm... median firing rate is -100% (100% decrease...). But max is 132% increase
# yrange <- alldat3[grepl("firing_rate",acsn), range(rval,na.rm=T)]
# plot(alldat3[grepl("DMSO",treatment) &  acsn == "NHEERL_MEA_acute_firing_rate_mean", rval], pch = 19, col = "black", ylim = yrange)
# points(alldat3[treatment == "PICRO" &  acsn == "NHEERL_MEA_acute_firing_rate_mean", rval], pch = 19, col = "blue")
# points(alldat3[treatment == "TTX" &  acsn == "NHEERL_MEA_acute_firing_rate_mean", rval], pch = 19, col = "red")
# 
# alldat3[grepl("DMSO",treatment), treatment := "DMSO"]
# boxplot(rval ~ treatment, alldat3[treatment %in% c("DMSO","PICRO","TTX","Media","Lysis") & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
# title(main = "Percent Change in Mean Firing rate\nin Control wells of DNT MEA acute Experiments")

# compounds tested in Group 2, which was to be repeated: 20190530
alldat3[grepl("20190530",apid), unique(treatment)]
# [1] "DMSOa" "17"    "DMSOb" "18"    "DMSOc" "19"    "TTX"   "20"    "PICRO" "24"    "Media" "27"   
# which of these were tested elsewhere?
alldat3[treatment %in% c(17,18,19,20,24,27), unique(apid)]
# [1] "20190530_MW68-0807" "20190530_MW68-0809" "20190530_MW68-0810" "20190829_MW69-3810" "20190829_MW69-3811" "20190829_MW69-3812"
alldat3[treatment %in% c(17,18,19,20,24,27), unique(apid), by = "spid"] # so all treatments were tested in both cultures.

plot(alldat3[spid == "EPAPLT0167D05" & grepl("firing_rate",acsn) & grepl("20190530",apid), .(log(as.numeric(conc)), rval)], 
     col = "red",pch=19, ylim = c(-120, 120), ylab = "Percent Change in Mean Firing Rate", xlab = "log(conc)", cex = 1.5)
points(alldat3[spid == "EPAPLT0167D05" & grepl("firing_rate",acsn) & grepl("20190829",apid), .(log(as.numeric(conc)), rval)], 
       col = "black",pch=19, cex = 1.5)
title(main = "Mean Firing Rate Rvals of EPAPLT0167D05 (Boscalid) in 2 cultures")
legend(x = "topright", legend = c("20190530","20190829"), pch = c(19,19), col = c("red","black"), title = "Culture")

# was the stuff in Group 1 repeated as well?
alldat3[grepl("20190528",apid), unique(treatment)]
# [1] "TTX"   "3"     "PICRO" "5"     "DMSOa" "8"     "DMSOb" "9"     "DMSOc" "11"    "Lysis" "12" 
alldat3[treatment %in% c(3,5,8,9,11,12), unique(spid)]
alldat3[treatment %in% c(3,5,8,9,11,12), unique(apid), by = "spid"] # only 2 were repeated

# 83 and 95 "were too variable"
alldat3[treatment %in% c(83, 95), unique(spid)] # [1] "EPAPLT0167H07" "EPAPLT0169A03"

spidi = "EPAPLT0167F02"
cultures <- unique(sapply(alldat3[spid == spidi, unique(apid)], function(x) strsplit(x, "_")[[1]][1]))
plot(alldat3[spid == spidi & grepl("firing_rate",acsn) & grepl(cultures[1],apid) & wllq == 1, .(log10(as.numeric(conc)), rval)], 
     col = "red",pch=19, ylim = c(-120, 120), ylab = "Percent Change in Mean Firing Rate", xlab = "log10(conc)", cex = 1.5)
points(alldat3[spid == spidi & grepl("firing_rate",acsn) & grepl(cultures[2],apid) & wllq == 1, .(log10(as.numeric(conc)), rval)], 
       col = "black",pch=19, cex = 1.25)
title(main = paste0("Mean Firing Rate Rvals of ",spid," in 2 cultures"))
legend(x = "topright", legend = cultures, pch = c(19,19), col = c("red","black"), title = "Culture")

# should we include Group 5 20190605 culture at all?
alldat3[grepl("20190605",apid)]
# empty... what? - oh right, this is the culture, not the experiment date
alldat3[grepl("MW69-0106",apid), unique(apid)] # "20190618_MW69-0106"

# were these chem tested elsewhere?
alldat3[grepl("MW69-0106",apid), unique(spid)]
# [1] "EPAPLT0169C05" "EPAPLT0167E10" "EPAPLT0167A02" "EPAPLT0167A08" "EPAPLT0167A04" "EPAPLT0167D09" "DMSO"          "Media"        
# [9] "Picrotoxin"    "Tetrodotoxin" 
alldat3[spid %in% c("EPAPLT0169C05", "EPAPLT0167E10", "EPAPLT0167A02", "EPAPLT0167A08" ,"EPAPLT0167A04", "EPAPLT0167D09"),
        unique(apid), by = "treatment"]
# yes, these were re-tested.

alldat3[grepl("firing_rate",acsn) & grepl("20190618",apid) & wllq == 1, .N, by = "spid"]
