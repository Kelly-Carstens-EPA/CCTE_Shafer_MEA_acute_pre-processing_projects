###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/" # the main directory of the source files in data set
dataset_title <- "TEMPLATE2020"
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
# 
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title, check.settings = T, check.parameters = T, check.timing = T, threshold = threshold), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (alldat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, append = F, threshold = threshold,
               check.settings = T, check.parameters = T, check.timing = T)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# collapse the plate data by calculating the percent change in activity (alldat2)
collapsePlateData(main.output.dir)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# look at data so far
load(file = paste0("output/",dataset_title,"_alldat2_",as.character.Date(Sys.Date()),".RData"))
alldat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 
rm(alldat2)

# get cytotox data
cytodat <- getCytoData(main.output.dir, dataset_title)
str(cytodat)

# combine the cytodat with alldat2, add trt, conc, and wllq to ea (alldat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# load alldat3 and finalize it
load(file = paste0("output/",dataset_title,"_alldat3_",as.character.Date(Sys.Date()),".RData"))
rm(cytodat)

# FINALIZE WLLQ

# set wllq to zero where rval is NA
alldat3[wllq==1 & is.na(rval),.N] # 208
alldat3[is.na(rval), wllq := 0]

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.


# ASSIGN SPIDS

library(xlsx)
spidmap <- read.xlsx("")
colnames(spidmap)[colnames(spidmap) == "EPA_SAMPLE_ID"] <- "spid"
colnames(spidmap)[colnames(spidmap) == "PREFERRED_NAME"] <- "treatment"
alldat3 <- merge(x = alldat3, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# ensure that the only non-spid values are: "Tritonx100" "Bicuculline"  "DMSO" 
alldat[is.na(spid),unique(treatment)]
# example fixes:
# alldat3[spid == "LYSIS", spid := "Tritonx100"] 
# alldat3[spid == "BC" | spid == "BIC", spid := "Bicuculline"]
unique(alldat3$spid)


# ASSIGN WLLT

alldat3[, wllt := "t"]
alldat3[spid == "DMSO", wllt := "n"]
alldat3[spid == "Tritonx100" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "p"]
alldat3[spid == "Tritonx100" & !(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), wllt := "x"]
alldat3[spid == "Bicuculline" & !(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), wllt := "p"]
alldat3[spid == "Bicuculline" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "z"]


# CHECK CONC'S

unique(alldat3$conc) # any NA's? does it look like conc correction was done?
alldat3[is.na(conc),unique(spid)] # all non-t wells, "DMSO"        "Bicuculline" "Tritonx100" ?
alldat3[spid %in% c("DMSO", "Bicuculline", "Tritonx100"), .(unique(conc)), by = "spid"]
# assign conc's to look like this:
# spid      V1
# 1: Bicuculline  25.000
# 2:        DMSO   0.002
# 3:  Tritonx100 100.000


# get acid, via invitrodb
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
shafer.assays <- tcplLoadAcid(fld = "asid",val=20,add.fld = "acsn")
mea.acute <- shafer.assays[grepl("MEA_acute",acnm)]
alldat3 <- merge(alldat3, mea.acute[, .(acsn,acid)], by = "acsn")


# check that all data is there, nothing is missing
check.points <- dcast(alldat3[, .N, by = c("acid","apid")], apid ~ acid, value.var = "N")
# view check.points. There should be exactly 48 points ea
unique(alldat3$acsn)
unique(alldat3$wllq) # any NA?
length(unique(alldat3$apid)) # correct number of plates?
unique(alldat3$conc)


# visualizations - ? future probably

# save the mc0 data
assign(paste0(dataset_title,"_alldat4"), value = alldat3[, .(spid, apid, rowi, coli, conc, acid, wllt, wllq, srcf, rval)])
save(alldat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_alldat4_",as.character.Date(Sys.Date()),".RData")))
